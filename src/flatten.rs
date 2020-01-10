use std::collections::BTreeMap;
use std::iter::FromIterator;

use crate::compat::BitvSet;
use crate::graph::{BlockId, Graph};
use crate::{Kind, RegClass, Register};

pub(crate) trait Flatten {
    /// Perform flattening itself.
    fn flatten(&mut self);
}

trait FlattenHelper {
    /// Flatten CFG and detect/enumerate loops.
    ///
    /// Get map: loop_start => [ loop ends ]
    fn flatten_get_ends(&mut self) -> BTreeMap<BlockId, Vec<BlockId>>;

    /// Assign loop_index/loop_depth to each block.
    fn flatten_assign_indexes(&mut self);

    // Assign new ids to blocks and instructions
    fn flatten_reindex_blocks(&mut self, list: &[BlockId]) -> Vec<BlockId>;
    fn flatten_reindex_instructions(&mut self, list: &[BlockId]);
}

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>> FlattenHelper
    for Graph<K, G, R>
{
    fn flatten_get_ends(&mut self) -> BTreeMap<BlockId, Vec<BlockId>> {
        let mut queue = vec![self.state.root];
        let mut visited = BitvSet::new();
        let mut ends: BTreeMap<BlockId, Vec<BlockId>> = BTreeMap::new();

        // Visit each block and find loop ends.
        while queue.len() > 0 {
            let cur = queue.remove(0);

            // Don't visit the same node several times.
            if !visited.insert(cur.to_uint()) {
                continue;
            }

            for succ in self.get_block(&cur).successors.iter() {
                if visited.contains(succ.to_uint()) {
                    // Loop detected!
                    //
                    // XXX Seems to be incorrect??
                    // 1 -> 2 -> 7 <-------\
                    //  \                  |
                    //   \    /---------|  |
                    //    \   |         |  |
                    //     -> 3 -> 4 \  |  |
                    //         \      6-----
                    //          -> 5 /
                    //
                    // 7 will be detected as a loop header.
                    ends.entry(*succ).or_default().push(cur);
                } else {
                    queue.push(*succ);
                }
            }
        }

        ends
    }

    fn flatten_assign_indexes(&mut self) {
        let ends = self.flatten_get_ends();

        let mut loop_index = 1;

        for (&start_id, ends) in ends.iter() {
            // Decrement number of incoming forward branches.
            assert!(self.get_block(&start_id).incoming_forward_branches == 2);
            self.get_mut_block(&start_id).incoming_forward_branches -= 1;

            // Fill the queue with all the backedge origins.
            let mut queue = Vec::from_iter(ends.iter().cloned());
            let expected_depth = self.get_block(&start_id).loop_depth;

            let mut visited = BitvSet::new();

            while queue.len() > 0 {
                let cur = queue.remove(0);
                let block = self.get_mut_block(&cur);

                // Skip visited blocks.
                if !visited.insert(cur.to_uint()) {
                    continue;
                }

                // Set depth and index of not-visited-yet nodes, if we're not visiting nested loop.
                if block.loop_depth == expected_depth {
                    block.loop_index = loop_index;
                    block.loop_depth += 1;
                }

                // Enqueue predecessors if current is not a loop start.
                if cur != start_id {
                    for pred in block.predecessors.iter() {
                        queue.push(*pred);
                    }
                }
            }

            // Increment loop index
            loop_index += 1;
        }
    }

    fn flatten_reindex_blocks(&mut self, list: &[BlockId]) -> Vec<BlockId> {
        let mut block_id = 0;
        let mut queue = vec![];
        let mut result = vec![];
        let mut mapping = BTreeMap::new();

        for id in list.iter() {
            let mut block = self.fields.blocks.remove(&id).expect("block");

            // Update root id
            if block.id == self.state.root {
                self.state.root = BlockId(block_id);
            }

            mapping.insert(block.id, BlockId(block_id));
            block.id = BlockId(block_id);
            block_id += 1;

            // Update block id in it's instructions
            for instr_id in block.instructions.iter() {
                self.get_mut_instr(instr_id).block = block.id;
            }

            result.push(block.id);
            queue.push(block);
        }

        // Remove all other instructions
        self.fields.blocks.clear();

        // Insert them again
        while let Some(mut block) = queue.pop() {
            block.successors = block
                .successors
                .iter()
                .map(|succ| *mapping.get(&succ).expect("successor"))
                .collect();
            block.predecessors = block
                .predecessors
                .iter()
                .map(|pred| *mapping.get(&pred).expect("predecessor"))
                .collect();
            self.fields.blocks.insert(block.id, block);
        }

        result
    }

    fn flatten_reindex_instructions(&mut self, list: &[BlockId]) {
        self.fields.instr_id = 0;
        let mut queue = vec![];
        let mut map = BTreeMap::new();

        // Go through blocks and map instructions
        for &block in list.iter() {
            let list = self.get_block(&block).instructions.clone();
            let mut new_list = vec![];
            let start_gap = self.create_gap(block);
            new_list.push(start_gap.id);
            queue.push(start_gap);

            for (i, id) in list.iter().enumerate() {
                // Pop each instruction from map
                let mut instr = self.fields.instructions.remove(&id).unwrap();

                // Insert mapping
                let id = self.next_instr_id();
                map.insert(instr.id, id);

                // And update its id
                instr.id = id;

                // Construct new block instructions list and insert instruction into
                // new map
                new_list.push(instr.id);
                queue.push(instr);

                // Insert gap
                if i != list.len() - 1 {
                    let gap = self.create_gap(block);
                    new_list.push(gap.id);
                    queue.push(gap);
                }
            }
            if list.len() != 0 {
                let end_gap = self.create_gap(block);
                new_list.push(end_gap.id);
                queue.push(end_gap);
            }

            // Replace block's instruction list
            self.get_mut_block(&block).instructions = new_list;
        }

        // Add phis to queue
        let mut i = 0;
        while i < self.fields.phis.len() {
            let mut phi = self
                .fields
                .instructions
                .remove(&self.fields.phis[i])
                .expect("Phi");

            // Insert mapping
            let id = self.next_instr_id();
            map.insert(phi.id, id);

            // Update id
            phi.id = id;

            // Queue phi
            queue.push(phi);
            i += 1;
        }

        // Remove all other instructions
        self.fields.instructions.clear();

        // Replace graph's instruction map
        while queue.len() > 0 {
            let mut instr = queue.pop().unwrap();

            // Update inputs
            instr.inputs = instr
                .inputs
                .iter()
                .map(|i| match map.get(&i) {
                    Some(r) => *r,
                    None => *i,
                })
                .collect();

            self.fields.instructions.insert(instr.id, instr);
        }
    }
}

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>> Flatten
    for Graph<K, G, R>
{
    fn flatten(&mut self) {
        self.flatten_assign_indexes();

        let mut queue = vec![self.state.root];
        let mut list = vec![];
        let mut visited = BitvSet::new();

        // Visit each block and its successors
        while queue.len() > 0 {
            let cur = queue.remove(0);

            // Skip visited blocks
            if !visited.insert(cur.to_uint()) {
                continue;
            }

            list.push(cur);

            // Visit successors if they've no unvisited incoming forward edges
            let successors = self.get_block(&cur).successors.clone();
            for succ_id in successors.iter() {
                let succ = self.get_mut_block(succ_id);
                if succ.incoming_forward_branches == 0 {
                    continue;
                }

                succ.incoming_forward_branches -= 1;
                if succ.incoming_forward_branches == 0 {
                    queue.insert(0, *succ_id);
                }
            }
        }

        // Assign flat ids to every block
        list = self.flatten_reindex_blocks(&list);

        // Assign flat ids to every instruction
        self.flatten_reindex_instructions(&list);
    }
}
