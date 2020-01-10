use crate::compat::BitvSet;
use crate::graph::{BlockId, Graph};
use crate::{Kind, RegClass, Register};

pub(crate) trait Liveness {
    fn liveness_analysis(&mut self);
}

trait LivenessHelper {
    // Build live_gen, live_kill
    fn build_local(&mut self, blocks: &[BlockId]);

    // Build live_in, live_out
    fn build_global(&mut self, blocks: &[BlockId]);
}

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>> Liveness
    for Graph<K, G, R>
{
    fn liveness_analysis(&mut self) {
        let blocks = self.get_block_list();
        self.build_local(&blocks);
        self.build_global(&blocks);
    }
}

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>> LivenessHelper
    for Graph<K, G, R>
{
    fn build_local(&mut self, blocks: &[BlockId]) {
        for block in blocks.iter() {
            let instructions = self.get_block(block).instructions.clone();

            for instr in instructions.iter() {
                let output = self.get_instr(instr).output;
                let inputs = self.get_instr(instr).inputs.clone();

                match output {
                    Some(output) => self.get_mut_block(block).live_kill.insert(output.to_uint()),
                    None => true,
                };

                for input_instr in inputs.iter() {
                    let input = self.get_output(input_instr);
                    if !self.get_block(block).live_kill.contains(input.to_uint()) {
                        self.get_mut_block(block).live_gen.insert(input.to_uint());
                    }
                }
            }
        }
    }

    fn build_global(&mut self, blocks: &[BlockId]) {
        let mut change = true;
        while change {
            change = false;

            for block in blocks.iter().rev() {
                let successors = self.get_block(block).successors.clone();

                let mut tmp = BitvSet::new();
                for succ in successors.iter() {
                    tmp.union_with(&self.get_block(succ).live_in);
                }

                // Propagate succ.live_in to block.live_out
                if self.get_block(block).live_out != tmp {
                    self.get_mut_block(block).live_out = tmp;
                    change = true;
                }

                // Propagate:
                // `union(diff(block.live_out, block.live_kill), block.live_gen)`
                // to block.live_in
                let mut old = self.get_block(block).live_out.clone();
                old.difference_with(&self.get_block(block).live_kill);
                old.union_with(&self.get_block(block).live_gen);
                if old != self.get_block(block).live_in {
                    self.get_mut_block(block).live_in = old;
                    change = true;
                }
            }
        }
    }
}
