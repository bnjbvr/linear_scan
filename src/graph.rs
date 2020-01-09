use crate::compat::{uint, BitvSet, SmallIntMap};
use crate::{GroupHelper, KindHelper, RegisterHelper};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct BlockId(pub uint);
impl BlockId {
    pub fn to_uint(&self) -> uint {
        self.0
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct InstrId(pub uint);
impl InstrId {
    pub fn to_uint(&self) -> uint {
        self.0
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct IntervalId(pub uint);
impl IntervalId {
    pub fn to_uint(&self) -> uint {
        self.0
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct StackId(pub uint);
impl StackId {
    pub fn to_uint(&self) -> uint {
        self.0
    }
}

pub struct Graph<K, G, R> {
    pub root: Option<BlockId>,
    block_id: uint,
    pub instr_id: uint,
    interval_id: uint,
    pub intervals: SmallIntMap<Interval<G, R>>,
    pub blocks: SmallIntMap<Block>,
    pub instructions: SmallIntMap<Instruction<K, G>>,
    pub phis: Vec<InstrId>,
    pub gaps: SmallIntMap<GapState>,
    pub prepared: bool,
    pub physical: SmallIntMap<SmallIntMap<IntervalId>>,
}

// Trait for all ids
pub trait GraphId {
    fn to_uint(&self) -> uint;
}

pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<InstrId>,
    pub successors: Vec<BlockId>,
    pub predecessors: Vec<BlockId>,

    // Fields for flattener
    pub loop_index: uint,
    pub loop_depth: uint,
    pub incoming_forward_branches: uint,

    // Fields for liveness analysis
    pub live_gen: BitvSet,
    pub live_kill: BitvSet,
    pub live_in: BitvSet,
    pub live_out: BitvSet,

    pub ended: bool,
}

#[derive(Clone)]
pub struct Instruction<K, G> {
    pub id: InstrId,
    pub block: BlockId,
    pub kind: InstrKind<K, G>,
    pub output: Option<IntervalId>,
    pub inputs: Vec<InstrId>,
    pub temporary: Vec<IntervalId>,
    pub added: bool,
}

// Abstraction to allow having user-specified instruction types
// as well as internal movement instructions
#[derive(Debug, Clone)]
pub enum InstrKind<K, G> {
    User(K),
    Gap,
    Phi(G),
    ToPhi(G),
}

pub struct Interval<G, R> {
    pub id: IntervalId,
    pub value: Value<G, R>,
    pub hint: Option<IntervalId>,
    pub ranges: Vec<LiveRange>,
    pub parent: Option<IntervalId>,
    pub uses: Vec<Use<G, R>>,
    children: Vec<IntervalId>,
    pub fixed: bool,
}

#[derive(PartialEq, Eq, Clone)]
pub enum Value<G, R> {
    VirtualVal(G),
    RegisterVal(R),
    StackVal(G, StackId),
}

#[derive(Clone)]
pub struct Use<G, R> {
    pub kind: UseKind<G, R>,
    pub pos: InstrId,
}

#[derive(PartialEq, Eq, Clone)]
pub enum UseKind<G, R> {
    UseAny(G),
    UseRegister(G),
    UseFixed(R),
}

#[derive(PartialEq, Eq, Clone)]
pub struct LiveRange {
    pub start: InstrId,
    pub end: InstrId,
}

pub struct GapState {
    pub actions: Vec<GapAction>,
}

#[derive(PartialEq, Eq, Clone)]
pub enum GapActionKind {
    Move,
    Swap,
}

#[derive(Clone)]
pub struct GapAction {
    pub kind: GapActionKind,
    pub from: IntervalId,
    pub to: IntervalId,
}

impl<
        G: GroupHelper<Register = R>,
        R: RegisterHelper<G>,
        K: KindHelper<Group = G, Register = R>,
    > Graph<K, G, R>
{
    /// Create new graph
    pub fn new() -> Graph<K, G, R> {
        Graph {
            root: None,
            block_id: 0,
            instr_id: 0,
            interval_id: 0,
            intervals: SmallIntMap::new(),
            blocks: SmallIntMap::new(),
            instructions: SmallIntMap::new(),
            phis: vec![],
            gaps: SmallIntMap::new(),
            prepared: false,
            physical: SmallIntMap::new(),
        }
    }

    /// Create gap (internal)
    pub fn create_gap(&mut self, block: BlockId) -> Instruction<K, G> {
        let id = self.instr_id();
        return Instruction {
            id: id,
            block: block,
            kind: InstrKind::Gap,
            output: None,
            inputs: vec![],
            temporary: vec![],
            added: true,
        };
    }

    /// Mutable block getter
    pub fn get_mut_block<'r>(&'r mut self, id: &BlockId) -> &'r mut Block {
        self.blocks.get_mut(&id.to_uint()).unwrap()
    }

    pub fn get_block<'r>(&'r self, id: &BlockId) -> &'r Block {
        self.blocks.get(&id.to_uint()).unwrap()
    }

    /// Return ordered list of blocks
    pub fn get_block_list(&self) -> Vec<BlockId> {
        let mut blocks = vec![];
        for (_, block) in self.blocks.iter() {
            blocks.push(block.id);
        }
        return blocks;
    }

    /// Mutable instruction getter
    pub fn get_mut_instr<'r>(&'r mut self, id: &InstrId) -> &'r mut Instruction<K, G> {
        self.instructions.get_mut(&id.to_uint()).unwrap()
    }

    pub fn get_instr<'r>(&'r self, id: &InstrId) -> &'r Instruction<K, G> {
        self.instructions.get(&id.to_uint()).unwrap()
    }

    /// Instruction output getter
    pub fn get_output(&self, id: &InstrId) -> IntervalId {
        self.instructions
            .get(&id.to_uint())
            .unwrap()
            .output
            .expect("Instruction output")
    }

    /// Mutable interval getter
    pub fn get_mut_interval<'r>(&'r mut self, id: &IntervalId) -> &'r mut Interval<G, R> {
        self.intervals.get_mut(&id.to_uint()).unwrap()
    }

    pub fn get_interval<'r>(&'r self, id: &IntervalId) -> &'r Interval<G, R> {
        self.intervals.get(&id.to_uint()).unwrap()
    }

    /// Mutable gap state getter
    pub fn get_mut_gap<'r>(&'r mut self, id: &InstrId) -> &'r mut GapState {
        if !self.gaps.contains_key(&id.to_uint()) {
            self.gaps.insert(id.to_uint(), GapState { actions: vec![] });
        }
        self.gaps.get_mut(&id.to_uint()).unwrap()
    }

    /// Find next intersection of two intervals
    pub fn get_intersection(&self, a: &IntervalId, b: &IntervalId) -> Option<InstrId> {
        let int_a = self.get_interval(a);
        let int_b = self.get_interval(b);

        for a in int_a.ranges.iter() {
            for b in int_b.ranges.iter() {
                match a.get_intersection(b) {
                    Some(pos) => return Some(pos),
                    _ => (),
                }
            }
        }

        return None;
    }

    /// Return `true` if `pos` is either some block's start or end
    pub fn block_boundary(&self, pos: InstrId) -> bool {
        let block = self.get_block(&self.get_instr(&pos).block);
        return block.start() == pos || block.end() == pos;
    }

    /// Find optimal split position between two instructions
    pub fn optimal_split_pos(&self, group: &G, start: InstrId, end: InstrId) -> InstrId {
        // Fast and unfortunate case
        if start == end {
            return end;
        }

        let mut best_pos = end;
        let mut best_depth = uint::max_value();
        for (_, block) in self.blocks.iter() {
            if best_depth >= block.loop_depth {
                let block_to = block.end();

                // Choose the most shallow block
                if start < block_to && block_to <= end {
                    best_pos = block_to;
                    best_depth = block.loop_depth;
                }
            }
        }

        // Always split at gap
        if !self.is_gap(&best_pos) && !self.clobbers(group, &best_pos) {
            assert!(best_pos.to_uint() >= start.next().to_uint());
            best_pos = best_pos.prev();
        }
        assert!(start < best_pos && best_pos <= end);
        return best_pos;
    }

    /// Split interval or one of it's children at specified position, return
    /// id of split child.
    pub fn split_at(&mut self, id: &IntervalId, pos: InstrId) -> IntervalId {
        // We should always make progress
        assert!(self.get_interval(id).start() < pos);

        // Split could be either at gap or at call
        let group = self.get_interval(id).value.group();
        assert!(self.is_gap(&pos) || self.clobbers(&group, &pos));

        let child = Interval::new(self, group.clone());
        let parent = match self.get_interval(id).parent {
            Some(parent) => parent,
            None => *id,
        };

        // Find appropriate child interval
        let mut split_parent = parent;
        if !self.get_interval(&split_parent).covers(pos) {
            for child in self.get_interval(&split_parent).children.iter() {
                if self.get_interval(child).covers(pos) {
                    split_parent = *child;
                }
            }
            assert!(self.get_interval(&split_parent).covers(pos));
        }

        // Insert movement
        let split_at_call = self.clobbers(&group, &pos);
        if split_at_call || !self.block_boundary(pos) {
            self.get_mut_gap(&pos).add_move(&split_parent, &child);
        }

        // Move out ranges
        let mut child_ranges = vec![];
        let parent_ranges = self
            .get_interval(&split_parent)
            .ranges
            .iter()
            .cloned()
            .filter_map(|range| {
                if range.end <= pos {
                    Some(range)
                } else if range.start < pos {
                    // Split required
                    child_ranges.push(LiveRange {
                        start: pos,
                        end: range.end,
                    });
                    Some(LiveRange {
                        start: range.start,
                        end: pos,
                    })
                } else {
                    child_ranges.push(range);
                    None
                }
            })
            .collect::<Vec<_>>();

        // Ensure that at least one range is always present
        assert!(child_ranges.len() != 0);
        assert!(parent_ranges.len() != 0);
        self.get_mut_interval(&child).ranges = child_ranges;
        self.get_mut_interval(&split_parent).ranges = parent_ranges;

        // Insert register hint
        self.get_mut_interval(&child).hint = Some(split_parent);

        // Move out uses
        let mut child_uses = vec![];
        let split_on_call = self.get_instr(&pos).kind.clobbers(&group);

        // XXX: Wait for rust bug to be fixed and use filter_mapped
        let mut parent_uses = self.get_interval(&split_parent).uses.clone();
        parent_uses.retain(|u| {
            if split_on_call && u.pos <= pos || !split_on_call && u.pos < pos {
                true
            } else {
                child_uses.push(u.clone());
                false
            }
        });
        self.get_mut_interval(&child).uses = child_uses;
        self.get_mut_interval(&split_parent).uses = parent_uses;

        // Add child
        let mut index = 0;
        for (i, child) in self.get_interval(&parent).children.iter().rev().enumerate() {
            if self.get_interval(child).end() <= pos {
                index = i + 1;
                break;
            }
        }
        self.get_mut_interval(&parent).children.insert(index, child);
        self.get_mut_interval(&child).parent = Some(parent);

        return child;
    }

    /// Find child interval, that covers specified position
    pub fn child_at(&self, parent: &IntervalId, pos: InstrId) -> Option<IntervalId> {
        let test = |interval_id| {
            let interval = self.get_interval(interval_id);
            interval.start() <= pos && pos < interval.end()
        };
        if test(parent) {
            return Some(*parent);
        }
        self.get_interval(parent)
            .children
            .iter()
            .find(|&child_id| test(child_id))
            .map(|x| *x)
    }

    pub fn child_with_use_at(&self, parent: &IntervalId, pos: InstrId) -> Option<IntervalId> {
        let test = |interval_id| {
            let interval = self.get_interval(interval_id);
            interval.start() <= pos
                && pos <= interval.end()
                && interval.uses.iter().any(|u| u.pos == pos)
        };
        if test(parent) {
            return Some(*parent);
        }
        self.get_interval(parent)
            .children
            .iter()
            .find(|&child_id| test(child_id))
            .map(|x| *x)
    }

    pub fn get_value(&self, i: &IntervalId, pos: InstrId) -> Option<Value<G, R>> {
        let child = self.child_with_use_at(i, pos);
        match child {
            Some(child) => Some(self.get_interval(&child).value.clone()),
            None => None,
        }
    }

    /// Return true if instruction at specified position is Gap
    pub fn is_gap(&self, pos: &InstrId) -> bool {
        match &self.get_instr(pos).kind {
            InstrKind::Gap => true,
            _ => false,
        }
    }

    /// Return true if instruction at specified position contains
    /// register-clobbering call.
    pub fn clobbers(&self, group: &G, pos: &InstrId) -> bool {
        return self.get_instr(pos).kind.clobbers(group);
    }

    /// Return next block id, used at graph construction
    #[inline(always)]
    fn block_id(&mut self) -> BlockId {
        let r = self.block_id;
        self.block_id += 1;
        return BlockId(r);
    }

    /// Return next instruction id, used at graph construction
    #[inline(always)]
    pub fn instr_id(&mut self) -> InstrId {
        let r = self.instr_id;
        self.instr_id += 1;
        return InstrId(r);
    }

    /// Return next interval id, used at graph construction
    #[inline(always)]
    fn interval_id(&mut self) -> IntervalId {
        let r = self.interval_id;
        self.interval_id += 1;
        return IntervalId(r);
    }
}

impl Block {
    /// Create new empty block
    pub fn new<
        G: GroupHelper<Register = R>,
        R: RegisterHelper<G>,
        K: KindHelper<Group = G, Register = R>,
    >(
        graph: &mut Graph<K, G, R>,
    ) -> Block {
        Block {
            id: graph.block_id(),
            instructions: vec![],
            successors: vec![],
            predecessors: vec![],
            loop_index: 0,
            loop_depth: 0,
            incoming_forward_branches: 0,
            live_gen: BitvSet::new(),
            live_kill: BitvSet::new(),
            live_in: BitvSet::new(),
            live_out: BitvSet::new(),
            ended: false,
        }
    }

    pub fn add_successor<'r>(&'r mut self, succ: BlockId) -> &'r mut Block {
        assert!(self.successors.len() <= 2);
        self.successors.push(succ);
        return self;
    }

    pub fn add_predecessor(&mut self, pred: BlockId) {
        assert!(self.predecessors.len() <= 2);
        self.predecessors.push(pred);
        // NOTE: we'll decrease them later in flatten.rs
        self.incoming_forward_branches += 1;
    }
}

impl<
        G: GroupHelper<Register = R>,
        R: RegisterHelper<G>,
        K: KindHelper<Group = G, Register = R>,
    > Instruction<K, G>
{
    /// Create instruction without output interval
    pub fn new_empty(
        graph: &mut Graph<K, G, R>,
        kind: InstrKind<K, G>,
        args: Vec<InstrId>,
    ) -> InstrId {
        let id = graph.instr_id();

        let mut temporary = vec![];
        for group in kind.temporary().iter() {
            temporary.push(Interval::new(graph, group.clone()));
        }

        let r = Instruction {
            id: id,
            block: BlockId(0), // NOTE: this will be overwritten soon
            kind: kind,
            output: None,
            inputs: args.clone(),
            temporary: temporary,
            added: false,
        };
        graph.instructions.insert(r.id.to_uint(), r);
        return id;
    }

    /// Create instruction with output
    pub fn new(graph: &mut Graph<K, G, R>, kind: InstrKind<K, G>, args: Vec<InstrId>) -> InstrId {
        let output = match kind.result_kind() {
            Some(k) => Some(Interval::new(graph, k.group())),
            None => None,
        };

        let instr = Instruction::new_empty(graph, kind, args);
        graph.get_mut_instr(&instr).output = output;
        return instr;
    }
}

impl<G: GroupHelper<Register = R>, R: RegisterHelper<G>> Interval<G, R> {
    /// Create new virtual interval
    pub fn new<K: KindHelper<Group = G, Register = R>>(
        graph: &mut Graph<K, G, R>,
        group: G,
    ) -> IntervalId {
        let r = Interval {
            id: graph.interval_id(),
            value: Value::VirtualVal(group),
            hint: None,
            ranges: vec![],
            parent: None,
            uses: vec![],
            children: vec![],
            fixed: false,
        };
        let id = r.id;
        graph.intervals.insert(r.id.to_uint(), r);
        return id;
    }

    /// Add range to interval's live range list.
    /// NOTE: Ranges are ordered by start position
    pub fn add_range(&mut self, start: InstrId, end: InstrId) {
        assert!(self.ranges.len() == 0 || self.ranges.first().unwrap().start >= end);

        // Extend last range
        if self.ranges.len() > 0 && self.ranges.first().unwrap().start == end {
            self.ranges[0].start = start;
        } else {
            // Insert new range
            self.ranges.insert(
                0,
                LiveRange {
                    start: start,
                    end: end,
                },
            );
        }
    }

    /// Return mutable first range
    pub fn first_range<'r>(&'r mut self) -> &'r mut LiveRange {
        assert!(self.ranges.len() != 0);
        return &mut self.ranges[0];
    }

    /// Return interval's start position
    pub fn start(&self) -> InstrId {
        assert!(self.ranges.len() != 0);
        return self.ranges.first().unwrap().start;
    }

    /// Return interval's end position
    pub fn end(&self) -> InstrId {
        assert!(self.ranges.len() != 0);
        return self.ranges.last().unwrap().end;
    }

    /// Return true if one of the ranges contains `pos`
    pub fn covers(&self, pos: InstrId) -> bool {
        return self.ranges.iter().any(|range| range.covers(pos));
    }

    /// Add use to the interval's use list.
    /// NOTE: uses are ordered by increasing `pos`
    pub fn add_use(&mut self, kind: UseKind<G, R>, pos: InstrId) {
        assert!(
            self.uses.len() == 0
                || self.uses[0].pos > pos
                || self.uses[0].kind.group() == kind.group()
        );
        self.uses.insert(
            0,
            Use {
                kind: kind,
                pos: pos,
            },
        );
    }

    /// Return next UseFixed(...) after `after` position.
    pub fn next_fixed_use(&self, after: InstrId) -> Option<Use<G, R>> {
        for u in self.uses.iter() {
            match u.kind {
                UseKind::UseFixed(_) if u.pos >= after => {
                    return Some(u.clone());
                }
                _ => (),
            }
        }
        return None;
    }

    /// Return next UseFixed(...) or UseRegister after `after` position.
    pub fn next_use(&self, after: InstrId) -> Option<Use<G, R>> {
        for u in self.uses.iter() {
            if u.pos >= after && !u.kind.is_any() {
                return Some(u.clone());
            }
        }
        return None;
    }

    /// Return last UseFixed(...) or UseRegister before `before` position
    pub fn last_use(&self, before: InstrId) -> Option<Use<G, R>> {
        for u in self.uses.iter().rev() {
            if u.pos <= before && !u.kind.is_any() {
                return Some(u.clone());
            }
        }
        return None;
    }
}

impl<
        G: GroupHelper<Register = R>,
        R: RegisterHelper<G>,
        K: KindHelper<Group = G, Register = R>,
    > KindHelper for InstrKind<K, G>
{
    type Group = G;
    type Register = R;

    /// Return true if instruction is clobbering registers
    fn clobbers(&self, group: &G) -> bool {
        match self {
            &InstrKind::User(ref k) => k.clobbers(group),
            &InstrKind::Gap => false,
            &InstrKind::ToPhi(_) => false,
            &InstrKind::Phi(_) => false,
        }
    }

    /// Return count of instruction's temporary operands
    fn temporary(&self) -> Vec<G> {
        match self {
            &InstrKind::User(ref k) => k.temporary(),
            &InstrKind::Gap => vec![],
            &InstrKind::Phi(_) => vec![],
            &InstrKind::ToPhi(_) => vec![],
        }
    }

    /// Return use kind of instruction's `i`th input
    fn use_kind(&self, i: uint) -> UseKind<G, R> {
        match self {
            &InstrKind::User(ref k) => k.use_kind(i),
            &InstrKind::Gap => panic!("Gap can't have any input"),
            &InstrKind::Phi(ref g) => UseKind::UseAny(g.clone()),
            &InstrKind::ToPhi(ref g) => UseKind::UseAny(g.clone()),
        }
    }

    /// Return result kind of instruction or None, if instruction has no result
    fn result_kind(&self) -> Option<UseKind<G, R>> {
        match self {
            &InstrKind::User(ref k) => k.result_kind(),
            &InstrKind::Gap => None,
            &InstrKind::Phi(ref g) => Some(UseKind::UseAny(g.clone())),
            &InstrKind::ToPhi(ref g) => Some(UseKind::UseAny(g.clone())),
        }
    }
}

impl LiveRange {
    /// Return true if range contains position
    pub fn covers(&self, pos: InstrId) -> bool {
        return self.start <= pos && pos < self.end;
    }

    /// Return first intersection position of two ranges
    pub fn get_intersection(&self, other: &LiveRange) -> Option<InstrId> {
        if self.covers(other.start) {
            return Some(other.start);
        } else if other.start < self.start && self.start < other.end {
            return Some(self.start);
        }
        return None;
    }
}

impl<G: GroupHelper<Register = R> + PartialEq, R: RegisterHelper<G>> Value<G, R> {
    pub fn is_virtual(&self) -> bool {
        match self {
            &Value::VirtualVal(_) => true,
            _ => false,
        }
    }

    pub fn group(&self) -> G {
        match self {
            &Value::VirtualVal(ref g) => g.clone(),
            &Value::RegisterVal(ref r) => r.group(),
            &Value::StackVal(ref g, _) => g.clone(),
        }
    }
}

impl<G: GroupHelper<Register = R>, R: RegisterHelper<G>> UseKind<G, R> {
    pub fn is_fixed(&self) -> bool {
        match self {
            &UseKind::UseFixed(_) => true,
            _ => false,
        }
    }

    pub fn is_any(&self) -> bool {
        match self {
            &UseKind::UseAny(_) => true,
            _ => false,
        }
    }

    pub fn group(&self) -> G {
        match self {
            &UseKind::UseRegister(ref g) => g.clone(),
            &UseKind::UseAny(ref g) => g.clone(),
            &UseKind::UseFixed(ref r) => r.group(),
        }
    }
}

impl GapState {
    pub fn add_move(&mut self, from: &IntervalId, to: &IntervalId) {
        self.actions.push(GapAction {
            kind: GapActionKind::Move,
            from: *from,
            to: *to,
        });
    }
}

impl Block {
    pub fn start(&self) -> InstrId {
        assert!(self.instructions.len() != 0);
        return *self.instructions.first().unwrap();
    }

    pub fn end(&self) -> InstrId {
        assert!(self.instructions.len() != 0);
        // TODO? could also be last().unwrap()+1, was last().next()
        return *self.instructions.last().unwrap();
    }
}

// Implement trait for ids
impl GraphId for BlockId {
    fn to_uint(&self) -> uint {
        match self {
            &BlockId(id) => id,
        }
    }
}

impl GraphId for InstrId {
    fn to_uint(&self) -> uint {
        match self {
            &InstrId(id) => id,
        }
    }
}

impl InstrId {
    pub fn prev(&self) -> InstrId {
        InstrId(self.to_uint() - 1)
    }
    pub fn next(&self) -> InstrId {
        InstrId(self.to_uint() + 1)
    }
}

impl GraphId for IntervalId {
    fn to_uint(&self) -> uint {
        match self {
            &IntervalId(id) => id,
        }
    }
}

impl GraphId for StackId {
    fn to_uint(&self) -> uint {
        match self {
            &StackId(id) => id,
        }
    }
}
