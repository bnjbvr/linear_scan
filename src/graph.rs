use std::collections::BTreeMap;
use std::fmt;

use crate::compat::{BitvSet, SmallIntMap};
use crate::{Kind, RegClass, Register};

macro_rules! define_entity {
    ($name:ident, $short_name:literal) => {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
        pub struct $name(pub usize);

        impl $name {
            pub fn to_uint(&self) -> usize {
                self.0
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                write!(fmt, $short_name)?;
                write!(fmt, "{}", self.0)
            }
        }
    };
}

define_entity!(BlockId, "b");
define_entity!(InstrId, "i");
define_entity!(IntervalId, "int");
define_entity!(StackId, "s");

impl InstrId {
    pub fn prev(&self) -> InstrId {
        InstrId(self.to_uint() - 1)
    }
    pub fn next(&self) -> InstrId {
        InstrId(self.to_uint() + 1)
    }
}

pub struct BlockBuilder<'graph, K, G, R> {
    graph: &'graph mut GraphBuilder<K, G, R>,
    block: BlockId,
}

impl<'a, G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>>
    BlockBuilder<'a, K, G, R>
{
    /// add instruction to block
    pub fn add(&mut self, kind: K, args: Vec<InstrId>) -> InstrId {
        let instr_id = self.graph.new_instr(kind, args);
        self.add_existing(instr_id);
        instr_id
    }

    /// add existing instruction to block
    pub fn add_existing(&mut self, instr_id: InstrId) {
        assert!(!self.graph.get_instr(&instr_id).added);
        self.graph.get_mut_instr(&instr_id).added = true;
        self.graph.get_mut_instr(&instr_id).block = self.block;

        let block = self.graph.get_mut_block(&self.block);
        assert!(!block.ended);
        block.instructions.push(instr_id);
    }

    /// add arg to existing instruction in block
    pub fn add_arg(&mut self, id: InstrId, arg: InstrId) {
        assert!(self.graph.get_instr(&id).block == self.block);
        self.graph.get_mut_instr(&id).inputs.push(arg);
    }

    /// add phi movement to block
    pub fn to_phi(&mut self, input: InstrId, phi: InstrId) {
        let reg_class = match self.graph.get_instr(&phi).kind {
            InstrKind::Phi(ref reg_class) => reg_class.clone(),
            _ => panic!("Expected Phi argument"),
        };
        let out = self.graph.get_instr(&phi).output.expect("Phi output");
        let inp = self
            .graph
            .get_instr(&input)
            .output
            .expect("Phi input output");

        // Insert one hint
        if self.graph.get_interval(&out).hint.is_none() {
            self.graph.get_mut_interval(&out).hint = Some(inp);
        }

        let res = Instruction::new_empty(self.graph, InstrKind::ToPhi(reg_class), vec![input]);
        self.graph.get_mut_instr(&res).output = Some(out);
        self.add_existing(res);
        self.graph.get_mut_instr(&phi).inputs.push(res);
        assert!(self.graph.get_instr(&phi).inputs.len() <= 2);
    }

    /// end block
    pub fn end(&mut self) {
        let block = self.graph.get_mut_block(&self.block);
        assert!(!block.ended);
        assert!(block.instructions.len() > 0);
        block.ended = true;
    }

    /// add `target_id` to block's successors
    pub fn goto(&mut self, target_id: BlockId) {
        self.graph
            .get_mut_block(&self.block)
            .add_successor(target_id);
        self.graph
            .get_mut_block(&target_id)
            .add_predecessor(self.block);
        self.end();
    }

    /// add `left` and `right` to block's successors
    pub fn branch(&mut self, left: BlockId, right: BlockId) {
        self.graph
            .get_mut_block(&self.block)
            .add_successor(left)
            .add_successor(right);
        self.graph.get_mut_block(&left).add_predecessor(self.block);
        self.graph.get_mut_block(&right).add_predecessor(self.block);
        self.end();
    }

    /// mark block as root
    pub fn make_root(&mut self) {
        self.graph.set_root(self.block);
    }
}

#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    pub instructions: Vec<InstrId>,
    pub successors: Vec<BlockId>,
    pub predecessors: Vec<BlockId>,

    // Fields for flattener
    pub loop_index: usize,
    pub loop_depth: usize,
    pub incoming_forward_branches: usize,

    // Fields for liveness analysis
    pub live_gen: BitvSet,
    pub live_kill: BitvSet,
    pub live_in: BitvSet,
    pub live_out: BitvSet,

    pub ended: bool,
}

impl Block {
    /// Create new empty block
    pub fn new<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>>(
        graph: &mut GraphBuilder<K, G, R>,
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

    pub fn start(&self) -> InstrId {
        assert!(self.instructions.len() != 0);
        return *self.instructions.first().unwrap();
    }

    pub fn end(&self) -> InstrId {
        assert!(self.instructions.len() != 0);
        return self.instructions.last().unwrap().next();
    }
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

impl<K: fmt::Debug, G: fmt::Debug> fmt::Debug for Instruction<K, G> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            r#"{:?} in {:?}, in: {:?} temp: {:?} out: {:?} kind: {:?}"#,
            self.id, self.block, self.inputs, self.temporary, self.output, self.kind
        )
    }
}

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>>
    Instruction<K, G>
{
    /// Create instruction without output interval
    pub fn new_empty(
        graph: &mut GraphBuilder<K, G, R>,
        kind: InstrKind<K, G>,
        args: Vec<InstrId>,
    ) -> InstrId {
        let id = graph.next_instr_id();

        let mut temporary = vec![];
        for reg_class in kind.temporary().iter() {
            temporary.push(Interval::new(graph, reg_class.clone()));
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
        graph.fields.instructions.insert(r.id, r);
        return id;
    }

    /// Create instruction with output
    pub fn new(
        graph: &mut GraphBuilder<K, G, R>,
        kind: InstrKind<K, G>,
        args: Vec<InstrId>,
    ) -> InstrId {
        let output = match kind.result_kind() {
            Some(k) => Some(Interval::new(graph, k.reg_class())),
            None => None,
        };

        let instr = Instruction::new_empty(graph, kind, args);
        graph.get_mut_instr(&instr).output = output;
        return instr;
    }
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

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>> Kind
    for InstrKind<K, G>
{
    type RegClass = G;
    type Register = R;

    /// Return true if instruction is clobbering registers
    fn clobbers(&self, reg_class: &G) -> bool {
        match self {
            &InstrKind::User(ref k) => k.clobbers(reg_class),
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
    fn use_kind(&self, i: usize) -> UseKind<G, R> {
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

#[derive(Debug)]
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

impl<G: RegClass<Register = R>, R: Register<G>> Interval<G, R> {
    /// Create new virtual interval
    pub fn new<K: Kind<RegClass = G, Register = R>, S: GraphState>(
        graph: &mut GraphWithState<K, G, R, S>,
        reg_class: G,
    ) -> IntervalId {
        let r = Interval {
            id: graph.interval_id(),
            value: Value::VirtualVal(reg_class),
            hint: None,
            ranges: vec![],
            parent: None,
            uses: vec![],
            children: vec![],
            fixed: false,
        };
        let id = r.id;
        graph.fields.intervals.insert(r.id, r);
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
                || self.uses[0].kind.reg_class() == kind.reg_class()
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

#[derive(PartialEq, Eq, Clone)]
pub enum Value<G, R> {
    VirtualVal(G),
    RegisterVal(R),
    StackVal(G, StackId),
}

impl<G: RegClass<Register = R> + PartialEq, R: Register<G>> Value<G, R> {
    pub fn is_virtual(&self) -> bool {
        match self {
            Value::VirtualVal(_) => true,
            _ => false,
        }
    }
    pub fn is_register(&self) -> bool {
        match self {
            Value::RegisterVal(_) => true,
            _ => false,
        }
    }
    pub fn is_stack(&self) -> bool {
        match self {
            Value::StackVal(_, _) => true,
            _ => false,
        }
    }

    pub fn reg_class(&self) -> G {
        match self {
            &Value::VirtualVal(ref rc) => rc.clone(),
            &Value::RegisterVal(ref r) => r.reg_class(),
            &Value::StackVal(ref rc, _) => rc.clone(),
        }
    }
}

impl<G: fmt::Debug, R: fmt::Debug> fmt::Debug for Value<G, R> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::VirtualVal(reg_class) => write!(fmt, "vreg({:?})", reg_class),
            Value::RegisterVal(reg) => write!(fmt, "reg({:?})", reg),
            Value::StackVal(reg_class, stack_id) => {
                write!(fmt, "stack({:?}, {:?})", reg_class, stack_id)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UseKind<G, R> {
    UseAny(G),
    UseRegister(G),
    UseFixed(R),
}

impl<G: RegClass<Register = R>, R: Register<G>> UseKind<G, R> {
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

    pub fn reg_class(&self) -> G {
        match self {
            &UseKind::UseRegister(ref rc) => rc.clone(),
            &UseKind::UseAny(ref rc) => rc.clone(),
            &UseKind::UseFixed(ref r) => r.reg_class(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Use<G, R> {
    pub kind: UseKind<G, R>,
    pub pos: InstrId,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LiveRange {
    pub start: InstrId,
    pub end: InstrId,
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

#[derive(Debug)]
pub struct GapState {
    pub actions: Vec<GapAction>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum GapActionKind {
    Move,
    Swap,
}

#[derive(Debug, Clone)]
pub struct GapAction {
    pub kind: GapActionKind,
    pub from: IntervalId,
    pub to: IntervalId,
}

pub struct BuildingGraph {
    block_id: usize,
    root: Option<BlockId>,
}

pub struct FinishedGraph {
    pub root: BlockId,
}

impl fmt::Debug for FinishedGraph {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "root: {:?}", self.root)
    }
}

#[derive(Debug)]
pub struct GraphFields<K, G, R> {
    pub instr_id: usize,
    interval_id: usize,
    pub intervals: BTreeMap<IntervalId, Interval<G, R>>,
    pub blocks: BTreeMap<BlockId, Block>,
    pub instructions: BTreeMap<InstrId, Instruction<K, G>>,
    pub phis: Vec<InstrId>,
    pub gaps: BTreeMap<InstrId, GapState>,
    /// Maps RegClass -> Register -> IntervalId.
    pub physical: SmallIntMap<SmallIntMap<IntervalId>>,
}

impl<K, G, R> GraphFields<K, G, R> {
    pub fn new() -> Self {
        Self {
            instr_id: 0,
            interval_id: 0,
            intervals: BTreeMap::new(),
            blocks: BTreeMap::new(),
            instructions: BTreeMap::new(),
            phis: vec![],
            gaps: BTreeMap::new(),
            physical: SmallIntMap::new(),
        }
    }
}

pub trait GraphState {}
impl GraphState for BuildingGraph {}
impl GraphState for FinishedGraph {}

#[derive(Debug)]
pub struct GraphWithState<K, G, R, S: GraphState> {
    pub fields: Box<GraphFields<K, G, R>>,
    pub state: S,
}

impl<K: Kind<RegClass = G, Register = R>, G: RegClass<Register = R>, R: Register<G>>
    GraphWithState<K, G, R, BuildingGraph>
{
    pub fn new() -> Self {
        Self {
            fields: Box::new(GraphFields::new()),
            state: BuildingGraph {
                block_id: 0,
                root: None,
            },
        }
    }

    // Instruction operations.

    /// Create new instruction outside the block
    pub fn new_instr(&mut self, kind: K, args: Vec<InstrId>) -> InstrId {
        Instruction::new(self, InstrKind::User(kind), args)
    }

    /// Create phi value
    pub fn phi(&mut self, reg_class: G) -> InstrId {
        let res = Instruction::new(self, InstrKind::Phi(reg_class), vec![]);
        // Prevent adding phi to block
        self.get_mut_instr(&res).added = true;
        self.fields.phis.push(res);
        res
    }

    // Block operations.

    /// Return the next block id.
    #[inline(always)]
    fn block_id(&mut self) -> BlockId {
        let r = self.state.block_id;
        self.state.block_id += 1;
        BlockId(r)
    }

    /// Create empty block and initialize it in the block.
    pub fn block(&mut self, body: impl Fn(&mut BlockBuilder<K, G, R>)) -> BlockId {
        let block = Block::new(self);
        let id = block.id;
        self.fields.blocks.insert(id, block);
        // Execute body.
        self.with_block(id, body);
        id
    }

    /// Perform operations on block.
    pub fn with_block(&mut self, id: BlockId, body: impl Fn(&mut BlockBuilder<K, G, R>)) {
        let mut b = BlockBuilder {
            graph: self,
            block: id,
        };
        body(&mut b);
    }

    /// Create an empty block.
    pub fn empty_block(&mut self) -> BlockId {
        let block = Block::new(self);
        let id = block.id;
        self.fields.blocks.insert(id, block);
        id
    }

    /// Set graph's root block
    fn set_root(&mut self, root_id: BlockId) {
        assert!(self.state.root.is_none());
        self.state.root = Some(root_id);
    }

    pub fn finish(self) -> Graph<K, G, R> {
        Graph {
            fields: self.fields,
            state: FinishedGraph {
                root: self.state.root.unwrap(),
            },
        }
    }
}

// Functions common to both graph states.
impl<K, G, R, S: GraphState> GraphWithState<K, G, R, S> {
    /// Return the next instruction id.
    #[inline(always)]
    pub fn next_instr_id(&mut self) -> InstrId {
        // TODO should be in the only constructable state?
        let r = self.fields.instr_id;
        self.fields.instr_id += 1;
        InstrId(r)
    }

    /// Return the next interval id.
    #[inline(always)]
    fn interval_id(&mut self) -> IntervalId {
        let r = self.fields.interval_id;
        self.fields.interval_id += 1;
        IntervalId(r)
    }

    /// Mutable instruction getter
    pub fn get_mut_instr<'r>(&'r mut self, id: &InstrId) -> &'r mut Instruction<K, G> {
        self.fields.instructions.get_mut(&id).unwrap()
    }

    pub fn get_instr<'r>(&'r self, id: &InstrId) -> &'r Instruction<K, G> {
        self.fields.instructions.get(&id).unwrap()
    }

    /// Mutable block getter
    pub fn get_mut_block<'r>(&'r mut self, id: &BlockId) -> &'r mut Block {
        self.fields.blocks.get_mut(&id).unwrap()
    }

    pub fn get_interval<'r>(&'r self, id: &IntervalId) -> &'r Interval<G, R> {
        self.fields.intervals.get(&id).unwrap()
    }

    /// Mutable interval getter
    pub fn get_mut_interval<'r>(&'r mut self, id: &IntervalId) -> &'r mut Interval<G, R> {
        self.fields.intervals.get_mut(&id).unwrap()
    }
}

pub type GraphBuilder<K, G, R> = GraphWithState<K, G, R, BuildingGraph>;
pub type Graph<K, G, R> = GraphWithState<K, G, R, FinishedGraph>;

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R>>
    Graph<K, G, R>
{
    /// Create gap (internal)
    pub fn create_gap(&mut self, block: BlockId) -> Instruction<K, G> {
        let id = self.next_instr_id();
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

    pub fn get_block<'r>(&'r self, id: &BlockId) -> &'r Block {
        self.fields.blocks.get(&id).unwrap()
    }

    /// Return ordered list of blocks
    pub fn get_block_list(&self) -> Vec<BlockId> {
        let mut blocks = vec![];
        for (_, block) in self.fields.blocks.iter() {
            blocks.push(block.id);
        }
        blocks
    }

    /// Instruction output getter
    pub fn get_output(&self, id: &InstrId) -> IntervalId {
        self.fields
            .instructions
            .get(&id)
            .unwrap()
            .output
            .expect("Instruction output")
    }

    /// Mutable gap state getter
    pub fn get_mut_gap<'r>(&'r mut self, id: &InstrId) -> &'r mut GapState {
        if !self.fields.gaps.contains_key(&id) {
            self.fields.gaps.insert(*id, GapState { actions: vec![] });
        }
        self.fields.gaps.get_mut(&id).unwrap()
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
    pub fn optimal_split_pos(&self, reg_class: &G, start: InstrId, end: InstrId) -> InstrId {
        // Fast and unfortunate case
        if start == end {
            return end;
        }

        let mut best_pos = end;
        let mut best_depth = usize::max_value();
        for (_, block) in self.fields.blocks.iter() {
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
        if !self.is_gap(&best_pos) && !self.clobbers(reg_class, &best_pos) {
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
        let reg_class = self.get_interval(id).value.reg_class();
        assert!(self.is_gap(&pos) || self.clobbers(&reg_class, &pos));

        let child = Interval::new(self, reg_class.clone());
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
        let split_at_call = self.clobbers(&reg_class, &pos);
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
        let split_on_call = self.get_instr(&pos).kind.clobbers(&reg_class);

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
    pub fn clobbers(&self, reg_class: &G, pos: &InstrId) -> bool {
        return self.get_instr(pos).kind.clobbers(reg_class);
    }
}
