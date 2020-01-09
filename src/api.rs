// Private imports
use crate::graph::{Block, InstrKind, Instruction};

// Public API
pub use crate::allocator::Allocator;
pub use crate::generator::{Generator, GeneratorFunctions};
pub use crate::graph::{BlockId, Graph, InstrId, StackId, UseKind, Value};

pub struct BlockBuilder<'graph, K, G, R> {
    graph: &'graph mut Graph<K, G, R>,
    block: BlockId,
}

pub trait GroupHelper: Clone + Eq {
    type Register;

    fn groups() -> Vec<Self>;
    fn registers(&self) -> Vec<Self::Register>;
    fn to_uint(&self) -> usize;
    fn from_uint(i: usize) -> Self;

    fn use_any(&self) -> UseKind<Self, Self::Register> {
        UseKind::UseAny(self.clone())
    }
    fn use_reg(&self) -> UseKind<Self, Self::Register> {
        UseKind::UseRegister(self.clone())
    }
}

pub trait RegisterHelper<Group>: Clone + Eq {
    fn group(&self) -> Group;
    fn to_uint(&self) -> usize;
    fn from_uint(g: &Group, i: usize) -> Self;

    fn use_fixed(&self) -> UseKind<Group, Self> {
        UseKind::UseFixed(self.clone())
    }
}

pub trait KindHelper: Clone {
    type Group;
    type Register;
    fn clobbers(&self, group: &Self::Group) -> bool;
    fn temporary(&self) -> Vec<Self::Group>;
    fn use_kind(&self, i: usize) -> UseKind<Self::Group, Self::Register>;
    fn result_kind(&self) -> Option<UseKind<Self::Group, Self::Register>>;
}

pub trait GraphAPI<
    K: KindHelper<Group = G, Register = R>,
    G: GroupHelper<Register = R>,
    R: RegisterHelper<G>,
>
{
    fn empty_block(&mut self) -> BlockId;
    fn block(&mut self, body: impl Fn(&mut BlockBuilder<K, G, R>)) -> BlockId;
    fn phi(&mut self, group: G) -> InstrId;
    fn with_block(&mut self, id: BlockId, body: impl Fn(&mut BlockBuilder<K, G, R>));
    fn new_instr(&mut self, kind: K, args: Vec<InstrId>) -> InstrId;
    fn set_root(&mut self, id: BlockId);
}

impl<
        G: GroupHelper<Register = R>,
        R: RegisterHelper<G>,
        K: KindHelper<Group = G, Register = R>,
    > GraphAPI<K, G, R> for Graph<K, G, R>
{
    /// Create empty block
    fn empty_block(&mut self) -> BlockId {
        let block = Block::new(self);
        let id = block.id;
        self.blocks.insert(id.to_uint(), block);
        return id;
    }

    /// Create empty block and initialize it in the block
    fn block(&mut self, body: impl Fn(&mut BlockBuilder<K, G, R>)) -> BlockId {
        let block = Block::new(self);
        let id = block.id;
        self.blocks.insert(id.to_uint(), block);

        // Execute body
        self.with_block(id, body);

        return id;
    }

    /// Create phi value
    fn phi(&mut self, group: G) -> InstrId {
        let res = Instruction::new(self, InstrKind::Phi(group), vec![]);
        // Prevent adding phi to block
        self.get_mut_instr(&res).added = true;
        self.phis.push(res);
        return res;
    }

    /// Perform operations on block
    fn with_block(&mut self, id: BlockId, body: impl Fn(&mut BlockBuilder<K, G, R>)) {
        let mut b = BlockBuilder {
            graph: self,
            block: id,
        };
        body(&mut b);
    }

    /// Create new instruction outside the block
    fn new_instr(&mut self, kind: K, args: Vec<InstrId>) -> InstrId {
        return Instruction::new(self, InstrKind::User(kind), args);
    }

    /// Set graph's root block
    fn set_root(&mut self, id: BlockId) {
        self.root = Some(id);
    }
}

impl<
        'a,
        G: GroupHelper<Register = R>,
        R: RegisterHelper<G>,
        K: KindHelper<Group = G, Register = R>,
    > BlockBuilder<'a, K, G, R>
{
    /// add instruction to block
    pub fn add(&mut self, kind: K, args: Vec<InstrId>) -> InstrId {
        let instr_id = self.graph.new_instr(kind, args);
        self.add_existing(instr_id);
        return instr_id;
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
        let group = match self.graph.get_instr(&phi).kind {
            InstrKind::Phi(ref group) => group.clone(),
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

        let res = Instruction::new_empty(self.graph, InstrKind::ToPhi(group), vec![input]);
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
