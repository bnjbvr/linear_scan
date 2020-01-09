// Public API
pub use crate::allocator::Allocator;
pub use crate::generator::{Generator, GeneratorFunctions};
pub use crate::graph::{BlockId, Graph, GraphBuilder, InstrId, StackId, UseKind, Value};

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
