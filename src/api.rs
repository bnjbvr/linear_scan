pub use crate::allocator::Allocator;
pub use crate::generator::{Generator, GeneratorFunctions};
pub use crate::graph::{BlockId, Graph, GraphBuilder, InstrId, StackId, UseKind, Value};

pub trait RegClass: Clone + Eq {
    type Register;

    fn all_reg_classes() -> Vec<Self>;

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

pub trait Register<RegClass>: Clone + Eq {
    fn reg_class(&self) -> RegClass;
    fn to_uint(&self) -> usize;
    fn from_uint(g: &RegClass, i: usize) -> Self;

    fn use_fixed(&self) -> UseKind<RegClass, Self> {
        UseKind::UseFixed(self.clone())
    }
}

pub trait Kind: Clone {
    type RegClass;
    type Register;

    fn clobbers(&self, group: &Self::RegClass) -> bool;
    fn temporary(&self) -> Vec<Self::RegClass>;
    fn use_kind(&self, i: usize) -> UseKind<Self::RegClass, Self::Register>;
    fn result_kind(&self) -> Option<UseKind<Self::RegClass, Self::Register>>;
}
