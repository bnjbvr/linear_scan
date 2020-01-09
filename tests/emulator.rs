use linearscan::compat::SmallIntMap;
use linearscan::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Kind {
    Increment,
    Sum,
    DoubleSum,
    MultAdd,
    BranchIfBigger,
    JustUse,
    FixedUse,
    Nop,
    Print,
    Number(usize),
    DoubleNumber(f64),
    ToDouble,
    Return,
    ReturnDouble,
}

// Register groups
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Group {
    Normal,
    Double,
}

// Registers
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
}

impl GroupHelper for Group {
    type Register = Register;
    fn groups() -> Vec<Group> {
        vec![Group::Normal, Group::Double]
    }
    fn registers(&self) -> Vec<Register> {
        match *self {
            Group::Normal => vec![Register::Rax, Register::Rbx, Register::Rcx, Register::Rdx],
            Group::Double => vec![
                Register::Xmm1,
                Register::Xmm2,
                Register::Xmm3,
                Register::Xmm4,
            ],
        }
    }
    fn to_uint(&self) -> usize {
        match self {
            Group::Normal => 0,
            Group::Double => 1,
        }
    }
    fn from_uint(i: usize) -> Group {
        match i {
            0 => Group::Normal,
            1 => Group::Double,
            _ => panic!(),
        }
    }
}

impl RegisterHelper<Group> for Register {
    fn group(&self) -> Group {
        match *self {
            Register::Rax | Register::Rbx | Register::Rcx | Register::Rdx => Group::Normal,
            Register::Xmm1 | Register::Xmm2 | Register::Xmm3 | Register::Xmm4 => Group::Double,
        }
    }

    fn to_uint(&self) -> usize {
        match self.group() {
            Group::Normal => match self {
                Register::Rax => 0,
                Register::Rbx => 1,
                Register::Rcx => 2,
                Register::Rdx => 3,
                _ => panic!(),
            },
            Group::Double => match self {
                Register::Xmm1 => 0,
                Register::Xmm2 => 1,
                Register::Xmm3 => 2,
                Register::Xmm4 => 3,
                _ => panic!(),
            },
        }
    }

    fn from_uint(g: &Group, i: usize) -> Register {
        match g {
            &Group::Normal => match i {
                0 => Register::Rax,
                1 => Register::Rbx,
                2 => Register::Rcx,
                3 => Register::Rdx,
                _ => panic!(),
            },
            &Group::Double => match i {
                0 => Register::Xmm1,
                1 => Register::Xmm2,
                2 => Register::Xmm3,
                3 => Register::Xmm4,
                _ => panic!(),
            },
        }
    }
}

impl KindHelper for Kind {
    type Group = Group;
    type Register = Register;

    fn clobbers(&self, _: &Group) -> bool {
        match &self {
            &Kind::Print => true,
            _ => false,
        }
    }

    fn temporary(&self) -> Vec<Group> {
        match self {
            &Kind::BranchIfBigger => vec![Group::Normal],
            _ => vec![],
        }
    }

    fn use_kind(&self, i: usize) -> UseKind<Group, Register> {
        match self {
            &Kind::BranchIfBigger if i == 0 => Register::Rcx.use_fixed(),
            &Kind::JustUse => Register::Rbx.use_fixed(),
            &Kind::FixedUse => {
                let r: Register = RegisterHelper::from_uint(&Group::Normal, i);
                r.use_fixed()
            }
            &Kind::Print => Register::Rdx.use_fixed(),
            &Kind::Return => Register::Rax.use_fixed(),
            &Kind::ReturnDouble => Register::Xmm1.use_fixed(),
            &Kind::DoubleSum => Group::Double.use_reg(),
            &Kind::ToDouble => Group::Normal.use_reg(),
            _ => Group::Normal.use_any(),
        }
    }

    fn result_kind(&self) -> Option<UseKind<Group, Register>> {
        match self {
            &Kind::Return => None,
            &Kind::ReturnDouble => None,
            &Kind::BranchIfBigger => None,
            &Kind::JustUse => None,
            &Kind::FixedUse => None,
            &Kind::Nop => None,
            &Kind::DoubleNumber(_) => Some(Group::Double.use_any()),
            &Kind::DoubleSum => Some(Group::Double.use_reg()),
            &Kind::ToDouble => Some(Group::Double.use_reg()),
            _ => Some(Group::Normal.use_reg()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UintOrDouble {
    Uint(usize),
    Double(f64),
}

impl UintOrDouble {
    fn unwrap_left(&self) -> usize {
        if let UintOrDouble::Uint(x) = self {
            *x
        } else {
            panic!("not a left")
        }
    }
    fn is_left(&self) -> bool {
        if let UintOrDouble::Uint(_) = self {
            true
        } else {
            false
        }
    }
    fn unwrap_right(&self) -> f64 {
        if let UintOrDouble::Double(x) = self {
            *x
        } else {
            panic!("not a right")
        }
    }
    fn is_right(&self) -> bool {
        if let UintOrDouble::Uint(_) = self {
            false
        } else {
            true
        }
    }
}

pub struct Emulator {
    ip: usize,
    instructions: Vec<Instruction>,
    blocks: SmallIntMap<usize>,
    result: Option<UintOrDouble>,
    registers: SmallIntMap<usize>,
    double_registers: SmallIntMap<f64>,
    stack: SmallIntMap<usize>,
    double_stack: SmallIntMap<f64>,
}

#[derive(Clone)]
enum Instruction {
    Move(Value<Group, Register>, Value<Group, Register>),
    Swap(Value<Group, Register>, Value<Group, Register>),
    UnexpectedEnd,
    Block(BlockId),
    Goto(BlockId),
    Generic(GenericInstruction),
}

#[derive(Clone)]
struct GenericInstruction {
    kind: Kind,
    output: Option<Value<Group, Register>>,
    inputs: Vec<Value<Group, Register>>,
    temporary: Vec<Value<Group, Register>>,
    succ: Vec<BlockId>,
}

impl GeneratorFunctions<Kind, Group, Register> for Emulator {
    fn prelude(&mut self) {
        // nop
    }

    fn epilogue(&mut self) {
        self.instructions.push(Instruction::UnexpectedEnd);
    }

    fn swap(&mut self, left: &Value<Group, Register>, right: &Value<Group, Register>) {
        self.instructions
            .push(Instruction::Swap(left.clone(), right.clone()));
    }

    fn move_(&mut self, from: &Value<Group, Register>, to: &Value<Group, Register>) {
        self.instructions
            .push(Instruction::Move(from.clone(), to.clone()));
    }

    fn block(&mut self, id: BlockId) {
        let ip = self.instructions.len();
        self.blocks.insert(id.to_uint(), ip);
        self.instructions.push(Instruction::Block(id));
    }

    fn goto(&mut self, id: BlockId) {
        self.instructions.push(Instruction::Goto(id));
    }

    fn instr(
        &mut self,
        kind: &Kind,
        output: Option<Value<Group, Register>>,
        inputs: &[Value<Group, Register>],
        temporary: &[Value<Group, Register>],
        succ: &[BlockId],
    ) {
        self.instructions
            .push(Instruction::Generic(GenericInstruction {
                kind: kind.clone(),
                output: output,
                inputs: inputs.to_owned(),
                temporary: temporary.to_owned(),
                succ: succ.to_owned(),
            }));
    }
}

pub fn run_test(expected: UintOrDouble, mut body: impl FnMut(&mut Graph<Kind, Group, Register>)) {
    let mut g = Graph::new();

    body(&mut g);

    g.allocate().unwrap();

    let mut emu = Emulator::new();
    let got = emu.run(&g);
    if got != expected {
        panic!("got {:?} expected {:?}", got, expected);
    }
}

impl Emulator {
    fn new() -> Emulator {
        Emulator {
            ip: 0,
            result: None,
            instructions: vec![],
            blocks: SmallIntMap::new(),
            registers: SmallIntMap::new(),
            double_registers: SmallIntMap::new(),
            stack: SmallIntMap::new(),
            double_stack: SmallIntMap::new(),
        }
    }

    fn run(&mut self, graph: &Graph<Kind, Group, Register>) -> UintOrDouble {
        // Generate instructions
        graph.generate(self);

        let instructions = self.instructions.clone();
        loop {
            // Execution finished
            if self.result.is_some() {
                return self.result.clone().unwrap();
            }

            match instructions[self.ip].clone() {
                Instruction::UnexpectedEnd => panic!("This end was really unexpected"),
                Instruction::Block(_) => {
                    self.ip += 1;
                }
                Instruction::Move(from, to) => {
                    let v = self.get(&from);
                    self.put(to, v);
                    self.ip += 1;
                }
                Instruction::Swap(left, right) => {
                    let t = self.get(&left);
                    let v = self.get(&right);
                    self.put(left, v);
                    self.put(right, t);
                    self.ip += 1;
                }
                Instruction::Goto(block) => {
                    let block_ip = self
                        .blocks
                        .get(&block.to_uint())
                        .expect("Block to be present");
                    self.ip = *block_ip;
                }
                Instruction::Generic(ref instr) => self.exec_generic(instr),
            }
        }
    }

    fn get(&self, slot: &Value<Group, Register>) -> UintOrDouble {
        match slot {
            Value::RegisterVal(r) if r.group() == Group::Normal => {
                UintOrDouble::Uint(*self.registers.get(&r.to_uint()).expect("Defined register"))
            }
            Value::RegisterVal(r) if r.group() == Group::Double => UintOrDouble::Double(
                *self
                    .double_registers
                    .get(&r.to_uint())
                    .expect("Defined f64 register"),
            ),
            Value::StackVal(Group::Normal, s) => {
                UintOrDouble::Uint(*self.stack.get(&s.to_uint()).expect("Defined stack slot"))
            }
            Value::StackVal(Group::Double, s) => UintOrDouble::Double(
                *self
                    .double_stack
                    .get(&s.to_uint())
                    .expect("Defined f64 stack slot"),
            ),
            _ => panic!(),
        }
    }

    fn put(&mut self, slot: Value<Group, Register>, value: UintOrDouble) {
        match slot {
            Value::RegisterVal(r) if r.group() == Group::Normal => {
                self.registers.insert(r.to_uint(), value.unwrap_left());
            }
            Value::RegisterVal(r) if r.group() == Group::Double => {
                self.double_registers
                    .insert(r.to_uint(), value.unwrap_right());
            }
            Value::StackVal(Group::Normal, s) => {
                self.stack.insert(s.to_uint(), value.unwrap_left());
            }
            Value::StackVal(Group::Double, s) => {
                self.double_stack.insert(s.to_uint(), value.unwrap_right());
            }
            _ => panic!(),
        };
    }

    fn exec_generic(&mut self, instr: &GenericInstruction) {
        let out = instr.output.clone();
        let inputs = instr.inputs.iter().map(|i| self.get(i)).collect::<Vec<_>>();

        let tmp = &instr.temporary;

        match instr.kind {
            Kind::Increment => self.put(
                out.expect("Increment out"),
                UintOrDouble::Uint(inputs[0].unwrap_left() + 1),
            ),
            Kind::JustUse => (),  // nop
            Kind::FixedUse => (), // nop
            Kind::Nop => (),      // nop
            Kind::Print => self.put(out.expect("Print out"), UintOrDouble::Uint(0)),
            Kind::Number(n) => self.put(out.expect("Number out"), UintOrDouble::Uint(n)),
            Kind::DoubleNumber(n) => {
                self.put(out.expect("Double Number out"), UintOrDouble::Double(n))
            }
            Kind::Sum => self.put(
                out.expect("Sum out"),
                UintOrDouble::Uint(inputs[0].unwrap_left() + inputs[1].unwrap_left()),
            ),
            Kind::MultAdd => self.put(
                out.expect("Mult add out"),
                UintOrDouble::Uint(
                    inputs[0].unwrap_left() * inputs[1].unwrap_left() + inputs[2].unwrap_left(),
                ),
            ),
            Kind::DoubleSum => self.put(
                out.expect("Double sum out"),
                UintOrDouble::Double(inputs[0].unwrap_right() + inputs[1].unwrap_right()),
            ),
            Kind::ToDouble => self.put(
                out.expect("ToDouble out"),
                UintOrDouble::Double(inputs[0].unwrap_left() as f64),
            ),
            Kind::Return => {
                assert!(inputs[0].is_left());
                self.result = Some(inputs[0].clone());
                return;
            }
            Kind::ReturnDouble => {
                assert!(inputs[0].is_right());
                self.result = Some(inputs[0].clone());
                return;
            }
            Kind::BranchIfBigger => {
                self.put(tmp[0].clone(), UintOrDouble::Uint(0));
                if inputs[0].unwrap_left() > inputs[1].unwrap_left() {
                    self.ip = *self
                        .blocks
                        .get(&instr.succ[0].to_uint())
                        .expect("branch true");
                } else {
                    self.ip = *self
                        .blocks
                        .get(&instr.succ[1].to_uint())
                        .expect("branch false");
                }
                return;
            }
        }

        // Move forward
        self.ip += 1;
    }
}
