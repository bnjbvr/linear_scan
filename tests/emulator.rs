use linearscan::compat::SmallIntMap;
use linearscan::*;

use log::debug;

#[derive(PartialEq, Debug, Clone)]
pub enum TestKind {
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

// Register class
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TestRegClass {
    Normal,
    Double,
}

// Registers
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TestReg {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
}

impl RegClass for TestRegClass {
    type Register = TestReg;
    fn all_reg_classes() -> Vec<TestRegClass> {
        vec![TestRegClass::Normal, TestRegClass::Double]
    }
    fn registers(&self) -> Vec<TestReg> {
        match *self {
            TestRegClass::Normal => vec![TestReg::Rax, TestReg::Rbx, TestReg::Rcx, TestReg::Rdx],
            TestRegClass::Double => {
                vec![TestReg::Xmm1, TestReg::Xmm2, TestReg::Xmm3, TestReg::Xmm4]
            }
        }
    }
    fn to_uint(&self) -> usize {
        match self {
            TestRegClass::Normal => 0,
            TestRegClass::Double => 1,
        }
    }
    fn from_uint(i: usize) -> TestRegClass {
        match i {
            0 => TestRegClass::Normal,
            1 => TestRegClass::Double,
            _ => panic!(),
        }
    }
}

impl Register<TestRegClass> for TestReg {
    fn reg_class(&self) -> TestRegClass {
        match *self {
            TestReg::Rax | TestReg::Rbx | TestReg::Rcx | TestReg::Rdx => TestRegClass::Normal,
            TestReg::Xmm1 | TestReg::Xmm2 | TestReg::Xmm3 | TestReg::Xmm4 => TestRegClass::Double,
        }
    }

    fn to_uint(&self) -> usize {
        match self {
            TestReg::Rax | TestReg::Xmm1 => 0,
            TestReg::Rbx | TestReg::Xmm2 => 1,
            TestReg::Rcx | TestReg::Xmm3 => 2,
            TestReg::Rdx | TestReg::Xmm4 => 3,
        }
    }

    fn from_uint(g: &TestRegClass, i: usize) -> TestReg {
        match g {
            &TestRegClass::Normal => match i {
                0 => TestReg::Rax,
                1 => TestReg::Rbx,
                2 => TestReg::Rcx,
                3 => TestReg::Rdx,
                _ => panic!(),
            },
            &TestRegClass::Double => match i {
                0 => TestReg::Xmm1,
                1 => TestReg::Xmm2,
                2 => TestReg::Xmm3,
                3 => TestReg::Xmm4,
                _ => panic!(),
            },
        }
    }
}

impl Kind for TestKind {
    type RegClass = TestRegClass;
    type Register = TestReg;

    fn clobbers(&self, _: &TestRegClass) -> bool {
        match &self {
            &TestKind::Print => true,
            _ => false,
        }
    }

    fn temporary(&self) -> Vec<TestRegClass> {
        match self {
            &TestKind::BranchIfBigger => vec![TestRegClass::Normal],
            _ => vec![],
        }
    }

    fn use_kind(&self, i: usize) -> UseKind<TestRegClass, TestReg> {
        match self {
            &TestKind::BranchIfBigger if i == 0 => TestReg::Rcx.use_fixed(),
            &TestKind::JustUse => TestReg::Rbx.use_fixed(),
            &TestKind::FixedUse => {
                let r: TestReg = Register::from_uint(&TestRegClass::Normal, i);
                r.use_fixed()
            }
            &TestKind::Print => TestReg::Rdx.use_fixed(),
            &TestKind::Return => TestReg::Rax.use_fixed(),
            &TestKind::ReturnDouble => TestReg::Xmm1.use_fixed(),
            &TestKind::DoubleSum => TestRegClass::Double.use_reg(),
            &TestKind::ToDouble => TestRegClass::Normal.use_reg(),
            _ => TestRegClass::Normal.use_any(),
        }
    }

    fn result_kind(&self) -> Option<UseKind<TestRegClass, TestReg>> {
        match self {
            &TestKind::Return => None,
            &TestKind::ReturnDouble => None,
            &TestKind::BranchIfBigger => None,
            &TestKind::JustUse => None,
            &TestKind::FixedUse => None,
            &TestKind::Nop => None,
            &TestKind::DoubleNumber(_) => Some(TestRegClass::Double.use_any()),
            &TestKind::DoubleSum => Some(TestRegClass::Double.use_reg()),
            &TestKind::ToDouble => Some(TestRegClass::Double.use_reg()),
            _ => Some(TestRegClass::Normal.use_reg()),
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

#[derive(Debug, Clone)]
enum Instruction {
    Move(Value<TestRegClass, TestReg>, Value<TestRegClass, TestReg>),
    Swap(Value<TestRegClass, TestReg>, Value<TestRegClass, TestReg>),
    UnexpectedEnd,
    Block(BlockId),
    Goto(BlockId),
    Generic(GenericInstruction),
}

#[derive(Debug, Clone)]
struct GenericInstruction {
    kind: TestKind,
    output: Option<Value<TestRegClass, TestReg>>,
    inputs: Vec<Value<TestRegClass, TestReg>>,
    temporary: Vec<Value<TestRegClass, TestReg>>,
    succ: Vec<BlockId>,
}

impl GeneratorFunctions<TestKind, TestRegClass, TestReg> for Emulator {
    fn prelude(&mut self) {
        // nop
    }

    fn epilogue(&mut self) {
        self.instructions.push(Instruction::UnexpectedEnd);
    }

    fn swap(&mut self, left: &Value<TestRegClass, TestReg>, right: &Value<TestRegClass, TestReg>) {
        self.instructions
            .push(Instruction::Swap(left.clone(), right.clone()));
    }

    fn move_(&mut self, from: &Value<TestRegClass, TestReg>, to: &Value<TestRegClass, TestReg>) {
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
        kind: &TestKind,
        output: Option<Value<TestRegClass, TestReg>>,
        inputs: &[Value<TestRegClass, TestReg>],
        temporary: &[Value<TestRegClass, TestReg>],
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

pub fn run_test(
    expected: UintOrDouble,
    mut body: impl FnMut(&mut GraphBuilder<TestKind, TestRegClass, TestReg>),
) {
    let _ = pretty_env_logger::try_init();

    let mut g = GraphBuilder::new();

    body(&mut g);
    let mut g = g.finish();

    debug!("Generated graph: {:#?}", g);

    g.allocate().unwrap();

    debug!("Graph after allocation: {:#?}", g);

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

    fn run(&mut self, graph: &Graph<TestKind, TestRegClass, TestReg>) -> UintOrDouble {
        // Generate instructions
        debug!("generating graph...");
        graph.generate(self);

        let instructions = self.instructions.clone();
        debug!("emulator interpreter loop starting!");
        loop {
            // Execution finished
            if self.result.is_some() {
                debug!("done with execution, returning result.");
                return self.result.clone().unwrap();
            }

            debug!("opcode: {:?}", instructions[self.ip]);
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

    fn get(&self, slot: &Value<TestRegClass, TestReg>) -> UintOrDouble {
        match slot {
            Value::RegisterVal(r) if r.reg_class() == TestRegClass::Normal => {
                UintOrDouble::Uint(*self.registers.get(&r.to_uint()).expect("Defined register"))
            }
            Value::RegisterVal(r) if r.reg_class() == TestRegClass::Double => UintOrDouble::Double(
                *self
                    .double_registers
                    .get(&r.to_uint())
                    .expect("Defined f64 register"),
            ),
            Value::StackVal(TestRegClass::Normal, s) => {
                UintOrDouble::Uint(*self.stack.get(&s.to_uint()).expect("Defined stack slot"))
            }
            Value::StackVal(TestRegClass::Double, s) => UintOrDouble::Double(
                *self
                    .double_stack
                    .get(&s.to_uint())
                    .expect("Defined f64 stack slot"),
            ),
            _ => panic!(),
        }
    }

    fn put(&mut self, slot: Value<TestRegClass, TestReg>, value: UintOrDouble) {
        match slot {
            Value::RegisterVal(r) if r.reg_class() == TestRegClass::Normal => {
                self.registers.insert(r.to_uint(), value.unwrap_left());
            }
            Value::RegisterVal(r) if r.reg_class() == TestRegClass::Double => {
                self.double_registers
                    .insert(r.to_uint(), value.unwrap_right());
            }
            Value::StackVal(TestRegClass::Normal, s) => {
                self.stack.insert(s.to_uint(), value.unwrap_left());
            }
            Value::StackVal(TestRegClass::Double, s) => {
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
            TestKind::Increment => self.put(
                out.expect("Increment out"),
                UintOrDouble::Uint(inputs[0].unwrap_left() + 1),
            ),
            TestKind::JustUse => (),  // nop
            TestKind::FixedUse => (), // nop
            TestKind::Nop => (),      // nop
            TestKind::Print => self.put(out.expect("Print out"), UintOrDouble::Uint(0)),
            TestKind::Number(n) => self.put(out.expect("Number out"), UintOrDouble::Uint(n)),
            TestKind::DoubleNumber(n) => {
                self.put(out.expect("Double Number out"), UintOrDouble::Double(n))
            }
            TestKind::Sum => self.put(
                out.expect("Sum out"),
                UintOrDouble::Uint(inputs[0].unwrap_left() + inputs[1].unwrap_left()),
            ),
            TestKind::MultAdd => self.put(
                out.expect("Mult add out"),
                UintOrDouble::Uint(
                    inputs[0].unwrap_left() * inputs[1].unwrap_left() + inputs[2].unwrap_left(),
                ),
            ),
            TestKind::DoubleSum => self.put(
                out.expect("Double sum out"),
                UintOrDouble::Double(inputs[0].unwrap_right() + inputs[1].unwrap_right()),
            ),
            TestKind::ToDouble => self.put(
                out.expect("ToDouble out"),
                UintOrDouble::Double(inputs[0].unwrap_left() as f64),
            ),
            TestKind::Return => {
                assert!(inputs[0].is_left());
                self.result = Some(inputs[0].clone());
                return;
            }
            TestKind::ReturnDouble => {
                assert!(inputs[0].is_right());
                self.result = Some(inputs[0].clone());
                return;
            }
            TestKind::BranchIfBigger => {
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
