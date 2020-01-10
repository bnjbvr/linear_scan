use linearscan::*;

use emulator::*;
mod emulator;

#[test]
fn realword_example() {
    run_test(UintOrDouble::Uint(21), |g| {
        let phi = g.phi(TestRegClass::Normal);

        let cond = g.empty_block();
        let left = g.empty_block();
        let after_left = g.empty_block();
        let right = g.empty_block();
        let ret = g.new_instr(TestKind::Number(10), vec![]);

        g.block(|b| {
            b.make_root();

            b.add_existing(ret);
            let zero = b.add(TestKind::Number(0), vec![]);
            b.to_phi(zero, phi);
            b.goto(cond);
        });

        g.with_block(cond, |b| {
            let ten = b.add(TestKind::Number(10), vec![]);
            b.add(TestKind::JustUse, vec![phi]);
            b.add(TestKind::BranchIfBigger, vec![phi, ten]);
            b.branch(right, left);
        });

        g.with_block(left, |b| {
            let print_res = b.add(TestKind::Print, vec![phi]);
            b.add(TestKind::Increment, vec![print_res]);
            b.goto(after_left);
        });

        g.with_block(after_left, |b| {
            let counter = b.add(TestKind::Increment, vec![phi]);
            b.to_phi(counter, phi);
            b.goto(cond);
        });

        g.with_block(right, |b| {
            let sum = b.add(TestKind::Sum, vec![ret, phi]);
            b.add(TestKind::Return, vec![sum]);
            b.end();
        });
    });
}

#[test]
fn nested_loops() {
    struct LoopResult {
        pre: BlockId,
        after: BlockId,
        out: InstrId,
    }

    run_test(UintOrDouble::Uint(125), |g| {
        fn create_loop(
            g: &mut GraphBuilder<TestKind, TestRegClass, TestReg>,
            inp: InstrId,
            f: impl Fn(
                &mut GraphBuilder<TestKind, TestRegClass, TestReg>,
                InstrId,
            ) -> Option<LoopResult>,
        ) -> Option<LoopResult> {
            let phi = g.phi(TestRegClass::Normal);
            let res_phi = g.phi(TestRegClass::Normal);
            let cond = g.empty_block();
            let body = g.empty_block();
            let after = g.empty_block();

            // Pre
            let pre = g.block(|b| {
                let init = b.add(TestKind::Number(0), vec![]);
                b.to_phi(init, phi);
                b.to_phi(inp, res_phi);
                b.goto(cond);
            });

            // Cond
            g.with_block(cond, |b| {
                let limit = b.add(TestKind::Number(4), vec![]);
                b.add(TestKind::BranchIfBigger, vec![phi, limit]);
                b.branch(after, body);
            });

            // Body
            g.with_block(body, |b| {
                let next = b.add(TestKind::Increment, vec![phi]);
                b.to_phi(next, phi);
            });

            g.with_block(after, |b| {
                b.add(TestKind::Nop, vec![]);
            });

            match f(g, res_phi) {
                // Link loops together
                Some(LoopResult { pre, after, out }) => {
                    g.with_block(body, |b| {
                        b.goto(pre);
                    });
                    g.with_block(after, |b| {
                        b.to_phi(out, res_phi);
                        b.goto(cond);
                    });
                }
                // Just loop
                None => {
                    g.with_block(body, |b| {
                        let next = b.add(TestKind::Increment, vec![res_phi]);
                        b.to_phi(next, res_phi);
                        b.goto(cond);
                    });
                }
            };

            Some(LoopResult {
                pre: pre,
                after: after,
                out: res_phi,
            })
        }

        let inp = g.new_instr(TestKind::Number(0), vec![]);
        let LoopResult { pre, after, out } = create_loop(g, inp, |g, inp| {
            create_loop(g, inp, |g, inp| create_loop(g, inp, |_, _| None))
        })
        .unwrap();

        // Start
        g.block(|b| {
            b.make_root();
            b.add_existing(inp);
            b.goto(pre);
        });

        g.with_block(after, |b| {
            b.add(TestKind::Return, vec![out]);
            b.end();
        });
    });
}

#[test]
fn trivial() {
    run_test(UintOrDouble::Uint(42), |g| {
        g.block(|b| {
            b.make_root();
            let lhs = b.add(TestKind::Number(20), vec![]);
            let rhs = b.add(TestKind::Number(22), vec![]);
            let sum = b.add(TestKind::Sum, vec![lhs, rhs]);
            b.add(TestKind::Return, vec![sum]);
            b.end();
        });
    });
}

#[test]
fn double_and_normal() {
    run_test(UintOrDouble::Double(286.875), |g| {
        g.block(|b| {
            b.make_root();

            // Create very high register pressure
            let mut normals = vec![];
            let mut doubles = vec![];
            let count = 16;
            for i in 0..count {
                normals.push(b.add(TestKind::Number(i + 1), vec![]));
                doubles.push(b.add(TestKind::DoubleNumber(((i + 1) as f64) / 8f64), vec![]));
            }

            let mut total = b.add(TestKind::DoubleNumber(0f64), vec![]);
            for i in (count - 1..0).rev() {
                let left = b.add(TestKind::Sum, vec![normals[i - 1], normals[i]]);
                let right = b.add(TestKind::DoubleSum, vec![doubles[i - 1], doubles[i]]);
                let double_left = b.add(TestKind::ToDouble, vec![left]);

                let subtotal = b.add(TestKind::DoubleSum, vec![double_left, right]);
                total = b.add(TestKind::DoubleSum, vec![total, subtotal]);
            }
            b.add(TestKind::ReturnDouble, vec![total]);
            b.end();
        });
    });
}

#[test]
fn parallel_move_cycles() {
    run_test(UintOrDouble::Uint(1234), |g| {
        g.block(|b| {
            b.make_root();

            let n1 = b.add(TestKind::Number(1), vec![]);
            let n2 = b.add(TestKind::Number(2), vec![]);
            let n3 = b.add(TestKind::Number(3), vec![]);
            let n4 = b.add(TestKind::Number(4), vec![]);

            // 1 <=> 2
            b.add(TestKind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(TestKind::FixedUse, vec![n2, n1, n3, n4]);

            // 1 <=> 2, 3 <=> 4
            b.add(TestKind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(TestKind::FixedUse, vec![n2, n1, n4, n3]);

            // shift
            b.add(TestKind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(TestKind::FixedUse, vec![n4, n1, n2, n3]);

            // reverse shift
            b.add(TestKind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(TestKind::FixedUse, vec![n2, n3, n4, n1]);

            // mixed
            b.add(TestKind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(TestKind::FixedUse, vec![n3, n2, n4, n1]);

            let ten = b.add(TestKind::Number(10), vec![]);
            let mut res = b.add(TestKind::Number(0), vec![]);
            res = b.add(TestKind::MultAdd, vec![res, ten, n1]);
            res = b.add(TestKind::MultAdd, vec![res, ten, n2]);
            res = b.add(TestKind::MultAdd, vec![res, ten, n3]);
            res = b.add(TestKind::MultAdd, vec![res, ten, n4]);

            b.add(TestKind::Return, vec![res]);
            b.end();
        });
    });
}
