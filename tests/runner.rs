use linearscan::*;

use emulator::*;
mod emulator;

#[test]
fn realword_example() {
    run_test(UintOrDouble::Uint(21), |g| {
        let phi = g.phi(Group::Normal);

        let cond = g.empty_block();
        let left = g.empty_block();
        let after_left = g.empty_block();
        let right = g.empty_block();
        let ret = g.new_instr(Kind::Number(10), vec![]);

        g.block(|b| {
            b.make_root();

            b.add_existing(ret);
            let zero = b.add(Kind::Number(0), vec![]);
            b.to_phi(zero, phi);
            b.goto(cond);
        });

        g.with_block(cond, |b| {
            let ten = b.add(Kind::Number(10), vec![]);
            b.add(Kind::JustUse, vec![phi]);
            b.add(Kind::BranchIfBigger, vec![phi, ten]);
            b.branch(right, left);
        });

        g.with_block(left, |b| {
            let print_res = b.add(Kind::Print, vec![phi]);
            b.add(Kind::Increment, vec![print_res]);
            b.goto(after_left);
        });

        g.with_block(after_left, |b| {
            let counter = b.add(Kind::Increment, vec![phi]);
            b.to_phi(counter, phi);
            b.goto(cond);
        });

        g.with_block(right, |b| {
            let sum = b.add(Kind::Sum, vec![ret, phi]);
            b.add(Kind::Return, vec![sum]);
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
            g: &mut Graph<Kind, Group, Register>,
            inp: InstrId,
            f: impl Fn(&mut Graph<Kind, Group, Register>, InstrId) -> Option<LoopResult>,
        ) -> Option<LoopResult> {
            let phi = g.phi(Group::Normal);
            let res_phi = g.phi(Group::Normal);
            let cond = g.empty_block();
            let body = g.empty_block();
            let after = g.empty_block();

            // Pre
            let pre = g.block(|b| {
                let init = b.add(Kind::Number(0), vec![]);
                b.to_phi(init, phi);
                b.to_phi(inp, res_phi);
                b.goto(cond);
            });

            // Cond
            g.with_block(cond, |b| {
                let limit = b.add(Kind::Number(4), vec![]);
                b.add(Kind::BranchIfBigger, vec![phi, limit]);
                b.branch(after, body);
            });

            // Body
            g.with_block(body, |b| {
                let next = b.add(Kind::Increment, vec![phi]);
                b.to_phi(next, phi);
            });

            g.with_block(after, |b| {
                b.add(Kind::Nop, vec![]);
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
                        let next = b.add(Kind::Increment, vec![res_phi]);
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

        let inp = g.new_instr(Kind::Number(0), vec![]);
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
            b.add(Kind::Return, vec![out]);
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
                normals.push(b.add(Kind::Number(i + 1), vec![]));
                doubles.push(b.add(Kind::DoubleNumber(((i + 1) as f64) / 8f64), vec![]));
            }

            let mut total = b.add(Kind::DoubleNumber(0f64), vec![]);
            for i in (count - 1..0).rev() {
                let left = b.add(Kind::Sum, vec![normals[i - 1], normals[i]]);
                let right = b.add(Kind::DoubleSum, vec![doubles[i - 1], doubles[i]]);
                let double_left = b.add(Kind::ToDouble, vec![left]);

                let subtotal = b.add(Kind::DoubleSum, vec![double_left, right]);
                total = b.add(Kind::DoubleSum, vec![total, subtotal]);
            }
            b.add(Kind::ReturnDouble, vec![total]);
            b.end();
        });
    });
}

#[test]
fn parallel_move_cycles() {
    run_test(UintOrDouble::Uint(1234), |g| {
        g.block(|b| {
            b.make_root();

            let n1 = b.add(Kind::Number(1), vec![]);
            let n2 = b.add(Kind::Number(2), vec![]);
            let n3 = b.add(Kind::Number(3), vec![]);
            let n4 = b.add(Kind::Number(4), vec![]);

            // 1 <=> 2
            b.add(Kind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(Kind::FixedUse, vec![n2, n1, n3, n4]);

            // 1 <=> 2, 3 <=> 4
            b.add(Kind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(Kind::FixedUse, vec![n2, n1, n4, n3]);

            // shift
            b.add(Kind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(Kind::FixedUse, vec![n4, n1, n2, n3]);

            // reverse shift
            b.add(Kind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(Kind::FixedUse, vec![n2, n3, n4, n1]);

            // mixed
            b.add(Kind::FixedUse, vec![n1, n2, n3, n4]);
            b.add(Kind::FixedUse, vec![n3, n2, n4, n1]);

            let ten = b.add(Kind::Number(10), vec![]);
            let mut res = b.add(Kind::Number(0), vec![]);
            res = b.add(Kind::MultAdd, vec![res, ten, n1]);
            res = b.add(Kind::MultAdd, vec![res, ten, n2]);
            res = b.add(Kind::MultAdd, vec![res, ten, n3]);
            res = b.add(Kind::MultAdd, vec![res, ten, n4]);

            b.add(Kind::Return, vec![res]);
            b.end();
        });
    });
}
