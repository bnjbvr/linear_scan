use crate::graph::{GapAction, GapActionKind, GapState, Graph, InstrId};
use crate::*;

#[derive(PartialEq, Clone, Eq)]
enum MoveStatus {
    ToMove,
    Moving,
    Moved,
}

pub trait GapResolver {
    fn resolve_gaps(&mut self);
}

trait GapResolverHelper {
    fn resolve_gap(&mut self, id: &InstrId) -> GapState;
    fn move_one(
        &mut self,
        actions: &[GapAction],
        i: usize,
        s: &mut [MoveStatus],
        result: &mut Vec<GapAction>,
    ) -> bool;
}

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R> + Clone>
    GapResolver for Graph<K, G, R>
{
    fn resolve_gaps(&mut self) {
        let mut keys = vec![];
        for (id, _) in self.fields.gaps.iter() {
            keys.push(*id);
        }
        for id in keys.iter() {
            let state = self.resolve_gap(id);

            // Overwrite previous state
            self.fields.gaps.insert(*id, state);
        }
    }
}

impl<G: RegClass<Register = R>, R: Register<G>, K: Kind<RegClass = G, Register = R> + Clone>
    GapResolverHelper for Graph<K, G, R>
{
    fn resolve_gap(&mut self, id: &InstrId) -> GapState {
        let state = self.fields.gaps.remove(&id).unwrap();
        let mut status = vec![MoveStatus::ToMove; state.actions.len()];

        let mut i = 0;
        let mut result = vec![];
        while i < state.actions.len() {
            if status[i] == MoveStatus::ToMove {
                self.move_one(&state.actions, i, &mut status, &mut result);
            }
            i += 1;
        }
        GapState { actions: result }
    }

    fn move_one(
        &mut self,
        actions: &[GapAction],
        i: usize,
        s: &mut [MoveStatus],
        result: &mut Vec<GapAction>,
    ) -> bool {
        assert!(actions[i].kind == GapActionKind::Move);
        let from = self.get_interval(&actions[i].from).value.clone();
        let to = self.get_interval(&actions[i].to).value.clone();

        // Ignore nop moves
        if from == to {
            return false;
        }

        s[i] = MoveStatus::Moving;
        let mut j = 0;
        let mut circular = false;
        let mut sentinel = false;
        while j < actions.len() {
            assert!(actions[j].kind == GapActionKind::Move);
            let other_from = self.get_interval(&actions[j].from).value.clone();

            if other_from == to {
                match &s[j] {
                    MoveStatus::ToMove => {
                        let r = self.move_one(actions, j, s, result);
                        if r {
                            assert!(!circular);
                            circular = true;
                        }
                    }
                    MoveStatus::Moving => {
                        sentinel = true;
                    }
                    MoveStatus::Moved => (),
                }
            }

            j += 1;
        }

        if circular {
            result.push(GapAction {
                kind: GapActionKind::Swap,
                from: actions[i].from,
                to: actions[i].to,
            });
        } else if !sentinel {
            result.push(actions[i].clone());
        }
        s[i] = MoveStatus::Moved;

        return circular || sentinel;
    }
}
