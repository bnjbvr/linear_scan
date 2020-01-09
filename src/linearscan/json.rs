// TODO only used in testing
/*
use extra::json::{ToJson, Json, Object, List, String, Number, Boolean, Null};
use std::collections::HashMap;
use crate::{KindHelper, GroupHelper, RegisterHelper};
use crate::graph::{Graph, Block, Instruction, Interval, LiveRange,
                        User, Gap, GapState, Move, Swap, ToPhi, Phi,
                        Use, UseAny, UseRegister, UseFixed,
                        Value, VirtualVal, RegisterVal, StackVal};

trait JsonHelper {
  fn get_blocks(&self) -> Json;
  fn get_intervals(&self) -> Json;
  fn get_instructions(&self) -> Json;
}

impl<G: GroupHelper<R>+ToStr,
     R: RegisterHelper<G>+ToStr,
     K: KindHelper<G, R>+ToStr> ToJson for Block<K> {
  fn to_json(&self) -> Json {
    let mut obj = ~HashMap::new();

    obj.insert(~"id", Number(self.id.to_uint() as float));
    obj.insert(~"successors", List(do self.successors.map() |succ| {
      Number(succ.to_uint() as float)
    }));

    obj.insert(~"start", Number(self.start().to_uint() as float));
    obj.insert(~"end", Number(self.end().to_uint() as float));
    obj.insert(~"loop_depth", Number(self.loop_depth as float));

    return Object(obj);
  }
}

impl<G: GroupHelper<R>+ToStr,
     R: RegisterHelper<G>+ToStr,
     K: KindHelper<G, R>+ToStr> ToJson for Instruction<K, G> {
  fn to_json(&self) -> Json {
    let mut obj = ~HashMap::new();

    obj.insert(~"id", Number(self.id.to_uint() as float));
    obj.insert(~"block", Number(self.block.to_uint() as float));
    obj.insert(~"kind", String(match self.kind {
      User(ref kind) => kind.to_str(),
      Gap => ~"~gap",
      ToPhi(_) => ~"~to_phi",
      Phi(_) => ~"~phi"
    }));
    obj.insert(~"inputs", List(do self.inputs.map() |input| {
      Number(input.to_uint() as float)
    }));
    obj.insert(~"temporary", List(do self.temporary.map() |t| {
      Number(t.to_uint() as float)
    }));
    obj.insert(~"output", match self.output {
      Some(output) => Number(output.to_uint() as float),
      None => Null
    });

    return Object(obj);
  }
}

impl ToJson for GapState {
  fn to_json(&self) -> Json {
    let mut obj = ~HashMap::new();

    obj.insert(~"actions", List(do self.actions.map() |act| {
      let mut obj = ~HashMap::new();
      obj.insert(~"type", String(match act.kind {
        Move => ~"move",
        Swap => ~"swap"
      }));
      obj.insert(~"from", Number(act.from.to_uint() as float));
      obj.insert(~"to", Number(act.to.to_uint() as float));
      Object(obj)
    }));

    return Object(obj);
  }
}

impl<G: GroupHelper<R>+ToStr,
     R: RegisterHelper<G>+ToStr> ToJson for Interval<G, R> {
  fn to_json(&self) -> Json {
    let mut obj = ~HashMap::new();

    obj.insert(~"id", Number(self.id.to_uint() as float));
    obj.insert(~"parent", match self.parent {
      Some(id) => Number(id.to_uint() as float),
      None => Null
    });
    obj.insert(~"children", List(do self.children.map() |child| {
      Number(child.to_uint() as float)
    }));
    obj.insert(~"ranges", self.ranges.to_json());
    obj.insert(~"uses", self.uses.to_json());
    obj.insert(~"value", self.value.to_json());

    return Object(obj);
  }
}

impl ToJson for LiveRange {
  fn to_json(&self) -> Json {
    let mut obj = ~HashMap::new();

    obj.insert(~"start", Number(self.start.to_uint() as float));
    obj.insert(~"end", Number(self.end.to_uint() as float));

    return Object(obj);
  }
}

impl<G: GroupHelper<R>+ToStr,
     R: RegisterHelper<G>+ToStr> ToJson for Use<G, R> {
  fn to_json(&self) -> Json {
    let mut obj = ~HashMap::new();
    let mut kind = ~HashMap::new();

    match self.kind {
      UseAny(_) => kind.insert(~"type", String(~"any")),
      UseRegister(_) => kind.insert(~"type", String(~"reg")),
      UseFixed(ref val) => {
        kind.insert(~"type", String(~"fixed"));
        kind.insert(~"value", String(val.to_str()))
      }
    };
    obj.insert(~"group", Number(self.kind.group().to_uint() as float));
    obj.insert(~"kind", Object(kind));
    obj.insert(~"pos", Number(self.pos.to_uint() as float));

    return Object(obj);
  }
}

impl<G: GroupHelper<R>+ToStr,
     R: RegisterHelper<G>+ToStr> ToJson for Value<G, R> {
  fn to_json(&self) -> Json {
    return String(match self {
      &VirtualVal(ref g) => ~"v{" + g.to_str() + "}",
      &RegisterVal(ref id) => id.to_str(),
      &StackVal(ref g, id) => ~"s{" + g.to_str() + "}" + id.to_str()
    });
  }
}

impl<G: GroupHelper<R>+ToStr,
     R: RegisterHelper<G>+ToStr,
     K: KindHelper<G, R>+ToStr> JsonHelper for Graph<K, G, R> {
  fn get_blocks(&self) -> Json {
    let mut result = ~[];

    for (_, block) in self.blocks.iter() {
      result.push(block.to_json());
    }

    return List(result);
  }

  fn get_intervals(&self) -> Json {
    let mut result = ~[];

    for (_, interval) in self.intervals.iter() {
      let mut obj = match interval.to_json() {
        Object(obj) => obj,
        _ => fail!("Unexpected interval JSON type")
      };

      obj.insert(~"physical", Boolean(interval.fixed));
      result.push(Object(obj));
    }

    return List(result);
  }

  fn get_instructions(&self) -> Json {
    let mut result = ~HashMap::new();

    for (id, instruction) in self.instructions.iter() {
      let mut obj = match instruction.to_json() {
        Object(obj) => obj,
        _ => fail!("Unexpected instruction JSON type")
      };

      match self.gaps.find(&instruction.id.to_uint()) {
        Some(gap) => {
          obj.insert(~"gap_state", gap.to_json());
        },
        None => ()
      }

      result.insert(id.to_str(), Object(obj));
    }

    return Object(result);
  }
}

impl<G: GroupHelper<R>+ToStr,
     R: RegisterHelper<G>+ToStr,
     K: KindHelper<G, R>+ToStr> ToJson for Graph<K, G, R> {
  fn to_json(&self) -> Json {
    let mut result = ~HashMap::new();

    // Export blocks
    result.insert(~"blocks", self.get_blocks());

    // Export intervals
    result.insert(~"intervals", self.get_intervals());

    // Export instructions
    result.insert(~"instructions", self.get_instructions());

    return result.to_json();
  }
}
*/
