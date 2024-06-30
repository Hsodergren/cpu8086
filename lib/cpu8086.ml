module Inst = Inst
module Bytestream = Bytestream

module Register = struct
  type t = int

  let zero = 0

  let set ~part v t =
    match part with
    | `L -> (t land 0xFF00) + (v land 0xFF)
    | `H -> (t land 0x00FF) + ((v land 0xFF) lsl 8)
    | `X -> v land 0xFFFF

  let get ~part t =
    match part with
    | `L -> t land 0x00FF
    | `H -> (t land 0xFF00) lsr 8
    | `X -> t land 0xFFFF

  let to_string t = Printf.sprintf "0x%04X" t
end

module Registers = struct
  type t = {
    a: Register.t;
    b: Register.t;
    c: Register.t;
    d: Register.t;
    sp: Register.t;
    bp: Register.t;
    si: Register.t;
    di: Register.t;
  }

  type diff = (Registerlabel.t * Register.t * Register.t) list

  let zero = {a=Register.zero;
              b=Register.zero;
              c=Register.zero;
              d=Register.zero;
              sp=Register.zero;
              bp=Register.zero;
              si=Register.zero;
              di=Register.zero}


  let set label v t =
    match label with
    | Registerlabel.A part -> {t with a = Register.set ~part v t.a}
    | Registerlabel.B part -> {t with b = Register.set ~part v t.b}
    | Registerlabel.C part -> {t with c = Register.set ~part v t.c}
    | Registerlabel.D part -> {t with d = Register.set ~part v t.d}
    | Registerlabel.SP -> {t with sp = Register.set ~part:`X v t.sp }
    | Registerlabel.BP -> {t with bp = Register.set ~part:`X v t.bp }
    | Registerlabel.SI -> {t with si = Register.set ~part:`X v t.si }
    | Registerlabel.DI -> {t with di = Register.set ~part:`X v t.di }

  let get label t =
    match label with
    | Registerlabel.A part -> Register.get t.a ~part
    | Registerlabel.B part -> Register.get t.b ~part
    | Registerlabel.C part -> Register.get t.c ~part
    | Registerlabel.D part -> Register.get t.d ~part
    | Registerlabel.SP -> Register.get t.sp ~part:`X
    | Registerlabel.BP -> Register.get t.bp ~part:`X
    | Registerlabel.SI -> Register.get t.si ~part:`X
    | Registerlabel.DI -> Register.get t.di ~part:`X

  let diff r1 r2 : diff =
    let aux label =
      let v1 = get label r1 in
      let v2 = get label r2 in
      if v1 <> v2 then [label,v1,v2] else []
    in
    aux (A `X) @
    aux (B `X) @
    aux (C `X) @
    aux (D `X) @
    aux SP @
    aux BP @
    aux SI @
    aux DI

  let diff_to_string d =
    List.map (fun (l,v1,v2) -> Printf.sprintf "%2s:%s->%s"
                 (Registerlabel.to_string l)
                 (Register.to_string v1)
                 (Register.to_string v2)) d
    |> String.concat "||"

  let print t =
    Printf.printf "a:  %s\n" @@ Register.to_string t.a;
    Printf.printf "b:  %s\n" @@ Register.to_string t.b;
    Printf.printf "c:  %s\n" @@ Register.to_string t.c;
    Printf.printf "d:  %s\n" @@ Register.to_string t.d;
    Printf.printf "sp: %s\n" @@ Register.to_string t.sp;
    Printf.printf "bp: %s\n" @@ Register.to_string t.bp;
    Printf.printf "si: %s\n" @@ Register.to_string t.si;
    Printf.printf "di: %s\n" @@ Register.to_string t.di;
end

module CPU = struct
  type register = int
  type t = {
    inst: Inst.t Seq.t;
    registers: Registers.t;
  }

  let start stream = {
    inst=Inst.of_stream stream;
    registers=Registers.zero
  }

  let diff_state p1 p2 =
    Registers.diff p1.registers p2.registers

  let handle instruction t =
    match instruction with
    | Inst.Mov {dst=Inst.Register dst;src = Inst.Register src} ->
      let value = Registers.get src t.registers in
      {t with registers=Registers.set dst value t.registers}
    | Inst.Mov {dst=Inst.Register dst;src = Inst.Immediate (v,_)} ->
      {t with registers=Registers.set dst v t.registers}
    | _ -> failwith "not imeplemented"
end
