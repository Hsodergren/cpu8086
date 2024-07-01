type reg_size = [ `X | `L | `H ]
type t = A of reg_size | C of reg_size | D of reg_size | B of reg_size | SP | BP | SI | DI
         

let add_size buf = function
  | `L -> Buffer.add_char buf 'l'
  | `H -> Buffer.add_char buf 'h'
  | `X -> Buffer.add_char buf 'x'
           
let add_reg buf = function
  | A s -> Buffer.add_char buf 'a'; add_size buf s
  | B s -> Buffer.add_char buf 'b'; add_size buf s
  | C s -> Buffer.add_char buf 'c'; add_size buf s
  | D s -> Buffer.add_char buf 'd'; add_size buf s
  | SP -> Buffer.add_string buf "sp"
  | BP -> Buffer.add_string buf "bp"
  | SI -> Buffer.add_string buf "si"
  | DI -> Buffer.add_string buf "di"

let to_string t =
  let buf = Buffer.create 0 in
  add_reg buf t;
  Buffer.to_bytes buf |> String.of_bytes
  
