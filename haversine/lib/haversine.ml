
let square x = x *. x

let to_radians =
  let c = Float.pi /. 180. in
  fun deg -> c *. deg

let referenceHanversine x0 y0 x1 y1 r =
  let dy = to_radians (y1 -. y0) in
  let dx = to_radians (x1 -. x0) in
  let y0 = to_radians y0 in
  let y1 = to_radians y1 in
  r *. Float.(square (sin (dy /. 2.)) +. cos y0 *. cos y1 *. square (sin (dx /. 2.)))
