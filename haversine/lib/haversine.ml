module Json = Json

module Point = struct
  type t = float * float
end

module Line = struct
  type t = {
      p1: Point.t;
      p2: Point.t;
    }

  let v p1 p2 = {p1;p2}

  let to_yojson {p1=(x0,y0);p2=(x1,y1)} =
    `Assoc ["x0",`Float x0;"y0",` Float y0;"x1",`Float x1; "y1", `Float y1]
  let square x = x *. x
                 
  let to_radians =
    let c = Float.pi /. 180. in
    fun deg -> c *. deg

  let refHaversine x0 y0 x1 y1 r =
    let dy = to_radians (y1 -. y0) in
    let dx = to_radians (x1 -. x0) in
    let y0 = to_radians y0 in
    let y1 = to_radians y1 in
    let a = Float.(square (sin (dy /. 2.)) +. cos y0 *. cos y1 *. square (sin (dx /. 2.))) in
    let c = 2. *. Float.asin (Float.sqrt a) in
    r *. c

  let haversine {p1=(x0,y0); p2=(x1,y1)} =
    refHaversine x0 y0 x1 y1 6378.1
end
