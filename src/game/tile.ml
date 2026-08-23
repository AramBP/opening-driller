type vec2 = { x : int ; y : int }
type layout = { origin : vec2 ; size : vec2 }

module Coord = struct
  type t = vec2

  let equal a b = Int.equal a.x b.x && Int.equal a.y b.y
  
  let of_pixel x y layout =
    {
      x = Int.of_float (x -. Float.of_int layout.origin.x) / layout.size.x ; 
      y = Int.of_float (y -. Float.of_int layout.origin.y) / layout.size.y 
    }
  
  (* [to_pixel coord layout] returns the the top most pixel (x, y) of the tile *)
  let to_pixel coord layout = (coord.x * layout.size.x + layout.origin.x, coord.y * layout.size.y + layout.origin.y)
  let in_board coord = coord.x > 0 && coord.x < 8 && coord.y > 0 && coord.y < 8
  let to_string coord =
    let file = match coord.x with 0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d" | 4 -> "e" | 5 -> "f" | 6 -> "g" | 7 -> "h" | x -> Int.to_string x in
    let rank = Int.to_string (coord.y + 1) in 
    file ^ rank
end
