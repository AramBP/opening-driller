type vec2 = { x : int ; y : int } [@@deriving eq]
type layout = { origin : vec2 ; size : vec2 }

module Coord = struct
  type t = vec2
  
  let of_pixel x y layout =
    {
      x = (x - layout.origin.x) / layout.size.x ; 
      y = (y - layout.origin.y) / layout.size.y 
    }
  
  (* [to_pixel coord layout] returns the the top most pixel (x, y) of the tile *)
  let to_pixel coord layout = (coord.x * layout.size.x + layout.origin.x, coord.y * layout.size.y + layout.origin.y)

  let of_idx (idx : int) : t =
    let file = idx mod 8 in
    let row = idx / 8 in
    { x = file ; y = row }

  let to_idx (coord : t) : int = coord.x + coord.y * 8 

  let to_string coord =
    let file = match coord.x with 0 -> "a" | 1 -> "b" | 2 -> "c" | 3 -> "d" | 4 -> "e" | 5 -> "f" | 6 -> "g" | 7 -> "h" | x -> Int.to_string x in
    let rank = Int.to_string (coord.y + 1) in 
    file ^ rank
end
