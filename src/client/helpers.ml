module Color_ext = struct
  let white_square = Raylib.Color.create 240 217 181 255 
  let black_square = Raylib.Color.create 181 136 99 255 

  let get_color (color : Game.Color.t) : Raylib.Color.t =
    match color with White -> white_square | Black -> black_square
end
