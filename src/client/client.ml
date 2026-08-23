module Tile = Game.Tile

module State = struct
  type screen = Main

  type t = {
    layout      : Tile.layout ;
    perspective : Game.Color.t ;
    screen      : screen
  }
end

let draw (cs : State.t) : unit = 
  let open Raylib in
  let lsx, lsy = cs.layout.size.x, cs.layout.size.y in
  
  let tile_color = ref Game.Color.Black in
  for x = 0 to 7 do
    tile_color := Game.Color.flip !tile_color;
    for y = 0 to 7 do
      let x, y = Tile.Coord.to_pixel { x ; y } cs.layout in
      draw_rectangle x y lsx lsy (Helpers.Color_ext.get_color !tile_color);
      tile_color := Game.Color.flip !tile_color
    done;
  done

let init =
  State.{
    layout      = Tile.{ size = { x = 80 ; y = 80 } ; origin = { x = 40 ; y = 40 } } ;
    perspective = White ;
    screen      = Main
  }
