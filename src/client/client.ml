open Core

module Button = Helpers.Button
type color = Game.color

module State = struct
  type button_kind = Flip
  type state_kind = Normal | Drag_and_drop
  type t = {
    selected_tile   : Tile.Coord.t option ;
    state           : state_kind ;
    layout          : Tile.layout ;
    buttons         : button_kind Button.t list ;
    perspective     : color
  }
end

let get_tile (tile : Tile.Coord.t) (cs : State.t) : Tile.Coord.t =
  let z = match cs.perspective with White -> 0 | Black -> 7 in
  { x = abs (z - tile.x) ; y = abs (z - tile.y) }

let board_hover (mx : int) (my : int) (cs : State.t) =
  (cs.layout.origin.x + cs.layout.size.x * 8) >= mx && mx >= cs.layout.origin.x &&
  (cs.layout.origin.y + cs.layout.size.y * 8) >= my && my >= cs.layout.origin.y

let select_tile (mx : int) (my : int) (cs : State.t) ~(cond : bool) : Tile.Coord.t option =
  if (board_hover mx my cs) && cond then
    Some (Tile.Coord.of_pixel mx my cs.layout)
  else
    None
       
let next (cs : State.t) : State.t =
  let open Raylib in
  let mx, my = Raylib.(get_mouse_x (), get_mouse_y ()) in
  
  let cursor = 
    if (board_hover mx my cs) then MouseCursor.Pointing_hand
    else MouseCursor.Arrow 
  in
  set_mouse_cursor cursor;

  let buttons = List.map cs.buttons ~f:(fun button -> Button.next mx my button) in
  let perspective = 
    let res = match is_mouse_button_pressed MouseButton.Left with 
    | true -> Button.on_click buttons (function Flip -> Game.flip_color cs.perspective)
    | false -> None
    in
    match res with Some color -> color | None -> cs.perspective
  in

  let selected_tile = 
    if is_mouse_button_pressed MouseButton.Left then 
      select_tile mx my cs ~cond:(true)
    else
      cs.selected_tile
  in
   
  { cs with perspective = perspective ; buttons = buttons ; selected_tile = selected_tile}

let draw (cs : State.t) (gs : Game.State.t) : unit = 
  let open Raylib in

  List.iter cs.buttons ~f:Button.draw;

  (* Draw chess board *)
  let lsx, lsy = cs.layout.size.x, cs.layout.size.y in
  let tile_color = ref Helpers.black_square in
  let flip_tile_color color = Helpers.(
    if color_is_equal color white_square then
      black_square
    else 
      white_square)
  in
  for x = 0 to 7 do
    tile_color := flip_tile_color !tile_color;
    for y = 0 to 7 do
      let x_px, y_px = Tile.Coord.to_pixel { x ; y } cs.layout in
      draw_rectangle x_px y_px lsx lsy !tile_color;
      tile_color := flip_tile_color !tile_color;
      if x = 7 then begin
        let y = abs ((match cs.perspective with White -> 7 | Black -> 0) - y) in
        draw_text_ex
          (Lazy.force Helpers.font_bold)
          String.(get (Tile.Coord.to_string { x ; y }) 1 |> of_char)
          (Vector2.create (x_px + lsx - 15 |> Float.of_int) (y_px + 7 |> Float.of_int))
          20. 2. !tile_color;
      end;
      if y = 7 then begin
        let x = abs ((match cs.perspective with White -> 0 | Black -> 7) - x) in
        draw_text_ex
          (Lazy.force Helpers.font_bold)
          String.(get (Tile.Coord.to_string { x ; y }) 0 |> of_char)
          (Vector2.create (x_px + 5 |> Float.of_int) (y_px + lsy - 20 |> Float.of_int))
          20. 2. !tile_color;
      end;
    done;
  done;

  (* Draw pieces *)
  Array.iteri gs.board ~f:(fun i piece_opt ->
    match piece_opt with
    | None -> ()
    | Some piece ->
      let coord = Tile.Coord.of_idx i in
      let tile = get_tile coord cs in
      let tex = Helpers.get_piece_tex piece in
      let x_px, y_px = Tile.Coord.to_pixel tile cs.layout in
      draw_texture tex x_px y_px Color.white
  );

  let moves = Game.all_moves gs cs.perspective in 
  
  List.iter moves ~f:(fun idx -> 
    let tile = Tile.Coord.of_idx idx in
    let x_px, y_px = Tile.Coord.to_pixel tile cs.layout in
    draw_circle (x_px + cs.layout.size.x/2) (y_px + cs.layout.size.y/2) (Float.of_int (cs.layout.size.x) *. 0.25) Helpers.transparent_dark_green
  );

  match cs.selected_tile with
  | None -> ()
  | Some tile -> 
      let idx = Tile.Coord.to_idx (get_tile tile cs) in
      match Game.get_piece gs idx gs.player with
      | None -> ()
      | Some piece ->
          let tex = Helpers.get_piece_tex piece in
          let x_px, y_px = Tile.Coord.to_pixel tile cs.layout in
          draw_rectangle x_px y_px cs.layout.size.x cs.layout.size.y Helpers.transparent_dark_green;
          draw_texture tex x_px y_px Color.white
     
let init =
  let flip_button = Button.create 800. 600. 100. 50. 0.5 "Flip" Center Helpers.gray Helpers.light_gray State.Flip in
  State.{
    selected_tile = None ;
    state         = Normal ;
    layout        = Tile.{ size = { x = 80 ; y = 80 } ; origin = { x = 40 ; y = 40 } } ;
    perspective   = White ;
    buttons       = [flip_button]
  }
