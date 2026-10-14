open Core

module Button = Helpers.Button
module type ScrollItem = Helpers.ScrollItem
module ScrollableBox = Helpers.ScrollableBox
type color = Game.color

module MoveItem : sig
  include ScrollItem
  val make : string  -> int -> int -> t
end = struct
  type content = { label : string ; color : Raylib.Color.t }
  type t = { content : content ; width : int ; height : int }

  let draw (item : t) (x : int) (y : int) : unit =
    let open Raylib in
    draw_rectangle x y item.width item.height Color.black;
    draw_rectangle_lines x y item.width item.height Color.white;
    draw_text item.content.label (x + 8) (y + 8) 10 item.content.color

  let make (label : string) (width : int) (height : int) : t =
    let color = Raylib.Color.white in
    let content = { label ; color } in
    { content = content ; width = width ; height = height}
end
module MoveHist = ScrollableBox (MoveItem)

module Input = struct
  type t = 
    | Empty
    | Drag_and_drop of Tile.Coord.t 
    | Normal of Tile.Coord.t
    | Move of (Tile.Coord.t * Tile.Coord.t)
end

module State = struct
  type button_kind = Flip
  type t = {
    input           : Input.t ;
    layout          : Tile.layout ;
    buttons         : button_kind Button.t list ;
    perspective     : color ;
    move_history    : MoveHist.t
  }
end

let create_move_item (gs : Game.State.t) (src : int) (dest : int) : MoveItem.t =
  let label = Game.move_to_string gs src dest in  
  MoveItem.make label 240 30

let board_hover (mx : int) (my : int) (cs : State.t) =
  (cs.layout.origin.x + cs.layout.size.x * 8) >= mx && mx >= cs.layout.origin.x &&
  (cs.layout.origin.y + cs.layout.size.y * 8) >= my && my >= cs.layout.origin.y

let select_tile (mx : int) (my : int) (cs : State.t) ~(cond : bool) : Tile.Coord.t option =
  if (board_hover mx my cs) && cond then
    Some (Tile.Coord.of_pixel mx my cs.layout)
  else
    None

let get_input (cs : State.t) (input : Input.t) : Input.t =
  let mx, my = Raylib.(get_mouse_x (), get_mouse_y ()) in
  match input with
  | Empty -> 
      let src = select_tile mx my cs ~cond:(Raylib.(is_mouse_button_down MouseButton.Left)) in
      if Option.is_some src then 
        Drag_and_drop (Option.value_exn src) 
      else
        Empty
  | Drag_and_drop src ->
      let dest = select_tile mx my cs ~cond:(Raylib.(is_mouse_button_released MouseButton.Left)) in
      if Option.is_some dest then
        let dest = Option.value_exn dest in
        if Tile.equal_vec2 src dest then
          Normal src
        else
          Move (src, dest)
      else
        Drag_and_drop src
  | Normal src ->
      let dest = select_tile mx my cs ~cond:(Raylib.(is_mouse_button_pressed MouseButton.Left)) in
      if Option.is_some dest then
        let dest = Option.value_exn dest in
        if Tile.equal_vec2 src dest then
          Empty
        else
          Move (src, dest)
      else
        Normal src
  | Move _ -> Empty

 
let get_tile (tile : Tile.Coord.t) (cs : State.t) : Tile.Coord.t =
  let z = match cs.perspective with White -> 0 | Black -> 7 in
  { x = abs (z - tile.x) ; y = abs (z - tile.y) }
       
let next (cs : State.t) (gs : Game.State.t) : (State.t * Game.State.t) =
  let open Raylib in
  let mx, my = (get_mouse_x (), get_mouse_y ()) in
  
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

  let input = get_input cs cs.input in

  (* Update the game state and if there is a valid move the move history as well *)
  let gs, move_history = 
    match input with
    | Move (src, dest) ->
        let src_idx = Tile.Coord.to_idx (get_tile src cs) in
        let dest_idx = Tile.Coord.to_idx (get_tile dest cs) in
        if Option.is_some (Game.get_piece gs src_idx gs.player) then 
          let moves = Game.get_moves gs src_idx gs.player in
          if List.mem moves dest_idx ~equal:Int.equal then
            let next_gs = Game.next_state gs src_idx dest_idx in
            let move_item = create_move_item gs src_idx dest_idx in
            next_gs, (MoveHist.add_item cs.move_history move_item)
          else
            gs, cs.move_history
        else
          gs, cs.move_history
    | _ -> gs, cs.move_history
  in 

  let move_history = MoveHist.update move_history in
   
  { cs with perspective = perspective ; buttons = buttons ; input = input ; move_history = move_history }, gs

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
 
  (match cs.input with
    | Empty | Move _ -> ()
    | Normal tile -> 
        let idx = Tile.Coord.to_idx (get_tile tile cs) in
        (match Game.get_piece gs idx gs.player with
        | None -> ()
        | Some piece ->
            let tex = Helpers.get_piece_tex piece in
            let x_px, y_px = Tile.Coord.to_pixel tile cs.layout in
            draw_rectangle x_px y_px cs.layout.size.x cs.layout.size.y Helpers.transparent_dark_green;
            draw_texture tex x_px y_px Color.white; 
            
            let moves = Game.get_moves gs idx gs.player in 
            List.iter moves ~f:(fun idx -> 
              let tile = get_tile (Tile.Coord.of_idx idx) cs in
              let x_px, y_px = Tile.Coord.to_pixel tile cs.layout in
              draw_circle (x_px + cs.layout.size.x/2) (y_px + cs.layout.size.y/2) (Float.of_int (cs.layout.size.x) *. 0.15) Helpers.transparent_dark_green
            );)
    | Drag_and_drop tile ->
        let idx = Tile.Coord.to_idx (get_tile tile cs) in
        match Game.get_piece gs idx gs.player with
        | None -> ()
        | Some piece ->
            let tex = Helpers.get_piece_tex piece in
            let x_px, y_px = Tile.Coord.to_pixel tile cs.layout in
            draw_rectangle x_px y_px cs.layout.size.x cs.layout.size.y Helpers.transparent_dark_green;
            
            let mx, my = (get_mouse_x (), get_mouse_y ()) in
            let hover_tile = get_tile (Tile.Coord.of_pixel mx my cs.layout) cs in

            let moves = Game.get_moves gs idx gs.player in 
            List.iter moves ~f:(fun idx -> 
              let tile = get_tile (Tile.Coord.of_idx idx) cs in
              let x_px, y_px = Tile.Coord.to_pixel tile cs.layout in
              if Tile.equal_vec2 tile hover_tile then
                draw_rectangle x_px y_px cs.layout.size.x cs.layout.size.y Helpers.transparent_dark_green
              else 
                draw_circle (x_px + cs.layout.size.x/2) (y_px + cs.layout.size.y/2) (Float.of_int (cs.layout.size.x) *. 0.15) Helpers.transparent_dark_green
            );
            draw_texture tex (mx - Texture.width tex / 2) (my - Texture.height tex / 2) Color.white);

    (* Draw move history *)
    MoveHist.draw (cs.move_history)
       
let init () =
  let flip_button = Button.create 800. 600. 100. 50. 0.5 "Flip" Center Helpers.gray Helpers.light_gray State.Flip in
  let move_history = MoveHist.init 800 20 280 400 Raylib.Color.white in
  State.{
    input = Empty ;
    layout        = Tile.{ size = { x = 80 ; y = 80 } ; origin = { x = 40 ; y = 40 } } ;
    perspective   = White ;
    buttons       = [flip_button] ;
    move_history  = move_history
  }
