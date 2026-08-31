open Core

exception InvalidMove of string

type color = Board.color
let flip_color = Board.(function White -> Black | Black -> White) 

type piece = Board.piece


module State = struct
  type status = Check | Checkmate | Stalemate
  type castling_rights = { white_king : bool ; white_queen : bool ; black_king : bool ; black_queen : bool }
  type state_kind = { player : Board.color ; }
  type t = { 
    board : Board.t ; 
    player : color ; 
    rights : castling_rights ; 
    en_passant : int option ;  
    n_half_moves : int ;
    n_moves : int
  }
end

let coord_to_idx (coord : string) : int =
  let on_fail = "Failed to parse coordinate : " ^ coord in
  if not (String.length coord = 2) then failwith on_fail;
  
  let rank = String.get coord 0 in
  let row = String.get coord 1 in
  match rank, row with
    | 'a'..'h', '1'..'8' -> Char.(to_int rank - to_int 'a') + Char.(to_int row - to_int '1') * 8
    | _ -> failwith on_fail

let idx_to_coord (idx : int) : string =
  let file = idx mod 8 in
  let row = idx / 8 in
  let rank = Char.of_int_exn (Char.to_int 'a' + file) in
  let row_char = Char.of_int_exn (Char.to_int '0' + row) in
  String.of_char_list [rank; row_char]

let trace_ray (gs : State.t) (src : int) (ray : int list) (player : color) : int list =
  match Board.get_piece gs.board src with
  | None -> failwith ("Empty square " ^ idx_to_coord src)
  | Some _ ->
    List.filter ray ~f:(fun dest -> 
      match Board.get_piece gs.board dest with
      | None -> true
      | Some (_, color) -> not (Board.equal_color player color)
    ) 

let all_moves (gs : State.t) (player : color) : int list =
  List.(
    init 64 ~f:(fun src -> 
      match Board.get_piece gs.board src with
      | None -> []
      | Some piece ->
          let _, color = piece in
          match Board.equal_color player color with
          | false -> []
          | true -> 
            let rays = List.Assoc.find_exn Moves.moves.(src) piece ~equal:Board.equal_piece in
            (join (Array.map rays ~f:(fun ray -> trace_ray gs src ray player) |> of_array))
    ) 
    |> join)

let of_string (fen_string : string) : State.t =
  let on_fail = "Failed to parse FEN encoded string : \n"^ fen_string in
  
  let fen_arr = String.split fen_string ~on:' ' |> Array.of_list in
  if not (Array.length fen_arr = 6) then failwith on_fail;

  let board = Board.of_string fen_arr.(0) in
  Printf.printf "%s\n" (Board.to_string board); Out_channel.(flush stdout);
  let player = 
    match fen_arr.(1) with 
    | "w" -> Board.White | "b" -> Board.Black 
    | _ -> failwith on_fail
  in
  let rights = State.{
      white_king = String.contains fen_arr.(2) 'K' ;
      white_queen = String.contains fen_arr.(2) 'Q' ;
      black_king = String.contains fen_arr.(2) 'k' ;
      black_queen = String.contains fen_arr.(2) 'q' 
    }
  in
  let en_passant = 
    match fen_arr.(3) with
    | "-" -> None
    | coord -> Some (coord_to_idx coord)
  in
  let n_half_moves = 
    match Int.of_string_opt fen_arr.(4) with
    | Some x -> x
    | None -> failwith on_fail
  in
  let n_moves = 
    match Int.of_string_opt fen_arr.(5) with
    | Some y -> y
    | None -> failwith on_fail
  in
  
  { board ; player ; rights ; en_passant ; n_half_moves ; n_moves }

let to_string (gs : State.t) : string =
  let board_str = Board.to_string gs.board in
  let player_str = match gs.player with
    | Board.White -> "w"
    | Board.Black -> "b"
  in
  let rights_str =
    let r = gs.rights in
    let s =
      (if r.white_king then "K" else "")
      ^ (if r.white_queen then "Q" else "")
      ^ (if r.black_king then "k" else "")
      ^ (if r.black_queen then "q" else "")
    in
    if String.is_empty s then "-" else s
  in
  let en_passant_str = match gs.en_passant with
    | None -> "-"
    | Some idx -> idx_to_coord idx
  in
  String.concat ~sep:" " [
    board_str;
    player_str;
    rights_str;
    en_passant_str;
    Int.to_string gs.n_half_moves;
    Int.to_string gs.n_moves;
  ]

let init =
  let default_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  of_string default_fen

