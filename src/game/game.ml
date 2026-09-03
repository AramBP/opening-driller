open Core

exception InvalidCastle of string
exception EmptySquare of string

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

let get_owner (gs : State.t) (idx : int) : color option =
  match Board.get_piece gs.board idx with
  | None -> None
  | Some (_, color) -> Some color 

let get_piece (gs : State.t) (idx : int) (color : color) : piece option =
  match Board.get_piece gs.board idx with
  | None -> None
  | Some (kind, color') ->
      if Board.equal_color color color' then
        Some (kind, color')
      else 
        None

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

let apply_move (gs : State.t) (src : int) (dest : int) : State.t = 
  match Board.get_piece gs.board src with
  | None -> raise (EmptySquare (idx_to_coord src))
  | Some piece ->
      let kind, color = piece in
      (* Update castling rights *)
      let white_king = 
        if gs.rights.white_king then not (Board.equal_piece piece (King, White)) && not (src = 7)
        else false
      in
      let white_queen = 
        if gs.rights.white_queen then not (Board.equal_piece piece (King, White)) && not (src = 0)
        else false
      in
      let black_king = 
        if gs.rights.black_king then not (Board.equal_piece piece (King, Black)) && not (src = 63)
        else false
      in
      let black_queen = 
        if gs.rights.black_queen then not (Board.equal_piece piece (King, Black)) && not (src = 56)
        else false
      in

      (* set en passant target square if pawn advances two spaces *)
      let en_passant = 
        if Board.equal_kind kind Pawn && abs(src - dest) = 16 then Some ((src + dest) / 2)
        else None
      in
      
      (* reset half move counter after captures or a pawn is moved *)
      let n_half_moves =
        if Board.equal_kind kind Pawn || Option.is_some (Board.get_piece gs.board dest) then 0
        else gs.n_half_moves
      in

      let n_moves = 
        if Board.equal_color gs.player Black then gs.n_moves + 1 
        else gs.n_moves
      in

      let board = Board.move_piece gs.board src dest in

      (* move rook to other side in case of castle *)
      let castle_type = 
        if dest = 62 then white_king
        else if dest = 58 then white_queen
        else if dest = 6 then black_king
        else if dest = 2 then black_queen
        else false
      in

      let board = 
        if Board.equal_kind kind King && castle_type then
          let rook_src, rook_dest = 
            if dest = 62 then 63, 61
            else if dest = 58 then 56, 59
            else if dest = 6 then 7, 5
            else if dest = 2 then 56, 59
            else raise (InvalidCastle ("From " ^ idx_to_coord src ^ " to " ^ idx_to_coord dest))
          in
          Board.move_piece board rook_src rook_dest
        else 
          board
      in

      (* In case of en passant remove the captured pawn *)
      let board =
        let is_en_passant = (match gs.en_passant with None -> false | Some x -> dest = x) in
        if Board.equal_kind kind Pawn && is_en_passant then
          if Board.equal_color color White then
            Board.move_piece board (dest - 8) (dest - 8)
          else
            Board.move_piece board (dest + 8) (dest + 8)
        else
          board
      in

      let player = Board.flip_color gs.player in
      
      { 
        board = board ; 
        player = player ; 
        n_moves = n_moves ; 
        n_half_moves = n_half_moves ; 
        en_passant = en_passant ; 
        rights = { white_king ; white_queen ; black_king ; black_queen } 
      }
  
let trace_ray (gs : State.t) (src : int) (ray : int list) (player : color) : int list =
  match get_piece gs src player with
  | None -> raise (EmptySquare (idx_to_coord src))
  | Some piece ->
    let is_occupied x = match get_owner gs x with None -> false | Some _ -> true in
    List.fold_until ray ~init:[] ~finish:(fun acc -> acc) ~f:(fun acc x ->
      let del_x = abs (x - src) % 8 in
      let target_owner = get_owner gs x in
      let cond = match target_owner with None -> false | Some color -> Board.equal_color color player in
      if cond then Stop acc
      else
        let moves = match piece with 
          | Pawn, piece_color ->
              let src_row = src / 8 in 
              (* Pawn can only move forward if the square is not occupied *)
              if del_x = 0 && is_occupied x then None
              (* Handle first move rule for pawns *)
              else if Board.equal_color piece_color White && src_row = 6 && del_x = 0 then 
                let x' = x - 8 in
                if is_occupied x' then Some [x]
                else Some [x ; x']
              else if Board.equal_color piece_color Black && src_row = 1 && del_x = 0 then
                let x' = x + 8 in
                if is_occupied x' then Some [x]
                else Some [x ; x'] 
              (* Test en passant exception *)
              else if not (del_x = 0) && not (is_occupied x) then
                if (match gs.en_passant with None -> false | Some dest -> dest = x) then
                  Some [x]
                else 
                  Some []
              else 
                Some [x]
          (*Test for castling exception king *)
          | King, _ when del_x = 2 -> 
              let gap = src + x / 2 in
              let out = x - 1 in
              let rights = 
                if x = 62 then gs.rights.white_king 
                else if x = 58 then gs.rights.white_queen
                else if x = 6 then gs.rights.black_king
                else if x = 2 then gs.rights.black_queen
                else false
              in
              if is_occupied gap || is_occupied x || not rights || ((x = 2 || x = 58) && is_occupied out) then
                None
              else
                Some [x]
          | _ -> Some [x]
        in 
        if Option.is_none moves then Stop acc
        else if Option.is_some target_owner then Stop (List.append (Option.value_exn moves) acc)
        else Continue (List.append (Option.value_exn moves) acc)
    )

(* Get a list containing all reachable moves for the specified piece owned by the specified player *)
let all_moves_piece (gs : State.t) (idx : int) (player : color) : int list = 
  match get_piece gs idx player with
      | None -> []
      | Some piece ->
          let rays = List.Assoc.find_exn Moves.moves.(idx) piece ~equal:Board.equal_piece in
          List.(join (Array.map rays ~f:(fun ray -> trace_ray gs idx ray player) |> of_array))

(* Get a list containing all reachables moves for pieces owned by the specified player *)
let all_moves (gs : State.t) (player : color) : int list = List.(init 64 ~f:(fun src -> all_moves_piece gs src player) |> join)

(* Get a list containing all legal moves for the piece at the specified index *)
let get_moves (gs : State.t) (src : int) (player : color) : int list = 
  match get_piece gs src player with
  | None -> raise (EmptySquare (idx_to_coord src))
  | Some (kind, color) -> 
      let all_moves_piece = all_moves_piece gs src player in
      List.fold_until all_moves_piece ~init:[] ~finish:(fun acc -> acc) ~f:(fun acc dest -> 
        let opponent = Board.flip_color player in
        (*Dont allow castle through check *)
        let legal_castle = match kind with
          | King ->
              let king_loc = Option.value_exn (Board.find_piece gs.board (kind, color)) in
              let dx = abs (king_loc - dest) in
              if src = king_loc && dx = 2 then 
                let moves_opponent = all_moves gs opponent in
                let castle_gap = 
                  if dest = 62 then 61
                  else if dest = 58 then 59
                  else if dest = 6 then 5
                  else if dest = 2 then 3
                  else raise (InvalidCastle ("From " ^ idx_to_coord src ^ " to " ^ idx_to_coord dest))
                in
                not (List.mem moves_opponent king_loc ~equal:Int.equal || List.mem moves_opponent castle_gap ~equal:Int.equal)
              else
                true
          | _ -> true 
        in
        if legal_castle then
          let test_board = apply_move gs src dest in 
          let moves_opponent = all_moves test_board opponent in
          let king_loc = Option.value_exn (Board.find_piece test_board.board (King, player)) in
          if List.mem moves_opponent king_loc ~equal:Int.equal then Continue acc
          else Continue (dest::acc)
        else
          Continue acc
      )
    

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

