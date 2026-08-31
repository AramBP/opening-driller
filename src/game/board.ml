open Core

type kind = Pawn | Knight | Bishop | Rook | Queen | King [@@deriving eq]
type color = White | Black [@@deriving eq]
type piece = kind * color [@@deriving eq]

type t = piece option Array.t

let get_piece (board : t) (idx : int) : piece option = board.(idx)

let find_piece (board : t) (piece : piece) : int option =
  Array.find_mapi board 
    ~f:(fun i -> function
      | None -> None
      | Some piece' -> 
          if equal_piece piece piece' then Some i
          else None)

let move_piece (board : t) (src : int) (dest : int) : t =
  match get_piece board src with 
  | None -> board
  | Some piece -> Array.(set board src None; set board dest (Some piece)); board

let to_string (board : t) : string =
  let piece_to_str = function
    | Pawn, White -> "P" | Pawn, Black -> "p"
    | Knight, White -> "N" | Knight, Black -> "n"
    | Bishop, White -> "B" | Bishop, Black -> "b"
    | Rook, White -> "R" | Rook, Black -> "r"
    | Queen, White -> "Q" | Queen, Black -> "q"
    | King, White -> "K" | King, Black -> "k"
  in
  let n_empty, res =
    Array.foldi board ~init:(0, "") ~f:(fun idx (n_empty, res) piece_opt ->
      (* starting a new rank: flush the previous rank's trailing empties, add '/' *)
      let n_empty, res =
        if idx > 0 && idx mod 8 = 0 then
          let flushed = if n_empty > 0 then Int.to_string n_empty else "" in
          (0, res ^ flushed ^ "/")
        else (n_empty, res)
      in
      match piece_opt with
      | None -> (n_empty + 1, res)
      | Some piece ->
          let flushed = if n_empty > 0 then Int.to_string n_empty else "" in
          (0, res ^ flushed ^ piece_to_str piece)
    )
  in
  (* flush trailing empties on the last rank *)
  res ^ (if n_empty > 0 then Int.to_string n_empty else "")

let of_string (fen_string : string) : t =
  let char_to_piece = function
    | 'P' -> Pawn, White | 'p' -> Pawn, Black
    | 'N' -> Knight, White | 'n' -> Knight, Black
    | 'B' -> Bishop, White | 'b' -> Bishop, Black
    | 'R' -> Rook, White | 'r' -> Rook, Black
    | 'Q' -> Queen, White | 'q' -> Queen, Black
    | 'K' -> King, White | 'k' -> King, Black 
    | c -> failwith ("Undefined piece : " ^ (String.of_char c))
  in
  let board = Array.init ~f:(fun _ -> None) 64 in
  let i = ref 0 in
  String.iter fen_string ~f:(fun c -> 
    if Char.equal c '/' then ()
    else
      match Char.get_digit c with
      | Some n -> i := !i + n
      | None -> board.(!i) <- Some (char_to_piece c); i := !i + 1
  );
  board
