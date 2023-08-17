let player_x = 1
let player_o = -1
let no_player = 0
let board_size = 9

let rec board_new size fill_value =
  match size with 0 -> [] | x -> fill_value :: board_new (x - 1) fill_value

let convert_to_player i =
  match i with _ when i = player_x -> "X" | _ when i = player_o -> "O" | _ -> "-"

let board_print board =
  let out = String.concat " " (List.map convert_to_player board) in
  Printf.printf "Board: [%s]\n" out

let board_move (board : int list) (move : int) (player : int) : int list =
  List.mapi (fun i el -> if i = move then player else el) board

let get_user_move player =
  let move = ref board_size in
  while !move = board_size do
    let player_str = if player == player_o then "O" else "X" in
    Printf.printf "Enter move (player %s): \n" player_str;
    let user_move = read_int_opt () in
    match user_move with
    | Some x when x >= 0 -> move := x
    | _ -> Printf.printf "Invalid move. Move should be an int in range [0; %d)\n%!" board_size
  done;
  !move

let make_user_move board player =
  let user_move = get_user_move player in
  board_move board user_move player

let () =
  let board = ref [] in
  board := board_new 9 no_player;
  board_print !board;
  print_newline ();

  let player = ref player_x in
  let quit_loop = ref false in
  while not !quit_loop do
    board := make_user_move !board !player;
    player := !player * -1;
    board_print !board
  done
