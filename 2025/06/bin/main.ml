(* AoC 2025 Day 06 (part 2) *)

let input_path =
  if Array.length Sys.argv = 2 then
    Sys.argv.(1)
  else begin
    Printf.eprintf "error: wrong number of arguments (%d)\nusage: %s INPUT\n" (Array.length Sys.argv) Sys.argv.(0);
    exit 2
  end
    
let column i grid =
  List.map (fun row -> List.nth row i) grid

type column_result = Empty | Number of int | Terminal of int * char

(* returns a number read from top to bottom, plus optionally any operator found on the botton *)
(* note: last line must be either a space or an operator because that's how the file is formatted *)
let process_column col =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let to_int c = Char.code c - Char.code '0' in
  let rec aux acc input =
    match acc, input with
      | _, ' ' :: rest -> aux acc rest
      | Some old, c :: rest when is_digit c -> aux (Some (old * 10 + to_int c)) rest
      | None, c :: rest when is_digit c -> aux (Some (to_int c)) rest
      | Some num, [c] when c = '*' || c = '+' -> Terminal (num, c)
      | None, [c] when c = '*' || c = '+' -> failwith "error: empty column before operator"
      | _, c :: _ -> failwith (Printf.sprintf "error: invalid character '%c' in column" c)
      | Some num, []  -> Number num
      | None, [] -> Empty in
  aux None col

let do_column_arith grid = 
  let rec process_columns j current tally =
    match j with
      | n when n < 0 -> tally
      | _ ->
          let next = j - 1 in
          match (column j grid) |> process_column with
            | Terminal (num, '*') ->
              let list = num :: current in
              let sub_total = List.fold_left ( * ) 1 list in
              Printf.printf "%s = %d\n" (list |> List.map (fun n -> Printf.sprintf "%d" n) |> String.concat " * ") sub_total;
              process_columns next [] (tally + sub_total)
            | Terminal (num, '+') ->
              let list = num :: current in
              let sub_total = List.fold_left ( + ) 0 list in
              Printf.printf "%s = %d\n" (list |> List.map (fun n -> Printf.sprintf "%d" n) |> String.concat " + ") sub_total;
              process_columns next [] (tally + sub_total)
            | Terminal (_, op) -> failwith (Printf.sprintf "error: unknown operation '%c'" op)
            | Number num -> process_columns next (num :: current) tally
            | Empty -> process_columns next [] tally in
  process_columns (List.length (List.hd grid) - 1) [] 0

let as_char_list s =
  let rec scan i list =
    if i < 0 then list
    else scan (i - 1) (s.[i] :: list)
  in
  scan (String.length s - 1) []

let lines_to_grid lines = List.map as_char_list lines

let process_lines lines = lines |> lines_to_grid |> do_column_arith

let parse_file path = 
  let chan = open_in path in
  try
    let rec read_lines lines chan = 
      try
        let line = input_line chan in
        read_lines (line :: lines) chan
      with End_of_file -> List.rev lines
    in
    let lines = read_lines [] chan in
    close_in chan;
    Printf.printf "Part2: %d\n" (process_lines lines)
  with e ->
    close_in_noerr chan;
    raise e

let _ = 
  parse_file input_path