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

let apply_ops grid ops =
  List.mapi (fun i op -> match op with
    | '*' -> List.fold_left ( * ) 1 (column i grid)
    | '+' -> List.fold_left ( + ) 0 (column i grid)
    | _ -> failwith (Printf.sprintf "error: unknown operation '%c'" op)
  ) ops
  |> List.fold_left (fun acc x -> acc + x) 0
  
let int_from str =
  (* read an int, slurp basic whitespace, read the rest *)
  try Some (Scanf.sscanf str "%d%[ \t]%[^\n]" (fun n _ rest -> (n, rest))) with
    | Scanf.Scan_failure _ -> None
    | End_of_file -> None
  
let op_from str =
  (* read an int, slurp basic whitespace, read the rest *)
  try
    match Scanf.sscanf str "%[*+]%[ \t]%[^\n]" (fun n _ rest -> (n, rest)) with
      | ("*", rest) -> Some ('*', rest)
      | ("+", rest) -> Some ('+', rest)
      | _ -> None
  with
    | Scanf.Scan_failure _ -> None
    | End_of_file -> None

let process_numbers line =
  let rec all_ints acc s =
    match int_from s with
    | Some (n, rest) -> all_ints (n :: acc) rest
    | None -> List.rev acc in
  all_ints [] line

let process_ops line =
  let rec all_ops acc s =
    match op_from s with
    | Some (op, rest) -> all_ops (op :: acc) rest
    | None -> List.rev acc in
  all_ops [] line
  
let process_lines lines =
  let rec process acc lines =
    match lines with
      | [last_line] -> 
          let ops = process_ops (String.trim last_line) in
          ((List.rev acc), ops)
      | line :: rest ->
          let nums = process_numbers (String.trim line) in
          process (nums :: acc) rest
      | [] -> failwith "error: no lines to process" in
  let (grid, ops) = process [] lines in
  apply_ops grid ops

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
    Printf.printf "Part1: %d\n" (process_lines lines)
  with e ->
    close_in_noerr chan;
    raise e

let _ = 
  parse_file input_path