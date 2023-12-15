open Core

let day_mains =
  [ Aoc2023.Day01.main
  ; Aoc2023.Day02.main
  ; Aoc2023.Day03.main
  ; Aoc2023.Day04.main
  ; Aoc2023.Day05.main
  ; Aoc2023.Day06.main
  ; Aoc2023.Day07.main
  ; Aoc2023.Day08.main
  ; Aoc2023.Day09.main
  ; Aoc2023.Day10.main
  ; Aoc2023.Day11.main
  ; Aoc2023.Day12.main
  ; Aoc2023.Day13.main
  ; Aoc2023.Day14.main
  ; Aoc2023.Day15.main
  ]

let run_day day_number input_filename () =
  try
    let file_content = In_channel.read_lines input_filename in
    let m =
      Map.of_alist_exn (module Int)
      @@ List.zip_exn
           (List.range 1 (List.length day_mains) ~start:`inclusive
              ~stop:`inclusive)
           day_mains in
    match Map.find m day_number with
    | None -> Stdio.print_endline "This day hasn't been done."
    | Some f -> f file_content
  with
  | Sys_error _ -> Stdio.print_endline "Could not read the input."
  | _ -> Stdio.print_endline "Failed to solve."

let command =
  Command.basic_spec ~summary:"Gives AoC result for the given day"
    Command.Spec.(
      empty
      +> anon ("day" %: int)
      +> flag "--input"
           (required Filename.arg_type)
           ~full_flag_required:()
           ~aliases:["-i"]
           ~doc:("filepath" ^ " " ^ "Input for the day"))
    run_day

let () = Command_unix.run command
