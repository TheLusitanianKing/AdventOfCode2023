open Core

let run_day day_number input_filename () =
  try
    let file_content = In_channel.read_lines input_filename in
    match day_number with
    | 1 -> Aoc2023.Day1.main file_content
    | 2 -> Aoc2023.Day2.main file_content
    | 3 -> Aoc2023.Day3.main file_content
    | 4 -> Aoc2023.Day4.main file_content
    | _ -> Stdio.print_endline "This day hasn't been done yet."
  with
  | Sys_error _ -> Stdio.print_endline "Could not read the input file."
  | _ -> Stdio.print_endline "Failed to solve the given day."

let command =
  Command.basic_spec ~summary:"Gives AoC result for the given day"
    Command.Spec.(
      empty
      +> anon ("day" %: int)
      +> flag "input" (required Filename.arg_type) ~doc:"Input for the day")
    run_day

let () = Command_unix.run command
