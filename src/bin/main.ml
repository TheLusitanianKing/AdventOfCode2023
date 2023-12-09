open Core

let day_mains =
  [ Aoc2023.Day1.main
  ; Aoc2023.Day2.main
  ; Aoc2023.Day3.main
  ; Aoc2023.Day4.main
  ; Aoc2023.Day5.main
  ; Aoc2023.Day6.main
  ; Aoc2023.Day7.main
  ; Aoc2023.Day8.main
  ; Aoc2023.Day9.main
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
