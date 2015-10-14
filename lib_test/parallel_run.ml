let parallel_run count cmd args =
  Printf.printf "Running `%s %s` in %d concurrent processess...\n%!"
    cmd (String.concat " " args) count;
  let exns_m = Mutex.create () in
  let exns = Hashtbl.create count in
  let run_cmd n =
    try
      Utils.(run cmd args |> ignore_string);
    with exn ->
      Mutex.lock exns_m;
      Hashtbl.add exns n exn;
      Mutex.unlock exns_m in
  Array.(make count () |> mapi (fun i _ -> i + 1) |> to_list)
  |> List.map (fun n -> (n, Thread.create run_cmd n))
  |> List.iter (fun (n, t) -> Thread.join t);
  Printf.printf "All processes finished. Failures: %d\n" (Hashtbl.length exns);
  if Hashtbl.length exns <> 0 then begin
    Hashtbl.iter (fun k v ->
      Printf.fprintf stderr "\tProcess %d failed with %s\n" k (Printexc.to_string v)
    ) exns;
    exit 1
  end

open Cmdliner

let processes = 
  let doc = "Run the command in $(docv) concurrent processes." in
  Arg.(value & opt int 10 & info ["p"; "processes"] ~docv:"PROCESSES" ~doc)

let cmd =
  let doc = "Path to the executable to run." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"CMD" ~doc)

let args =
  let doc = "Command line arguments to [CMD]." in
  Arg.(value & pos_right 0 string [] & info [] ~docv:"ARGS" ~doc)

let parallel_run_t = Term.(pure parallel_run $ processes $ cmd $ args)

let info =
  let doc = "Run a command concurrently in multiple processes." in
  Term.info "parallel-run" ~version:"0.0.1" ~doc ~man:[]

let _ =
  match Term.eval (parallel_run_t, info) with | `Error _ -> exit 1 | _ -> exit 0
