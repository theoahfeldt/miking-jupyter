open Jupyter_kernel
open Boot.Repl
open Boot.Eval
open Lwt.Infix

let to_utf8 = Boot.Ustring.to_utf8

let current_output = ref (BatIO.output_string ())
let other_actions = ref []

let enable_visualize = ref false
let ipm_port = ref 0

let text_data_of_string str =
  Client.Kernel.mime ~ty:"text/plain" str

let kernel_output_string str = BatIO.nwrite !current_output str
let kernel_output_ustring ustr = ustr |> to_utf8 |> kernel_output_string

let _ = Py.initialize ~version:3 ()
let ocaml_module = Py.Import.add_module "_mcore_kernel"

(* Set Python's sys.stdout to our own ocaml function to handle Python prints *)
let init_py_print () =
  let py_ocaml_print args =
    kernel_output_string (Py.String.to_string args.(0));
    Py.none
  in
  Py.Module.set_function ocaml_module "ocaml_print" py_ocaml_print;
  ignore @@ Py.Run.eval ~start:Py.File "
import sys
from _mcore_kernel import ocaml_print
class OCamlPrint:
    def write(self, str):
        ocaml_print(str)

sys.stdout = OCamlPrint()"

let init_py_mpl () =
  ignore @@ Py.Run.eval ~start:Py.File "
import os, sys
sys.path.append(os.path.expanduser('~') + '/.local/lib/mcore/kernel')
os.environ['MPLBACKEND']='module://mpl_backend'";
  let py_ocaml_show args =
    let data = Py.String.to_string args.(0) in
    other_actions := Client.Kernel.mime ~base64:true ~ty:"image/png" data :: !other_actions;
    Py.none
  in
  Py.Module.set_function ocaml_module "ocaml_show" py_ocaml_show

let init () =
  initialize_envs ();
  Boot.Mexpr.program_output := kernel_output_ustring;
  Py.Module.set_function ocaml_module "after_exec" (fun _ -> Py.none);
  init_py_print ();
  init_py_mpl ();
  Lwt.return ()

let parse_and_eval code count =
  parse_prog_or_mexpr (Printf.sprintf "In [%d]" count) code
  |> repl_eval_ast

let visualize_model code count =
  if !enable_visualize then
    let seq2string tm =
      let open Boot.Ast in
      match tm with
      | TmSeq(fi, seq) -> tmseq2ustring fi seq |> to_utf8
      | _ -> Boot.Msg.raise_error (tm_info tm) "Expected a string to visualize, but got other term"
    in
    let model_str = parse_and_eval code count |> seq2string in
    let iframe_str = Printf.sprintf {|<embed src="http://localhost:%d/" width="100%%" height="400"</embed>|} !ipm_port in
    let uri = Uri.of_string (Printf.sprintf "http://localhost:%d/js/data-source.js" !ipm_port) in
    let body = Cohttp_lwt.Body.of_string model_str in
    Lwt_main.run (Cohttp_lwt_unix.Client.post ~body uri >|= ignore);
    other_actions := Client.Kernel.mime ~ty:"text/html" iframe_str::!other_actions;
    None
  else
    Boot.Msg.raise_error NoInfo "The kernel is not running with support for %%visualize"

let is_expr pycode =
  try
    ignore @@ Py.compile ~source:pycode ~filename:"" `Eval;
    true
  with _ -> false

let eval_python code =
  let statements, expr =
    try BatString.rsplit code ~by:"\n"
    with Not_found -> ("", code)
  in
  let py_val = if is_expr expr then
    (ignore @@ Py.Run.eval ~start:Py.File statements;
    Py.Run.eval expr)
  else
    Py.Run.eval ~start:Py.File code
  in
  if py_val = Py.none then
    None
  else
    Some(Py.Object.to_string py_val)

let exec ~count code =
  let magic_indicator, content =
    try BatString.split code ~by:"\n"
    with Not_found -> ("", code)
  in
  let result = try
    Ok (
      match magic_indicator with
      | "%%python" -> eval_python content
      | "%%visualize" -> visualize_model content count
      | _ ->
        parse_and_eval code count
        |> repl_format
        |> Option.map to_utf8)
    with e -> error_to_ustring e |> to_utf8 |> Result.error
  in
  let ok_message result =
    ignore @@ Py.Module.get_function ocaml_module "after_exec" [||];
    let new_actions =
      match BatIO.close_out !current_output with
      | "" -> !other_actions
      | s -> text_data_of_string s :: !other_actions
    in
    let actions = List.rev new_actions in
    current_output := BatIO.output_string ();
    other_actions := [];
    { Client.Kernel.msg=result
    ; Client.Kernel.actions=actions }
  in
  Lwt.return (Result.map ok_message result)

let complete ~pos str =
  let start_pos, completions = get_completions str pos in
  Lwt.return { Client.Kernel.completion_matches = completions
             ; Client.Kernel.completion_start = start_pos
             ; Client.Kernel.completion_end = pos}

let get_open_port start_port max_port =
  let open Unix in
  let localhost port = ADDR_INET (inet_addr_loopback, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  let rec iterate_ports port =
    if port < max_port then
      try
        bind sock (localhost port);
        close sock;
        Some port
      with Unix_error (EADDRINUSE,_,_) ->
        iterate_ports (port + 1)
    else
      None
  in iterate_ports start_port

let main =
  let mcore_kernel =
    Client.Kernel.make
      ~language:"MCore"
      ~language_version:[0; 1]
      ~file_extension:".mc"
      ~codemirror_mode:"mcore"
      ~banner:"The core language of Miking - a meta language system
for creating embedded domain-specific and general-purpose languages"
      ~init:init
      ~exec:exec
      ~complete:complete
      ()
  in
  let ipm_exec = ref "default" in
  let args = [ ("--enable-visualize", Arg.Set(enable_visualize), "Enable the %%visualize directive")
             ; ("-v", Arg.Set(enable_visualize), "Short form of --enable-visualize")
             ; ("--ipm-server", Arg.Set_string(ipm_exec), "For --enable-visualize: set the name of the IPM server executable")
             ]
  in
  let config =
    Client_main.mk_config ~usage:"Usage: kernel --connection-file {connection_file} [-v] [--ipm-server SERVER]"
                          ~additional_args:args
                          ()
  in
  let run_kernel () = Client_main.main ~config:config ~kernel:mcore_kernel in
  if !enable_visualize then
    match get_open_port 3030 3130 with
    | Some p ->
      let ipm_server = Lwt_process.exec
        ("", [|!ipm_exec; "--no-file"; "-p"; string_of_int p|])
      in
      ipm_port := p;
      Lwt_main.run (run_kernel () <&> (ipm_server >|= ignore))
    | None -> prerr_endline "Failed to get open port... Exiting."
  else
    Lwt_main.run (run_kernel ())
