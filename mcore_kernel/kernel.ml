open Jupyter_kernel

let init () =
  Lwt.return (Py.initialize ())

let exec ~count:_ _ =
  Lwt.return (Ok { Client.Kernel.msg=Some("ok")
                 ; Client.Kernel.actions=[]})

let main =
  let mcore_kernel =
    Client.Kernel.make
      ~language:"MCore"
      ~language_version:[0; 1]
      ~file_extension:".mc"
      ~codemirror_mode:"ocaml"
      ~banner:"The core language of Miking - a meta language system
for creating embedded domain-specific and general-purpose languages"
      ~init:init
      ~exec:exec
      () in
      let config = Client_main.mk_config ~usage:"Usage: hello" () in
      Lwt_main.run (Client_main.main ~config:config ~kernel:mcore_kernel)
