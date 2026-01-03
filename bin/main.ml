let read_all path = 
    let ic = open_in path in
    Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> 
      let n = in_channel_length ic in 
      really_input_string ic n
      )

let () = 
  (* Display the tokens lexed *)
  match Sys.argv with 
  | [| _; "--lex"; filename |] -> 
    let s = read_all filename in 
    let t = Parser.init_token_stream s in
    Parser.parse_func t 
    |> Ast.Print.pp_func 
    |> Format.print_string 
  | _ -> print_endline "Usage: oc0c --lex <file>"
