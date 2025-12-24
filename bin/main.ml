let read_all filename = 
    let ic = open_in filename in
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
    let l = Lexer.init_lexer s in 
    Lexer.lex_all l 
    |> List.iter (fun t -> print_endline (Token.show t))
  | _ -> print_endline "Usage: oc0c --lex <file>"
