open Token

type lexer = 
  {
    len : int;
    content : string;
    mutable position : int;
    mutable ch : char option
  }

let keyword_tbl : (string, t) Hashtbl.t =
  let keyword_map =
  [
    ("struct", Struct);
    ("typedef", Typedef);
    ("if", If);
    ("else", Else);
    ("while", While);
    ("for", For);
    ("continue", Continue);
    ("break", Break);
    ("return", Return);
    ("assert", Assert);
    ("true", True);
    ("false", False);
    ("NULL", Null);
    ("alloc", Alloc);
    ("alloc_array", Alloc_array);
    ("int", Int);
    ("bool", Bool);
    ("void", Void);
    ("char", Char);
    ("string", String);
  ] 
  in 
  let t = Hashtbl.create 32 in
  List.iter (fun (s, tok) -> Hashtbl.add t s tok) keyword_map;
  t

let peek_char (l : lexer) : char option = 
  if l.position + 1 >= l.len 
    then None 
    else Some (String.get l.content (l.position + 1))

let advance_char (l : lexer) : unit = 
  if l.position + 1 >= l.len then (
    l.position <- l.len;
    l.ch <- None
  ) else (
    l.position <- l.position + 1;
    l.ch <- Some (String.get l.content l.position)
  )

let skip_while l ~pred = 
  while pred l do 
    advance_char l;
  done
 
let skip_white_space l =
  let pred l =
    match l.ch with
    | None -> false
    | Some c -> Char.equal c ' ' || Char.equal c '\n' || 
        Char.equal c '\t' || Char.equal c '\r'
  in
  skip_while l ~pred

let take_while l ~pred : string = 
  let start = l.position in
    skip_while l ~pred;
    String.sub l.content start (l.position - start + 1)

let make_dec_const (l : lexer) : Int32.t =
  let pred l =
    match peek_char l with
    | Some c -> c >= '0' && c <= '9'
    | None -> false
  in
  let s = take_while l ~pred in
  Int32.of_string s

let make_hex_const (l : lexer) : Int32.t =
  let pred l = 
    match peek_char l with
    | Some c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    | None -> false
  in 
    let digits = take_while l ~pred in 
    if String.length digits = 0 then 
      Fmt.failwith "make_hex_const: expected hex digits after 0x"
    else 
    Int32.of_string ("0x" ^ digits)

let make_num_const l = 
  match (l.ch, peek_char l) with
  | (Some '0', Some ('x' | 'X')) -> 
    advance_char l;
    advance_char l;
    Hex_const (make_hex_const l)
  | (Some ('0'..'9'), _) -> Dec_const (make_dec_const l)
  | _ -> Fmt.failwith "make_num_const: expected a digit"

let make_operator l =
  match (l.ch, peek_char l) with
  | (Some '+', Some '=') -> advance_char l; PlusEq
  | (Some '+', _)        -> Plus

  | (Some '-', Some '=') -> advance_char l; MinusEq
  | (Some '-', Some '-') -> advance_char l; Minus_minus
  | (Some '-', _)        -> Minus

  | (Some '*', Some '=') -> advance_char l; StarEq
  | (Some '*', _)        -> Star

  | (Some '/', Some '=') -> advance_char l; SlashEq
  | (Some '/', _)        -> Slash

  | (Some '%', Some '=') -> advance_char l; PercentEq
  | (Some '%', _)        -> Percent

  | (Some '=', Some '=') -> advance_char l; EqEq
  | (Some '=', _)        -> Assign

  | _ -> Fmt.failwith "make_operator: expected operator"
                
let make_ident l = 
  let ident_str = 
    take_while l 
    ~pred:(fun lex -> 
             match peek_char lex with
             | Some ('a'..'z' | 'A'..'Z' | '0'..'9' | '_') -> true
             | _ -> false
          )
  in 
  match Hashtbl.find_opt keyword_tbl ident_str with 
  | Some kw -> kw
  | None -> Ident (Symbol.symbol ident_str)

let next_token (l : lexer) : t = 
  skip_white_space l;
  let tok =
    match l.ch with
    | None -> Eof
    | Some ch ->
        match ch with 
        | '0'..'9' -> make_num_const l
        | '+' | '-' | '*' | '/' | '%' | '=' -> make_operator l
        | 'a'..'z' | 'A'..'Z' -> make_ident l
        | '{' -> LBrace
        | '}' -> RBrace
        | '(' -> LParen
        | ')' -> RParen
        | ';' -> Semicolon
        | _ -> Fmt.failwith "next_token: expected a valid token"
  in
  advance_char l;
  tok

let init_lexer content = 
  let lexer = {
    len = String.length content;
    content;
    position = -1;
    ch = None
  }
  in 
  advance_char lexer;
  lexer

let rec lex_all l =
  let tok = next_token l in 
  match tok with
  | Eof -> [Eof]
  | _ -> tok :: lex_all l