{
  open Lexing
  open Parser
  exception Error of string

  

  (*let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }*)
}

let vide = [' ''\t''\n''\r'] 
let lettre= ['a'-'z''A'-'Z''0'-'9']

rule token = parse
     |"input symbols :" { INPUTSYMBOLS }
     |"stack symbols :" { STACKSYMBOLS }
     |"states :"        { STATES}
     |"initial state :" { INITIALSTATE}
     |"initial stack :" { INITIALSTACK}
     |"transitions :"   { TRANSITIONS }
     |'(' {LPAREN}
     |')' {RPAREN}
     |',' {COMMA}
     |';' {SEMICOLON}
     |lettre as c {LETTRE (c)}
     |vide*       { token lexbuf }
     | eof             { EOF }
     | _       { raise (Error (Lexing.lexeme lexbuf)) }