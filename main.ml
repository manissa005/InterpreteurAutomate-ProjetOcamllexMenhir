
let lexbuf = Lexing.from_channel stdin 

let ast = Parser.s Lexer.token lexbuf 


let _ = Type.verif_autom ast;Type.execute_automate ast "aca" ;;
