{
 open Lexing TigerParser;

 exception LexicalError of string * int * int (* (message, loc1, loc2) *)

 fun lexerError lexbuf s =
     raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);

 val commentStart = ref 0;  (* Start of outermost comment being scanned *)

 fun commentNotClosed lexbuf =
     raise LexicalError ("Comment not terminated",
                         !commentStart, getLexemeEnd lexbuf);

 val commentDepth = ref 0;  (* Current comment nesting *)

 val stringLiteralStart = ref 0; (* Current string literal start *)

 val stringLiteralValue = ref ""; (* Current string literal value *)

 fun stringLiteralNotValid lexbuf =
     raise LexicalError ("String literal not valid",
                         !stringLiteralStart, getLexemeEnd lexbuf);


 (* Scan keywords as identifiers and use this function to distinguish them. *)
 (* If the set of keywords is large, use an auxiliary hashtable.            *)

 fun keyword s =
     case s of
         "type"             => TYPE
       | "array"            => ARRAY
       | "of"               => OF
       | "var"              => VAR
       | "nil"              => NIL
       | "function"         => FUNCTION
       | "let"              => LET
       | "in"               => IN
       | "end"              => END
       | "if"               => IF
       | "then"             => THEN
       | "else"             => ELSE
       | "while"            => WHILE
       | "break"            => BREAK
       | "for"              => FOR
       | "to"               => TO
       | "do"               => DO
       | _                  => ID s;

fun controlChar (s, lexbuf) =
    case substring(s, 2, 1) of
        "@"                 => "\^@"
      | "A"                 => "\^A"
      | "B"                 => "\^B"
      | "C"                 => "\^C"
      | "D"                 => "\^D"
      | "E"                 => "\^E"
      | "F"                 => "\^F"
      | "G"                 => "\^G"
      | "H"                 => "\^H"
      | "I"                 => "\^I"
      | "J"                 => "\^J"
      | "K"                 => "\^K"
      | "L"                 => "\^L"
      | "M"                 => "\^M"
      | "N"                 => "\^N"
      | "O"                 => "\^O"
      | "P"                 => "\^P"
      | "Q"                 => "\^Q"
      | "R"                 => "\^R"
      | "S"                 => "\^S"
      | "T"                 => "\^T"
      | "U"                 => "\^U"
      | "V"                 => "\^V"
      | "W"                 => "\^W"
      | "X"                 => "\^X"
      | "Y"                 => "\^Y"
      | "Z"                 => "\^Z"
      | "["                 => "\^["
      | "\\"                => "\^\"
      | "]"                 => "\^]"
      | "^"                 => "\^^"
      | "_"                 => "\^_"
      _                     => stringLiteralNotValid lexbuf
 }

let endOfInput =            (eof | `\^Z`)
let whitespace =            [` ` `\t` `\n` `\r`]

rule Token = parse
  whitespace                { Token lexbuf }
  | [`0`-`9`]+              { case Int.fromString (getLexeme lexbuf) of
                                NONE   => lexerError lexbuf "internal error"
                              | SOME i => INT i
                            }
  | [`a`-`z``A`-`Z`][`a`-`z``A`-`Z``0`-`9``_`]*
                            { keyword (getLexeme lexbuf) }
  | "/*"                    { commentStart := getLexemeStart lexbuf;
                              commentDepth := 1;
                              SkipComment lexbuf; Token lexbuf }
  | `"`                     { stringLiteralStart := getLexemeStart lexbuf;
                              stringLiteralValue := "";
                              StringLiteral lexbuf; Token lexbuf }
  | `=`                     { EQ }
  | `:`                     { COLON }
  | `,`                     { COMMA }
  | `{`                     { LBRACE }
  | `}`                     { RBRACE }
  | ":="                    { ASSIGN }
  | `(`                     { LPARAN }
  | `)`                     { RPARAN }
  | `.`                     { DOT }
  | `[`                     { LBRACKET }
  | `]`                     { RBRACKET }
  | `;`                     { SEMICOLON }
  | `+`                     { PLUS }
  | `-`                     { MINUS }
  | `*`                     { TIMES }
  | `/`                     { DIV }
  | "<>"                    { NOTEQ }
  | `>`                     { GT }
  | `<`                     { LT }
  | ">="                    { GE }
  | "<="                    { LE }
  | `|`                     { OR }
  | `&`                     { AND }
  | eof                     { EOF }
  | _                       { lexerError lexbuf "Illegal symbol in input" }

and SkipComment = parse
    "*/"                    { commentDepth := !commentDepth - 1;
                              if !commentDepth = 0 then ()
                              else SkipComment lexbuf
                            }
   | "/*"                   { commentDepth := !commentDepth + 1;
                              SkipComment lexbuf }
   | endOfInput             { commentNotClosed lexbuf }
   | _                      { SkipComment lexbuf }

and StringLiteral = parse
     `"`                    { STRING !stringLiteralValue }
   | `\\` `n`               { stringLiteralValue := !stringLiteralValue ^ "\n"; StringLiteral lexbuf }
   | `\\` `t`               { stringLiteralValue := !stringLiteralValue ^ "\t"; StringLiteral lexbuf }
   | `\\` `^` [`A`-`Z` `@` `[` `\\` `]` `^` `_`]
                            { stringLiteralValue := !stringLiteralValue ^ (controlChar (getLexeme lexbuf, lexbuf)); StringLiteral lexbuf }
   | `\\` [`0`-`9`] [`0`-`9`] [`0`-`9`]
                            { case Int.fromString (substring ((getLexeme lexbuf), 1, 3)) of
                                NONE   => lexerError lexbuf "internal error"
                                SOME i => (( stringLiteralValue := !stringLiteralValue ^ str(chr(i)) ; StringLiteral lexbuf
                                          ) handle _ => stringLiteralNotValid lexbuf) }
   | `\\` `"`               { stringLiteralValue := !stringLiteralValue ^ "\""; StringLiteral lexbuf }
   | `\\` `\\`              { stringLiteralValue := !stringLiteralValue ^ "\\"; StringLiteral lexbuf }
   | `\\` [whitespace]+ `\\`
                            { StringLiteral lexbuf }
   | (endOfInput | [`\t` `\n` `\r` `\\`])
                            { stringLiteralNotValid lexbuf }
   | _                      { stringLiteralValue := !stringLiteralValue ^ (getLexeme lexbuf); StringLiteral lexbuf }
;
