{
module Microc.Scanner where
import Microc.Ast

}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = 0-9

tokens :-
 $white+  ;
 "/*" ( [^\*] | \*+ [^\/] )* "*/"    ;
 \(       { \_ -> LPAREN }
 \)       { \_ -> RPAREN }
 \{       { \_ -> LBRACE }
 \}       { \_ -> RBRACE }
 \;       { \_ -> LSemi        }
 \,       { \_ -> LComma        }
 \+       { \_ -> LAdd        }
 \-       { \_ -> LSub        }
 \*       { \_ -> LMul        }
 \/       { \_ -> LDiv        }
 \=       { \_ -> LAssign         }
 \=\=     { \_ -> LEqual        }
 \!\=     { \_ -> LNeq        }
 \<       { \_ -> LLess        }
 \<\=     { \_ -> LLeq        }
 \>       { \_ -> LGreater        }
 \>\=     { \_ -> LGeq        }
 \&\&     { \_ -> LAnd        }
 \|\|     { \_ -> LOr          }
 \!       { \_ -> LNot             }
 "if"     { \_ -> LIf }
 "else"   { \_ -> LElse }
 "for"    { \_ -> LFor }
 "while"  { \_ -> LWhile }
 "return" { \_ -> LRet }
 "int"    { \_ -> LType TyInt }
 "bool"   { \_ -> LType TyBool }
 "void"   { \_ -> LType TyVoid }
 "true"   { \_ -> LBool True }
 "false"  { \_ -> LBool False }
 $digit+  { \s -> LInt (read s) }
 $digit+ \. $digit+ [eE] $digit* { \s -> LFloat (read s) }
 $alpha [$alpha $digit \_]* { \s -> LId s }

{
data Lexeme = LInt Int
            | LFloat Double
            | LId String
            | LType Type
            | LBool Bool
            | LRet
            | LAssign
            | LComma
            | LSemi
            | LPAREN
            | RPAREN
            | LBRACE
            | RBRACE
            | LBRACK
            | RBRACK
            | LFor
            | LWhile
            | LIf
            | LElse
            | LAdd
            | LSub
            | LMul
            | LDiv
            | LEqual
            | LNeq
            | LLess
            | LLeq
            | LGreater
            | LGeq
            | LAnd
            | LOr 
            | LNot
            }
