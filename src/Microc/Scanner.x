{
module Scanner where
}

%wrapper "basic"

$letter = [a-zA-Z]
$digit = 0-9

tokens :-
 $white+  ;
 "/*" ( [^\*] | \*+ [^\/] )* "*/"    ;
 "("      { \_ -> LPAREN }
 ")"      { \_ -> RPAREN }
 "{"      { \_ -> LBRACE }
 "}"      { \_ -> RBRACE }
 ";"      { \_ -> LSemi }
 ","      { \_ -> LComma }
 "+"      { \_ -> LOp Add }
 "-"      { \_ -> LOp Sub }
 "*"      { \_ -> LOp Mult }
 "/"      { \_ -> LOp Div }
 "="      { \_ -> LAssign }
 "=="     { \_ -> LOp Equal }
 "!="     { \_ -> LOp Neq }
 '<'      { \_ -> LOp Less }
 "<="     { \_ -> LOp Leq }
 ">"      { \_ -> LOp Greater }
 ">="     { \_ -> LOp Geq }
 "&&"     { \_ -> LOp And }
 "||"     { \_ -> LOp Or }
 "!"      { \_ -> LUop Not }
 "if"     { \_ -> LIf }
 "else"   { \_ -> LElse }
 "for"    { \_ -> LFor }
 "while"  { \_ -> LWhile }
 "return" { \_ -> LRet }
 "int"    { \_ -> LType Int }
 "bool"   { \_ -> LType Bool }
 "void"   { \_ -> LType Void }
 "true"   { \_ -> LBool True }
 "false"  { \_ -> LBool False }
 $digit+  { \s -> LInt (read s) }
 $alpha [$alpha $digit \_]* { \s -> LId s }

{
import Microc.Ast
data Lexeme = LInt Int
            | LFloat Double
            | LId String
            | LType Type
            | LBool Bool
            | LOp Op
            | LUop Uop
            | LRet
            | LAssign
            | LComma
            | LSemi
            | LPAREN
            | LPAREN
            | LBRACE
            | LBRACE
            | LBRACK
            | LBRACK
            | LFor
            | LWhile
            | LIf
            | LElse
            | LComma
            
}