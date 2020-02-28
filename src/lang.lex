structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

fun getstring s =
   let
    val n = size s
   in
     substring (s, 1, n-2)
   end

val pos = ref 1
val eof = fn () => Tokens.EOF(~1,~1)

exception Illegal_character of pos

(* Conventions:
   - Variables start with upper case letters
   - Identifiers (names of rules, predicates or constants) start with lower case letters
   - Identifiers cannot be the reserved keywords 'init' or 'goal'
*)

%%
%header (functor Lang_LexFun(structure Tokens: Lang_TOKENS));

upper = [A-Z];
lower = [a-z];
any   = [a-zA-Z0-9];
ident = {lower}{any}*;
var   = {upper}{any}*;
num   = [0-9]+;
cmt   = %(.)*;

ws = [\ \t];

%%
<INITIAL>\n       => (pos := (!pos) + 1; lex());
<INITIAL>{ws}+    => (lex());
<INITIAL>{cmt}    => (lex());
<INITIAL>{ident}  => (Tokens.IDENT (yytext,!pos,!pos));
<INITIAL>{var}    => (Tokens.VAR (yytext,!pos,!pos));
<INITIAL>{num}    => (Tokens.NUM (yytext,!pos,!pos));
<INITIAL>":"      => (Tokens.COLON (!pos,!pos));
<INITIAL>","      => (Tokens.COMMA (!pos,!pos));
<INITIAL>"->"     => (Tokens.ARROW (!pos,!pos));
<INITIAL>"."      => (Tokens.DOT (!pos,!pos));
<INITIAL>"("      => (Tokens.LPAREN (!pos,!pos));
<INITIAL>")"      => (Tokens.RPAREN (!pos,!pos));
<INITIAL>"{"      => (Tokens.LBRACK (!pos,!pos));
<INITIAL>"}"      => (Tokens.RBRACK (!pos,!pos));
<INITIAL>"#init"  => (Tokens.INIT (!pos,!pos));
<INITIAL>"#goal"  => (Tokens.GOAL (!pos,!pos));

