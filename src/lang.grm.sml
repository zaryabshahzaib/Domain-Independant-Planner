functor LangLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Lang_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure PL = Planner

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\007\000\000\000\
\\001\000\002\000\022\000\003\000\021\000\004\000\020\000\000\000\
\\001\000\005\000\011\000\000\000\
\\001\000\005\000\015\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\007\000\025\000\000\000\
\\001\000\008\000\036\000\000\000\
\\001\000\008\000\038\000\000\000\
\\001\000\008\000\039\000\000\000\
\\001\000\010\000\027\000\000\000\
\\001\000\011\000\024\000\000\000\
\\001\000\011\000\029\000\000\000\
\\001\000\012\000\035\000\000\000\
\\001\000\012\000\037\000\000\000\
\\001\000\013\000\009\000\000\000\
\\001\000\014\000\014\000\000\000\
\\041\000\000\000\
\\042\000\002\000\007\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\006\000\026\000\000\000\
\\048\000\000\000\
\\049\000\009\000\012\000\000\000\
\\050\000\000\000\
\\051\000\006\000\028\000\000\000\
\\052\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\"
val actionRowNumbers =
"\001\000\015\000\018\000\003\000\
\\025\000\032\000\016\000\004\000\
\\019\000\001\000\002\000\017\000\
\\005\000\011\000\006\000\023\000\
\\010\000\027\000\031\000\029\000\
\\030\000\012\000\001\000\001\000\
\\001\000\026\000\002\000\001\000\
\\013\000\007\000\024\000\028\000\
\\014\000\008\000\020\000\009\000\
\\021\000\022\000\000\000"
val gotoT =
"\
\\002\000\004\000\004\000\003\000\008\000\002\000\009\000\001\000\
\\010\000\038\000\000\000\
\\006\000\006\000\000\000\
\\002\000\004\000\004\000\003\000\008\000\002\000\009\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\011\000\000\000\
\\000\000\
\\000\000\
\\002\000\004\000\004\000\015\000\005\000\014\000\000\000\
\\001\000\017\000\003\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\004\000\004\000\015\000\005\000\028\000\000\000\
\\002\000\004\000\004\000\015\000\005\000\029\000\000\000\
\\002\000\004\000\004\000\015\000\005\000\030\000\000\000\
\\000\000\
\\001\000\017\000\003\000\031\000\000\000\
\\002\000\004\000\004\000\015\000\005\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 39
val numrules = 16
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (string) | VAR of unit ->  (string)
 | IDENT of unit ->  (string)
 | file of unit ->  (PL.rule list*PL.KB.t*PL.KB.t)
 | rules of unit ->  (PL.rule list) | rule of unit ->  (PL.rule)
 | goal of unit ->  (PL.KB.t) | init of unit ->  (PL.KB.t)
 | set of unit ->  (PL.KB.t) | prop of unit ->  (PL.KB.P.t)
 | args of unit ->  (PL.KB.P.term list) | id of unit ->  (string)
 | term of unit ->  (PL.KB.P.term)
end
type svalue = MlyValue.svalue
type result = PL.rule list*PL.KB.t*PL.KB.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "IDENT"
  | (T 2) => "VAR"
  | (T 3) => "NUM"
  | (T 4) => "COLON"
  | (T 5) => "COMMA"
  | (T 6) => "ARROW"
  | (T 7) => "DOT"
  | (T 8) => "LPAREN"
  | (T 9) => "RPAREN"
  | (T 10) => "LBRACK"
  | (T 11) => "RBRACK"
  | (T 12) => "INIT"
  | (T 13) => "GOAL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.goal goal1, _, goal1right)) :: ( _, ( 
MlyValue.init init1, _, _)) :: ( _, ( MlyValue.rules rules1, 
rules1left, _)) :: rest671)) => let val  result = MlyValue.file (fn _
 => let val  (rules as rules1) = rules1 ()
 val  (init as init1) = init1 ()
 val  (goal as goal1) = goal1 ()
 in ((rules, init, goal))
end)
 in ( LrTable.NT 9, ( result, rules1left, goal1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.rule rule1, rule1left, rule1right)) :: 
rest671)) => let val  result = MlyValue.rules (fn _ => let val  (rule
 as rule1) = rule1 ()
 in ([rule])
end)
 in ( LrTable.NT 8, ( result, rule1left, rule1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.rules rules1, _, rules1right)) :: ( _, ( 
MlyValue.rule rule1, rule1left, _)) :: rest671)) => let val  result = 
MlyValue.rules (fn _ => let val  (rule as rule1) = rule1 ()
 val  (rules as rules1) = rules1 ()
 in (rule :: rules)
end)
 in ( LrTable.NT 8, ( result, rule1left, rules1right), rest671)
end
|  ( 3, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.set set2, _, _))
 :: _ :: ( _, ( MlyValue.set set1, _, _)) :: _ :: ( _, ( MlyValue.prop
 prop1, prop1left, _)) :: rest671)) => let val  result = MlyValue.rule
 (fn _ => let val  (prop as prop1) = prop1 ()
 val  set1 = set1 ()
 val  set2 = set2 ()
 in (PL.newRule (prop, set1, set2))
end)
 in ( LrTable.NT 7, ( result, prop1left, DOT1right), rest671)
end
|  ( 4, ( ( _, ( _, _, DOT1right)) :: _ :: ( _, ( MlyValue.set set1, _
, _)) :: _ :: _ :: ( _, ( _, INIT1left, _)) :: rest671)) => let val  
result = MlyValue.init (fn _ => let val  (set as set1) = set1 ()
 in (set)
end)
 in ( LrTable.NT 5, ( result, INIT1left, DOT1right), rest671)
end
|  ( 5, ( ( _, ( _, _, DOT1right)) :: _ :: ( _, ( MlyValue.set set1, _
, _)) :: _ :: _ :: ( _, ( _, GOAL1left, _)) :: rest671)) => let val  
result = MlyValue.goal (fn _ => let val  (set as set1) = set1 ()
 in (set)
end)
 in ( LrTable.NT 6, ( result, GOAL1left, DOT1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.prop prop1, prop1left, prop1right)) :: 
rest671)) => let val  result = MlyValue.set (fn _ => let val  (prop
 as prop1) = prop1 ()
 in (PL.KB.create [prop])
end)
 in ( LrTable.NT 4, ( result, prop1left, prop1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.set set1, _, set1right)) :: _ :: ( _, ( 
MlyValue.prop prop1, prop1left, _)) :: rest671)) => let val  result = 
MlyValue.set (fn _ => let val  (prop as prop1) = prop1 ()
 val  (set as set1) = set1 ()
 in (PL.KB.add (prop) set)
end)
 in ( LrTable.NT 4, ( result, prop1left, set1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.prop (fn _ => let val  (id as id1) = id1
 ()
 in (PL.KB.P.newProp id [])
end)
 in ( LrTable.NT 3, ( result, id1left, id1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.args args1, _
, _)) :: _ :: ( _, ( MlyValue.id id1, id1left, _)) :: rest671)) => let
 val  result = MlyValue.prop (fn _ => let val  (id as id1) = id1 ()
 val  (args as args1) = args1 ()
 in (PL.KB.P.newProp id args)
end)
 in ( LrTable.NT 3, ( result, id1left, RPAREN1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.term term1, term1left, term1right)) :: 
rest671)) => let val  result = MlyValue.args (fn _ => let val  (term
 as term1) = term1 ()
 in ([term])
end)
 in ( LrTable.NT 2, ( result, term1left, term1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.args args1, _, args1right)) :: _ :: ( _, ( 
MlyValue.term term1, term1left, _)) :: rest671)) => let val  result = 
MlyValue.args (fn _ => let val  (term as term1) = term1 ()
 val  (args as args1) = args1 ()
 in (term :: args)
end)
 in ( LrTable.NT 2, ( result, term1left, args1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.VAR VAR1, VAR1left, VAR1right)) :: rest671)
) => let val  result = MlyValue.term (fn _ => let val  (VAR as VAR1) =
 VAR1 ()
 in (PL.KB.P.newVar VAR)
end)
 in ( LrTable.NT 0, ( result, VAR1left, VAR1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.term (fn _ => let val  (IDENT
 as IDENT1) = IDENT1 ()
 in (PL.KB.P.newConst IDENT)
end)
 in ( LrTable.NT 0, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.term (fn _ => let val  (NUM as NUM1) =
 NUM1 ()
 in (PL.KB.P.newConst NUM)
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.id (fn _ => let val  (IDENT
 as IDENT1) = IDENT1 ()
 in (IDENT)
end)
 in ( LrTable.NT 1, ( result, IDENT1left, IDENT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.file x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Lang_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun VAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VAR (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun INIT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GOAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
end
end
