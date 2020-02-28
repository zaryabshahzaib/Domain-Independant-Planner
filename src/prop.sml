structure Prop : PROP =
struct

  exception NotUnifiable
  
  datatype term' = Var of string | Const of string
  type term = term'
  type t = string * term list

  (* Implementing substitution as a list of pairs *)
  type subst = (term * term) list

  fun newVar (s: string) = Var (s)
  
  fun newConst (s: string) = Const (s)

  fun newProp (s: string) (tl: term list) = (s, tl)

  val emptySubst = []

  fun isEmptySubst (s: subst) = s = []

  (* eq: t * t -> bool
     REQUIRES: true
     ENSURES: true iff p1 is identical to p2 (including variable names)
  *)
  fun eq (p1: t, p2: t) = p1 = p2

  fun isGround ((n, args): t) = let
    fun isGround_ [] = true
      | isGround_ (Var _ :: _) = false
      | isGround_ (Const _ :: t) = isGround_ t
  in
    isGround_ args
  end

  fun apply_ ([]: subst) (t: term) = t
    | apply_ ((Var s, s') :: sub) t = (case t 
      of Var st => if st = s then s' else apply_ sub t
       | _ => t )
    | apply_ _ _ = raise Fail "ERROR: Trying to substitute a constant." 
  
  fun apply (s: subst) ((n, args): t) = (n, List.map (apply_ s) args)

  fun compose (tau: subst) (sigma: subst) = let
    val dom_tau = List.map (fn (x, _) => x) tau
    (* Removing (x/t) from sigma if x is in tau's domain *)
    val sigma' = List.filter (fn (x, t) => not (List.exists (fn y => y = x) dom_tau)) sigma
    (* Applying sigma to tau's co-domain *)
    val tau' = List.map (fn (x, t) => (x, apply_ sigma t)) tau 
  in
    tau' @ sigma'
  end
 
  (* unify p1 p2: t -> t -> subst
     REQUIRES: true
     ENSURES: evaluates to a substitution s such that
              (p1)(s) = (p2)(s)
  *)
  fun unify ((n1, args1): t) ((n2, args2): t) = let
    (* Simplified unification procedure:
       - No occurs check (since there are no function symbols)
       - Lists must have the same length
    *)
    fun unify_ ([]: term list) ([]: term list) = []
      | unify_ (Var s1 :: t1) (Var s2 :: t2) = 
        let
          val sub = (Var s1, Var s2)
          val subf = apply_ [sub]
        in
          sub :: (unify_ (List.map subf t1) (List.map subf t2))
        end
      | unify_ (Var s1 :: t1) (Const s2 :: t2) = 
        let
          val sub = (Var s1, Const s2)
          val subf = apply_ [sub]
        in
          sub :: (unify_ (List.map subf t1) (List.map subf t2))
        end
      | unify_ (Const s1 :: t1) (Var s2 :: t2) =
        let
          val sub = (Var s2, Const s1)
          val subf = apply_ [sub]
        in
          sub :: (unify_ (List.map subf t1) (List.map subf t2))
        end
      | unify_ (Const s1 :: t1) (Const s2 :: t2) = 
        if s1 = s2 then unify_ t1 t2
        else raise NotUnifiable
      | unify_ _ _ = raise NotUnifiable
  in
    if n1 = n2 then unify_ args1 args2 else raise NotUnifiable
  end

  fun termToString (Var s: term) = "?" ^ s
    | termToString (Const s) = s

  fun argsToString ([]: term list) = ""
    | argsToString (h :: t) = "(" ^ 
      (List.foldl (fn (i, acc) => acc ^ ", " ^ (termToString i) ) (termToString h) t) ^ ")"
  
  fun toString ((n, args): t) = n ^ (argsToString args)

  fun substToString ([]: subst) = ""
    | substToString ((t1, t2) :: nil) = (termToString t1) ^ "/" ^ (termToString t2)
    | substToString ((t1, t2) :: s) = (termToString t1) ^ "/" ^ (termToString t2) ^ ", " ^ substToString s

end
