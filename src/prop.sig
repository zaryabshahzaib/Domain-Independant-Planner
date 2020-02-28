(* Propositions (without function symbols) *)

signature PROP =
sig
  exception NotUnifiable

  type term
  type t (* Must be an equality type *)
  type subst

  val newVar: string -> term
  val newConst: string -> term
  val newProp: string -> term list -> t
  val emptySubst: subst
  val isEmptySubst: subst -> bool
  val eq: t * t -> bool
  val isGround: t -> bool
  val compose: subst -> subst -> subst
  val apply: subst -> t -> t
  val unify: t -> t -> subst
  val toString: t -> string
  val substToString: subst -> string
end


