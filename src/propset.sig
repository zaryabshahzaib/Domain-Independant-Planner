(* Set of propositions *)

signature PROPSET =
sig
  exception SetNotGround
  exception NoInstances

  (* Type of the elements in the set *)
  structure P: PROP

  (* Type of the set *)
  type t

  val create: P.t list -> t
  val add: P.t -> t -> t
  val remove: P.t -> t -> t
  val union: t -> t -> t
  val diff: t -> t -> t
  val member: P.t -> t -> bool
  val subset: t -> t -> bool
  val equal: t -> t -> bool
  val instances: P.t -> t -> P.subst list
  val subsetInstances: t -> t -> P.subst list
  val apply: P.subst -> t -> t
  val toString: t -> string
end


