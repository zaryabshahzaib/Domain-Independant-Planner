(* Domain independent planner *)

signature PLANNER =
sig
  
  (* The knowledge base is anything ascribing to the PROPSET signature *)
  structure KB : PROPSET
 
  type facts = KB.t
  type action = KB.P.t
  type rule = action * facts * facts
  type plan = action list

  (* Used by the parser *)
  val newRule: KB.P.t * KB.t * KB.t -> rule

  (* Planner functions *)
  val planDfs    : rule list * facts * facts -> plan option
  val planAllDfs : rule list * facts * facts -> plan list
  val planBfs    : rule list * facts * facts -> plan option
  val planAllBfs : rule list * facts * facts -> plan list

  val toString: plan -> string

end


