functor PropSet(P: PROP) : PROPSET =
struct
  exception SetNotGround
  exception NoInstances

  structure P = P

  (* Set implemented as a list without duplicates *)
  type t = P.t list

  fun create (l: P.t list): t = l

  fun add (e: P.t) (s: t) = if List.exists (fn x => P.eq (x, e)) s then s else (e :: s)

  (* remove e s
     REQUIRES: true
     ENSURES: result is s without e
  *)
  fun remove (e: P.t) ([]: t) = []
    | remove e (h :: t) = if P.eq (e, h) then t else h :: remove e t

  (* union s1 s2
     REQUIRES: true
     ENSURES: result is the union of stable items and multi-set union of ephemeral items
  *)
  fun union (s1: t) (s2: t) = List.foldl (fn (x, acc) => add x acc) s2 s1

  (* diff s1 s2
     REQUIRES: true
     ENSURES: result is the set s1 without elements from s2
  *)
  fun diff (s1: t) (s2: t) = List.foldl (fn (x, acc) => remove x acc) s1 s2

  fun member (e: P.t) (s: t) = List.exists (fn x => P.eq (x, e)) s

  fun subset (s1: t) (s2: t) = List.all (fn x => member x s2) s1

  fun equal (s1: t) (s2: t) = subset s1 s2 andalso subset s2 s1

  fun isGround (s: t) = List.all P.isGround s

  fun apply (sub: P.subst) (s: t) = List.map (P.apply sub) s

  (* instances: P.t -> t -> P.subst list
     REQUIRES: isGround s
     ENSURES: instances e s evaluates to a list of substitutions [ss1,...,ssn]
              such that (e)(ssi) \in s
              or raises NoInstances if there is no substitution
  *)
  fun instances (e: P.t) (s: t) = 
    if isGround s then
    let
      fun findInstances (p: P.t) ([]: t): P.subst list = []
        | findInstances p (p1 :: t) = let
          val s = P.unify p p1
        in
          s :: (findInstances p t)
        end
        handle P.NotUnifiable => findInstances p t
        val inst = findInstances e s
    in
      if List.length inst = 0 then raise NoInstances
      else inst
    end
    else raise SetNotGround

  (* subsetInstances: t -> t -> P.subst list
     REQUIRES: isGround s2
     ENSURES: subsetInstances s1 s2 is a list of substitutions [ss1,...,ssn]
              such that (s1)(ssi) \subset s2
              or raises NoInstances if there is no substitution
  *)
  fun subsetInstances (s1: t) (s2: t) = case (isGround s1, isGround s2) 
    of (true, true) => if subset s1 s2 then [P.emptySubst] else raise NoInstances
     | (false, true) => let
        fun ssInstances ([]: t) (s2: t) = [P.emptySubst]
          | ssInstances (p :: tl) s2 =
            List.foldr (fn (sub, acc) => let
              val (p' :: tl') = apply sub (p :: tl)
              val new_s2 = remove p' s2
            in
              (List.map (fn sub2 => P.compose sub sub2) (ssInstances tl' new_s2)) @ acc
              (* If tl' is not a subset of s2, skip this substitution *)
              handle NoInstances => acc
            end
            ) [] (instances p s2)
          val res = ssInstances s1 s2
      in
        if (List.length res) = 0 orelse (P.isEmptySubst (List.hd res)) then raise NoInstances
        else res
      end
     | (_, false) => raise SetNotGround

  fun toString ([]: t) = "{}"
    | toString (h :: t) = "{" ^ 
      (List.foldl (fn (i, acc) => acc ^ ", " ^ (P.toString i) ) (P.toString h) t) ^ "}"

end
