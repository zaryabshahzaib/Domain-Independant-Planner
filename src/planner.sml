structure Planner : PLANNER =
struct

  exception Unimplemented

  structure KB: PROPSET = PropSet(Prop)

  type facts = KB.t
  type action = KB.P.t
  type rule = action * facts * facts
  type plan = action list
  
  fun newRule (s: KB.P.t, pre: KB.t, post: KB.t) = (s, pre, post)
 
  val actionToString = KB.P.toString

  fun ruleToString (s, pre, post) = actionToString s ^ ": " ^ 
    KB.toString pre ^ " -> " ^ KB.toString post ^ "."

  fun toString ([]: plan) = "[]"
    | toString (h :: t) = "[" ^
      (List.foldl (fn (i, acc) => acc ^ ", " ^ (actionToString i) ) 
        (actionToString h) t) ^ "]"

  (*** Planners ***)

  (* getfrontier: rule list * facts * (facts * action list)list * facts list ->
   (facts * action list)list
     REQUIRES: true
     ENSURES: returns the corresponding frontier nodes for the given state, by
     applying the applicable rules in the rule list.
  *)
  fun getfrontier ([] : rule list,init: facts,accum: (facts * action list) list,
    visited : facts list): (facts * action list)list = accum
    | getfrontier((a,pre,post)::l,init,accum,visited) = 
    let
      fun member (_: facts, []: facts list): bool = false 
        | member(e,x::l) = if KB.equal(e)(x) then true else member(e,l)
      val subsetins = (SOME (KB.subsetInstances(pre)(init))
                        handle NoInstances => NONE)
    in
      if (subsetins = NONE) then getfrontier(l, init, accum, visited) 
      else if (valOf(subsetins) = [[]]) then 
                if member(post,visited) then 
                  getfrontier(l,init,accum,visited) 
                else let
                  val newstate = KB.union (KB.diff(init)(pre)) (post)
                in
                  getfrontier(l,init,(newstate,[a])::accum,visited) 
                end         
      else let
        val newinitalstates = List.map (fn x=> KB.apply(x)(pre)) 
        (valOf(subsetins))
        val newfinalstates = List.map (fn x=> KB.apply(x)(post)) 
        (valOf(subsetins))
        val instantiatedrules  = List.map (fn x=> KB.P.apply(x)(a)) 
        (valOf(subsetins))

        fun getfrontierhelper ([]: facts list,[]: facts list,
          accum: (facts * action list) list,[]: action list,init: facts,
          visited: facts list): (facts * action list)list = accum
          | getfrontierhelper (x::l,y::b,accum,act::act',init,visited) = 
          let
            val newstate = KB.union (KB.diff(init)(x)) (y)
          in
            if member(newstate,visited) then getfrontierhelper(l,b,accum,act',
              init,visited)
            else getfrontierhelper(l,b,(newstate,[act])::accum,act',init,
              visited)
          end

        val newfrontier = getfrontierhelper(newinitalstates,newfinalstates,[],
          instantiatedrules,init,visited)
      in
          getfrontier(l,init,newfrontier@accum,visited) 
      end
    end

  (* planAllSolutions: rules list * facts * facts * (facts * action list)list 
     * bool * plan list * facts list * bool -> plan list
     REQUIRES: true
     ENSURES: returns a list of all the plans which take us from the 
     initital state to the goal state if any exist, empty list otherwise.
  *)
   fun planAllSolutions ([] : rule list,init: facts,
      final: facts,frontier: (facts * action list)list,flag: bool, 
      accum: plan list,visited: facts list,dfs: bool): plan list = []
     | planAllSolutions (rl,init,final,frontier,flag,accum,visited,dfs) = 
     if flag andalso (frontier = []) then accum 
     else if (frontier = []) 
        then 
          if (KB.subset(final)(init)) then [] 
          else
              let
                val front = getfrontier(rl,init,[],init::visited)
              in
                planAllSolutions(rl,init,final,front,true,accum,init::visited,
                  dfs)
              end
    else
      let
        val ((current,path)::l)= frontier
      in
        if KB.subset(final)(current) then planAllSolutions(rl,init,final,l,flag,
          [path]@accum,visited,dfs) else
        let
          val newfrontier = getfrontier(rl,current,[],current::visited)
          val newfrontier' = List.map (fn (x,y)=>(x,path@y)) newfrontier
        in
          if dfs then planAllSolutions(rl,init,final,(newfrontier' @ l),flag,
            accum,current::visited,dfs)
          else planAllSolutions(rl,init,final,(l @ newfrontier'),flag,
            accum,current::visited,dfs)

        end
      end

  (* planDfs: rules list * facts * facts -> plan option
     REQUIRES: true
     ENSURES: If a plan exists which takes us from the initial to the 
     goal state, planDfs returns SOME plan, NONE otherwise.
  *)
   fun planDfs (rl, init, final): 
      plan option = case planAllSolutions(rl,init,final,[],false,[],[],true) of
        [] => NONE
        | x::l => SOME x

  (* planAllDfs: rules list * facts * facts -> plan option
     REQUIRES: true
     ENSURES: If plans exists such that they take us from the initial to the 
     goal state, planAllDfs returns the plan list containing 
     all such plans, empty plan list otherwise.
  *)
   fun planAllDfs (rl, init, final): 
      plan list = planAllSolutions(rl,init,final,[],false,[],[],true);

  (* planBfs: rules list * facts * facts -> plan option
     REQUIRES: true
     ENSURES: If a plan exists which takes us from the initial to the 
     goal state, planBfs returns SOME plan, NONE otherwise.
  *)
   fun planBfs (rl, init, final): 
      plan option = case planAllSolutions(rl,init,final,[],false,[],[],false) of
        [] => NONE
        | x::l => SOME x
  
  (* planAllBfs: rules list * facts * facts -> plan option
     REQUIRES: true
     ENSURES: If plans exists such that they take us from the initial to the 
     goal state, planAllBfs returns the plan list containing all such plans
     , empty plan list otherwise.
  *)
   fun planAllBfs (rl, init, final): 
      plan list = planAllSolutions(rl,init,final,[],false,[],[],false);

end
