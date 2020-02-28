structure Test = 
struct

  	fun planListToString [] = "{}\n"
    | planListToString (h::t) = "{ " ^ 
      (List.foldl (fn (p, acc) => acc ^ "\n  " ^ (Planner.toString p) ) (Planner.toString h) t) ^ " }\n"

  (****************** TESTS ******************)
  
	fun testDfsorBFS(path: string, dfs: bool): Planner.plan option = let
		
		val problem0 = Parser.parse (path)
		val plan1 = Planner.planBfs problem0
		val plan2 = Planner.planDfs problem0
	in
		if dfs then plan2 else plan1
	end

	fun testDfsAllorBfsAll(path: string, dfs: bool): Planner.plan list = let
		
		val problem0 = Parser.parse (path)
		val plan1 = Planner.planAllBfs problem0
		val plan2 = Planner.planAllDfs problem0
	in
		if dfs then plan2 else plan1
	end

	val boat = testDfsAllorBfsAll("examples/boat.pp",true);
	val hanoi1 = testDfsAllorBfsAll("examples/hanoi1.pp",false);

	val blocks1 = testDfsorBFS("examples/blocks1.pp",true);
	val blocks8 = testDfsorBFS("examples/blocks8.pp",false);

	val sample1 = testDfsAllorBfsAll("examples/sample1.pp",true);
	val sample5 = testDfsAllorBfsAll("examples/sample5.pp",false);
	val samplenoSol = testDfsAllorBfsAll("examples/sample-nosol.pp",false);

	val monkey1 = testDfsAllorBfsAll("examples/monkey1.pp",true);
	val monkey2 = testDfsAllorBfsAll("examples/monkey2.pp",false);








end

