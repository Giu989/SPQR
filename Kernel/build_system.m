(* ::Package:: *)

(* ::Title:: *)
(*System Builder and Solver*)


Condition[j[a___]*j[b___],Length[{a}] === Length[{b}]]^:=Apply[j,List[a]+List[b]];
j[a___]^n_^:=Apply[j,n*List[a]];


complementUnsorted[l1_,l2_]:=Delete[l1,l2//ReplaceAll[PositionIndex[l1]]//DeleteCases[x_/;!ListQ[x]]];


generateWeightMatrix[variables_,ordering_] := Module[{},
	If[(variables // Length) == 1,
		Return[{{1}}];
	];
	If[ordering === Lexicographic,
		IdentityMatrix[variables // Length] // Return;
	];
	If[ordering === DegreeLexicographic,
		ConstantArray[1,{variables // Length,variables // Length}] // UpperTriangularize // Times[#, {1}~Join~ConstantArray[-1, (variables // Length)-1]]& // Return;
	];
	If[ordering === DegreeReverseLexicographic,
		Join[ConstantArray[1,variables // Length] // List,-IdentityMatrix[(variables // Length)-1] // Reverse] // PadLeft // Return;
	];
	(*failsafe*)
	Print["Error: No valid ordering specified"];
	Return[$Failed];
];
generateWeightMatrix[variables1_,variables2_,ordering_] := BlockDiagonalMatrix[{generateWeightMatrix[variables2,ordering],generateWeightMatrix[variables1,ordering]}] // Normal;


createSeeds[length_,weight_]:=Join @@ Permutations /@ IntegerPartitions[weight, {length}, Range[0, weight]];
createAllSeeds[length_,maxweight_]:=createSeeds[length,#]&/@Range[0,maxweight]//Flatten[#,1]&;


Options[BuildPolynomialSystem] = {"MonomialOrder" -> Lexicographic, "IrreducibleMonomials"->{},"EliminateVariables"->{{},0},"PrintDebugInfo"->0};
(**)
BuildPolynomialSystem[targets_,ideal_,variables_,maxWeight_,OptionsPattern[]]:= Module[
	{
	idealj,targetsj,jseeds,equations,sortedMonomials,sortedMonomialsj,monomials,systemSparse,
	params,nonzeromat,adjlists,graphName,learn,newEqnNumb,irreducibleMonomials,(*targ,*)(*extraparam,*)
	weightMatrix,varsToTrim,varsNotToTrim,trimDegree,positions,monomialsInIdeal,jseedMonomialsOuter,
	idealCoefficientMatrix,coefficients,tmp1,tmp2,tmp3,associationRules,systemAssociation,targetAssociation,
	valuesUniqueToPositions,numberOfRows,numberOfCols,valuesUnique,indexUniqueToPosition,triples,byI,pivots,
	adjLists,reverseIndex,takePattern,monomialsInTarget,printDebug1,tm,
	Nothing
	},
	(*only prints the statement if the level set in the option value is equal to or higher than the second option here*)
	printDebug1[a_,c_] := printDebug[a,OptionValue["PrintDebugInfo"],c];
	
	(*extracting data from options*)
	varsToTrim = OptionValue["EliminateVariables"] // First;
	varsNotToTrim = complementUnsorted[variables,varsToTrim];
	trimDegree = OptionValue["EliminateVariables"] // Last;

	(*turning the ideal to j notation*)
	idealj = Outer[((CoefficientRules[#,variables][[;;,1]]//Map[Apply[j]])*(CoefficientRules[#,variables][[;;,2]]) // Apply[Plus])&,ideal];
	targetsj = Outer[((CoefficientRules[#,variables][[;;,1]]//Map[Apply[j]])*(CoefficientRules[#,variables][[;;,2]]) // Apply[Plus])&,targets];
	
	(*seeding and trimming the generated system*)
	printDebug1["creating system seeds...",1];
		tm = AbsoluteTiming[
		jseeds = createAllSeeds[variables//Length,maxWeight] (*// Map[Apply[j]]*)//MapApply[j];
		positions = Position[jseeds,j[a___]/;Plus@@({a}[[1;;(varsToTrim // Length)]])<=(maxWeight)-trimDegree] // Flatten;
		jseeds = jseeds[[positions]];
	]//First;
	printDebug1[StringJoin["done: ",ToString[tm],"s\n\n"],1];
	
	(*building the weight matrix for monomial sorting*)
	If[Length[varsToTrim]==0,
		weightMatrix = generateWeightMatrix[variables,OptionValue["MonomialOrder"]];
		,
		weightMatrix = generateWeightMatrix[varsNotToTrim,varsToTrim,OptionValue["MonomialOrder"]];
	];
	
	(*generating the list of all monomials in the ideal*)
	monomialsInIdeal = idealj // Cases[#,_j,\[Infinity]]& // DeleteDuplicates;
	monomialsInTarget = targetsj // Cases[#,_j,\[Infinity]]& // DeleteDuplicates;
	
	(*generating all monomials and sorting them*)
	printDebug1["generating and sorting monomials...",1];
	tm = AbsoluteTiming[
		jseedMonomialsOuter = Outer[Times,jseeds,monomialsInIdeal];
		monomials = Join[(Range[1,targetsj//Length]//Map[targ]//Reverse),(Join[jseeds,jseedMonomialsOuter,monomialsInTarget] // Flatten // DeleteDuplicates) // MapApply[List]//SortBy[weightMatrix . #&]//Reverse // MapApply[j]];
	]//First;
	printDebug1[StringJoin["done: ",ToString[tm],"s\n\n"],1];
	
	(*extracting all unique coefficients (functions) of the parameters*)
	idealCoefficientMatrix = CoefficientArrays[idealj,monomialsInIdeal] // Last // Normal;
	coefficients = idealCoefficientMatrix // SparseArray // #["NonzeroValues"]& // DeleteDuplicates // Sort;
	
	(*rows and columns of final matrix*)
	{numberOfRows, numberOfCols} = {(jseeds // Length)*(idealj // Length) + (targetsj // Length),monomials // Length};
	
	(*preprocessing lists for system construction*)
	printDebug1[StringJoin["preparing to generate system of size ",numberOfRows//ToString," x ",numberOfCols//ToString,"..."],1];
	tm = AbsoluteTiming[
		tmp1=idealCoefficientMatrix -> ((jseedMonomialsOuter)//ReplaceAll[PositionIndex[monomials]])//ReplaceAll[{x_}/;IntegerQ[x]->x] // Map[Thread] // Thread;
		tmp2 = Table[tmp1[[All,1,i]] -> tmp1[[All,2]] // Thread // DeleteCases[#,x_/;(x//First)==0]&,{i,1,idealj // Length}];
		tmp3 = Table[tmp2 // Map[Cases[#,x_/;(x//First)==i]&] // Part[#,All,All,2]&, {i,coefficients}];
	
		(*building lists for system construction*)
		associationRules = Table[Table[Table[{Range[(jseedMonomialsOuter // Length)]+(targetsj // Length)+(jseedMonomialsOuter // Length)*(k-1),i} // Transpose,{i,l[[k]]}] // Transpose // Flatten[#,1]&,{k,1,idealj//Length}] // Apply[Join],{l,tmp3}];
		systemAssociation = coefficients -> associationRules // Thread // Association;
		targetAssociation = Table[targ[i]-targetsj[[i]],{i,1,targetsj//Length}] // CoefficientArrays[#,monomials]& // Last // ArrayRules // Most // GroupBy[Last] // Part[#, All, All, 1]&;
		valuesUniqueToPositions = Merge[{targetAssociation,systemAssociation},Join//Apply];
		valuesUnique = valuesUniqueToPositions // Keys;
		indexUniqueToPosition = valuesUniqueToPositions // KeyMap[Position[valuesUnique, #, {1}]& /* (Part[#, 1, 1]&)];
	]//First;
	
	printDebug1[StringJoin["done: ",ToString[tm],"s\n\n"],1];
	
	(*parameters of system*)
	params = Complement[Join[ideal // Variables, targets // Variables],variables]~Join~{extraparam};
	
	(*generating adjacency lists and take patterns for FiniteFlow loading*)
	printDebug1["generating system...",1];
	tm = AbsoluteTiming[
		triples=Flatten[KeyValueMap[Function[{k,lst},Prepend[k]/@lst],indexUniqueToPosition],1];
		byI=GroupBy[triples,#[[2]]&];
		pivots=Sort@Keys[byI];
		adjLists=Map[Sort@DeleteDuplicates@#[[All,3]]&,byI/@pivots];
		reverseIndex=Association@Flatten@KeyValueMap[Thread[#2->#1]&,indexUniqueToPosition];
		takePattern=MapIndexed[Thread[{#2[[1]],#1}]&,adjLists]// Map[Lookup[reverseIndex,#]&]//Flatten // Thread[{1, #}]&;
	]//First;
	printDebug1[StringJoin["done: ",ToString[tm],"s\n\n"],1];
	
	(*generating the FiniteFLow graph and loading in the unique non zero values*)
	printDebug1["building FiniteFlow Graph...",1];
	tm = AbsoluteTiming[
		graphName = StringJoin["ffGraph","$",RandomInteger[10^15]//ToString];
		FFDeleteGraph[graphName];
		FFNewGraph[graphName,"in",params];
		FFAlgRatFunEval[graphName, "uniqueNonZeroes", {"in"}, params, valuesUnique];
	]//First;
	printDebug1[StringJoin["done: ",ToString[tm],"s\n\n"],1];
	
	(*building the full matrix in FiniteFLow using take patterns*)
	printDebug1["loading system data into FiniteFlow Graph...",1];
	tm = AbsoluteTiming[
		FFAlgTake[graphName, "take", {"uniqueNonZeroes"}, takePattern];
		FFGraphOutput[graphName, "take"];
		FFAlgNodeSparseSolver[
			graphName,
			"solvedSystem",
			{"take"},
			adjLists,
			monomials,
			"NeededVars"->(Range[1,targetsj//Length]//Map[targ]//Reverse)
		];
	]//First;
	printDebug1[StringJoin["done: ",ToString[tm],"s\n\n"],1];
	
	(*learning the graph*)
	printDebug1["learning irreducible monomials...",1];
	tm = AbsoluteTiming[
	FFGraphOutput[graphName, "solvedSystem"];
	FFSolverOnlyHomogeneous[graphName, "solvedSystem"];
	learn = FFSparseSolverLearn[graphName, monomials];
	]//First;
	printDebug1[StringJoin["done: ",ToString[tm],"s\n\n"],1];
	newEqnNumb=FFSparseSolverMarkAndSweepEqs[graphName, "solvedSystem"];
	irreducibleMonomials = "IndepVars"//ReplaceAll[learn] // ReplaceAll[j[x__]:>Times@@(variables^{x})];
	
	printDebug1[StringJoin["final number of equations needed: ",newEqnNumb//ToString],2];
	
	(*checking if the graph has been learned correctly*)
	If[(OptionValue["IrreducibleMonomials"]//Length)=!=0,
		(*check here if we match the expected monomials*)
		If[SubsetQ[OptionValue["IrreducibleMonomials"],irreducibleMonomials],
		(*check passed*)
		Nothing;
		,
		(*check failed*)
		Print["Irreducible monomials do not match. Try increasing the system size"];
		Print["Found ", irreducibleMonomials // Length," monomials: ",irreducibleMonomials];
		Return[$Failed];
		];
	];
	Return[{graphName,params,learn,variables}];
];


Options[ReconstructPolynomialRemainder] = {"Vector" -> False,"PrintDebugInfo"->1,"DeleteGraph"->True};
ReconstructPolynomialRemainder[output_List,OptionsPattern[]]:=Module[{reconstructed,ans},
	reconstructed = FFReconstructFunction[output[[1]],output[[2]],"PrintDebugInfo"->OptionValue["PrintDebugInfo"],"MaxPrimes"->200,"MaxDegree"->1000];
	If[OptionValue["Vector"],
		ans=ArrayReshape[reconstructed,{"DepVars","IndepVars"}//ReplaceAll[output[[3]]]//Map[Length]];
	,
		ans=ArrayReshape[reconstructed,{"DepVars","IndepVars"}//ReplaceAll[output[[3]]]//Map[Length]] // Dot[#,"IndepVars"//ReplaceAll[output[[3]]]]& // ReplaceAll[j[x__]:>Times@@(output[[4]]^{x})];
	];
	(*for some reason this graph deletion doesn't work*)
	If[OptionValue["DeleteGraph"],FFDeleteGraph[output[[1]]//Evaluate]];
	Return[ans];
];
