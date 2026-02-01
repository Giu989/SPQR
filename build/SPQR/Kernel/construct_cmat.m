(* ::Package:: *)

(* ::Title:: *)
(*Companion Matrix Generation*)


Options[BuildCompanionMatrices] = {"MonomialOrder" -> Lexicographic,"PrintDebugInfo"->0, "ExtraParams"->{}, "LinkGraph" -> <||>};
BuildCompanionMatrices[ideal_,variables_,maxWeight_,irreducibleMonomials_,OptionsPattern[]]:=Module[
	{
		cmatsMonomials,solverOutput,cpmatrixNames,takePatternLists,uniqueparam,
		Nothing
	},
	
	(*check for variables with subscripts or indices which currently cannot be handeled further down the pipeline*)
	If[(variables//Depth)>2,
		Print["Warning: Variables with indices are currently not supported. This is a known issue. Please rename your variables"];
		Return[$Failed];
	];
	
	(*catch if infinite irreducible monomials are accidentally passed*)
	If[irreducibleMonomials === \[Infinity],
		Print["Warning: Infinite irreducible monomials. Companion matrices cannot be generated"];
		Return[$Failed];
	];
	
	cmatsMonomials = Outer[#*irreducibleMonomials&,variables] // Flatten;
	(*Print["BuildPolynomialSystem: ", 
	    cmatsMonomials, " ", ideal, " ", variables, " ", maxWeight
    ];*)
	solverOutput = BuildPolynomialSystem[cmatsMonomials,ideal,variables,maxWeight,
											"IrreducibleMonomials"->irreducibleMonomials,"MonomialOrder"->OptionValue["MonomialOrder"],
											"PrintDebugInfo"->OptionValue["PrintDebugInfo"],"ExtraParams"->OptionValue["ExtraParams"],
											"LinkGraph"->OptionValue["LinkGraph"]
										];
	
	If[Head[solverOutput]===$Failed,Return[$Failed]];
	
	If[Length[solverOutput[[3]][[2]][[2]]]!=Length[irreducibleMonomials],
		uniqueparam = ToExpression[Unique["dummyparam"]];
		Print["Warning: Not all irreducible monomials targeted during divison, SPQR will faill to build companion matrices"];
		Print["This is a known bug. As a temporary fix, add the equation: ", uniqueparam - 1, " to the ideal as well as ", uniqueparam, " to the list of variables"];
		Print["Check also if each entry in the ideal factorises"];
		Return[$Failed];
	];
	solvedSystemNames = Cases[FiniteFlow`Private`FFGraphNodes[solverOutput[[1]]],x_ /; StringMatchQ[ToString[x],"solvedSystem" ~~ ___]];
	(*If[Length[solvedSystemNames]=!=1,Print["Warning: graph parsing failed. Found ", solveSystemNames, " solvedSystems"]; Return[$Failed]];*)
	solvedSystemName = solvedSystemNames // Last;
	(*Print["found graph node with name: ",solvedSystemName];*)
	(*take companion matrices using FFAlgTake and then return them as outputs to the graph with FFGraphOutput*)
	cpmatrixNames = variables // Map[ToString] // Map[StringJoin[#,Unique[]//ToString]&] // Map[ToExpression] // Map[m];
	takePatternLists = (variables//Length)*(irreducibleMonomials//Length)^2//Range // Map[{1,#}&] // Partition[#,(irreducibleMonomials//Length)^2]&;
	Table[FFAlgTake[solverOutput[[1]],cpmatrixNames[[i]],{solvedSystemName},takePatternLists[[i]]],{i,1,takePatternLists//Length}];
	Table[FFGraphOutput[solverOutput[[1]],cpmatrixNames[[i]]],{i,1,takePatternLists//Length}];
	
	Return[solverOutput[[1;;2]]~Join~{solverOutput[[3]][[2]][[2]]}~Join~{cpmatrixNames}~Join~{variables}];
];
