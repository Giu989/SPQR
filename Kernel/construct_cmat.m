(* ::Package:: *)

Options[BuildCompanionMatrices] = {"MonomialOrder" -> Lexicographic,"PrintDebugInfo"->0, "ExtraParams"->{}};
BuildCompanionMatrices[ideal_,variables_,maxWeight_,irreducibleMonomials_,OptionsPattern[]]:=Module[
	{
		cmatsMonomials,solverOutput,cpmatrixNames,takePatternLists,uniqueparam,
		Nothing
	},
	
	cmatsMonomials = Outer[#*irreducibleMonomials&,variables]//Flatten;
	solverOutput = BuildPolynomialSystem[cmatsMonomials,ideal,variables,maxWeight,
											"IrreducibleMonomials"->irreducibleMonomials,"MonomialOrder"->OptionValue["MonomialOrder"],
											"PrintDebugInfo"->OptionValue["PrintDebugInfo"],"ExtraParams"->OptionValue["ExtraParams"]
										];
	
	If[Head[solverOutput]===$Failed,Return[$Failed]];
	
	If[Length[solverOutput[[3]][[2]][[2]]]!=Length[irreducibleMonomials],
		uniqueparam = ToExpression[Unique["dummyparam"]];
		Print["Warning: Not all irreducible monomials targeted during divison, SPQR will faill to build companion matrices"];
		Print["This is a known bug. As a temporary fix, add the equation: ", uniqueparam - 1, " to the ideal as well as ", uniqueparam, " to the list of variables"];
		Print["Check also if each entry in the ideal factorises"];
		Return[$Failed];
	];
	
	(*take companion matrices using FFAlgTake and then return them as outputs to the graph with FFGraphOutput*)
	cpmatrixNames = variables // Map[ToString] // Map[StringJoin[#,Unique[]//ToString]&] // Map[ToExpression] // Map[m];
	takePatternLists = (variables//Length)*(irreducibleMonomials//Length)^2//Range // Map[{1,#}&] // Partition[#,(irreducibleMonomials//Length)^2]&;
	Table[FFAlgTake[solverOutput[[1]],cpmatrixNames[[i]],{"solvedSystem"},takePatternLists[[i]]],{i,1,takePatternLists//Length}];
	Table[FFGraphOutput[solverOutput[[1]],cpmatrixNames[[i]]],{i,1,takePatternLists//Length}];
	
	Return[solverOutput[[1;;2]]~Join~{solverOutput[[3]][[2]][[2]]}~Join~{cpmatrixNames}~Join~{variables}];
];
