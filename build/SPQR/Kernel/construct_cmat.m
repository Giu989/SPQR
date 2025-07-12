(* ::Package:: *)

Options[BuildCompanionMatrices] = {"MonomialOrder" -> Lexicographic,"PrintDebugInfo"->0};
BuildCompanionMatrices[ideal_,variables_,maxWeight_,irreducibleMonomials_,OptionsPattern[]]:=Module[
	{
	cmatsMonomials,solverOutput,cpmatrixNames,takePatternLists
	}
	,
	cmatsMonomials = Outer[#*irreducibleMonomials&,variables]//Flatten;
	solverOutput = BuildPolynomialSystem[cmatsMonomials,ideal,variables,maxWeight,"IrreducibleMonomials"->irreducibleMonomials,"MonomialOrder"->OptionValue["MonomialOrder"],"PrintDebugInfo"->OptionValue["PrintDebugInfo"]];
	If[solverOutput===$Failed,Return[$Failed]];
	
	(*take companion matrices using FFAlgTake and then return them as outputs to the graph with FFGraphOutput*)
	cpmatrixNames = variables // Map[ToString] // Map[StringJoin["cmat",#]&] // Map[ToExpression] // Map[m];
	takePatternLists = (variables//Length)*(irreducibleMonomials//Length)^2//Range // Map[{1,#}&] // Partition[#,(irreducibleMonomials//Length)^2]&;
	Table[FFAlgTake[solverOutput[[1]],cpmatrixNames[[i]],{"solvedSystem"},takePatternLists[[i]]],{i,1,takePatternLists//Length}];
	Table[FFGraphOutput[solverOutput[[1]],cpmatrixNames[[i]]],{i,1,takePatternLists//Length}];
	
	Return[solverOutput[[1;;3]]~Join~{cpmatrixNames}~Join~{variables}];
];
