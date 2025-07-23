(* ::Package:: *)

(*finding irreducible monomials for a system of equations*)
Options[FindIrreducibleMonomials] = {"MonomialOrder" -> Lexicographic};
FindIrreducibleMonomials[polySystem_,vars_,OptionsPattern[]]:=Module[{gb,leadingexps,maxexp,lessthanLists,intersection,monomials,params,paramsNsub},

	(*numerical substitution of the parameters*)
	params = Complement[polySystem // Variables,vars];
	paramsNsub =params->1+10^-1 (RandomSample[Range[1,10^3+(params//Length)],params//Length]//Map[Prime])(*/(RandomSample[Range[1,10+(params//Length)],params//Length]//Map[Prime])*)//Thread;
	
	gb = GroebnerBasis[polySystem // ReplaceAll[paramsNsub],vars,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions];
	leadingexps = MonomialList[gb, vars, OptionValue["MonomialOrder"]][[;;, 1]] // Map[Exponent[#, vars]&];

	(*checkIfInfinite*)
	If[Select[leadingexps,(Length[DeleteCases[#,0]]==1)&] // Apply[Plus] // MemberQ[#,0]&, Return[\[Infinity]]];

	(*compute irreducible monomials*)
	maxexp = (leadingexps // Flatten // Max);
	lessthanLists=leadingexps // Map[Function[exp,Select[Tuples[Range[0, maxexp],{vars // Length}],Less[#,exp]& /* Thread /* Apply[Or]]]];
	intersection = lessthanLists // Apply[Intersection];
	monomials = intersection // Map[Power[vars,#]&] // Map[Apply[Times]] // Apply[Plus] // MonomialList[#,vars,OptionValue["MonomialOrder"]]&;
	Return[monomials];
];
(*below currently broken*)
FindIrreducibleMonomials[polySystem_,OptionsPattern[]]:=FindIrreducibleMonomials[polySystem,polySystem//Variables,OptionsPattern[]];


printDebug[string_,debugWeight_,debugWeightThreshold_]:= If[debugWeight>=debugWeightThreshold,WriteString[$Output,string]];
