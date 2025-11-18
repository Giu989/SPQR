(* ::Package:: *)

(* ::Title:: *)
(*Miscellaneous and Helper Functions*)


(*finding irreducible monomials for a system of equations*)
(*code adapted from "Genealogical Constraints from Master Integral Counting" by G. Crisanti, L. Lippstreu, A. J. McLeod and M. Polackova, to appear soon*)

primeList = Range[201]-1//Map[FFPrimeNo];

(*coordinate wise divisibility test for exponent vectors*)
dividesQ[e_,m_]:=And@@Thread[m>=e];

Options[FindIrreducibleMonomials] = {"MonomialOrder" -> Lexicographic,"Sort"->False};
FindIrreducibleMonomials[polySystem_,vars1_,OptionsPattern[]]:=Module[
	{
		ord,params,paramsNsub,gb,leadingexps,lt,n,pure,bounds,todo,seen=<||>,res={},v,vv,i,prime,vars
	},

	ord = OptionValue["MonomialOrder"];
	prime = RandomSample[primeList,1][[1]];
	
	(*numerical substitution of the parameters*)
	params = Complement[polySystem // Variables,vars1];
	paramsNsub = Thread[params->(RandomInteger[{1,10^8+(params//Length)},params//Length]//Map[Prime])];
	
	If[OptionValue["Sort"],
		Print["Warning: sorting monomials. Please use the new ordering provided as the second output of this function along with the found irreducible monomials"];
		vars = GroebnerBasis`DistributedTermsList[polySystem // ReplaceAll[paramsNsub],vars1,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions,Modulus->prime,Sort->True]//Last;
		,
		vars = vars1;
	];
	
	gb = GroebnerBasis[polySystem // ReplaceAll[paramsNsub],vars,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions,Modulus->prime];
	
	lt = MonomialList[gb, vars, ord][[;;, 1]] // Map[Exponent[#, vars]&];
	n = Length[vars];
	
	pure=Table[Select[lt,#[[i]]>0&&Total[Drop[#,{i}]]==0&],{i,n}];
	
	(*check if non zero dimensional ideal*)
	If[Select[lt,(Length[DeleteCases[#,0]]==1)&] // Apply[Plus] // MemberQ[#,0]&, Return[\[Infinity]]];
	
	(*coordinate bounds on dimensions: minimal pure powers - 1*)
	bounds=(Min/@MapThread[#1[[All,#2]]&,{pure,Range[n]}])-1;
	
	todo={ConstantArray[0,n]};
	
	(*breadth first staircase walk*)
	While[todo=!={},
		v=First@todo;
		todo=Rest@todo;
		If[!KeyExistsQ[seen,v],
			seen[v]=True;
			If[NoneTrue[lt,dividesQ[#,v]&],
				AppendTo[res,Times@@(vars^v)];
				Do[
					If[v[[i]]<bounds[[i]],
						vv=ReplacePart[v,i->v[[i]]+1];
						todo=Append[todo,vv]]
					,{i,n}
				]
			];
		];
	];
	If[OptionValue["Sort"],
		Return[{MonomialList[Plus@@res,vars,ord]//DeleteCases[0],vars}];
	,
		Return[MonomialList[Plus@@res,vars,ord]//DeleteCases[0]];
	];
];


(*finds the monomials appearing inside each polynomial of an eliminated Groebner Basis*)
FindEliminationMonomials[polySystem_,varsToElim_,varsToKeep_] := Module[
	{
		prime,params,paramsNsub,gb,monomialCoordinates,monomials,
		Nothing
	},

	prime = RandomSample[primeList,1][[1]];

	(*numerical substitution of the parameters*)
	params = Complement[polySystem // Variables,Union[varsToElim,varsToKeep]];
	paramsNsub = Thread[params->(RandomInteger[{1,10^8+(params//Length)},params//Length]//Map[Prime])];

	gb = GroebnerBasis[polySystem // ReplaceAll[paramsNsub],varsToKeep,varsToElim,CoefficientDomain->RationalFunctions,Modulus->prime];
	If[Length[gb]==0,Return[{}]];

	monomialCoordinates = gb // MonomialList[#,varsToKeep]& // CoefficientRules[#,varsToKeep]& // Part[#,All,All,All,1]& // Flatten[#,{{1},{2,3}}]&;
	monomials=monomialCoordinates // Map[Power[varsToKeep,#]&,#,{2}]& // Map[Apply[Times],#,{2}]&;

	Return[monomials];
];


Options[SortVariables] = {"MonomialOrder" -> Lexicographic};
SortVariables[polySystem_,vars1_,OptionsPattern[]]:=Module[
	{
		ord,prime,params,paramsNsub,vars
	},

	ord = OptionValue["MonomialOrder"];
	
	(*numerical substitution of the parameters*)
	params = Complement[polySystem // Variables,vars1];
	paramsNsub = Thread[params->(RandomInteger[{1,10^8+(params//Length)},params//Length]//Map[Prime])];
	
	vars = GroebnerBasis`DistributedTermsList[polySystem // ReplaceAll[paramsNsub],vars1,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions,Sort->True]//Last;
	
	Return[vars];
];


printDebug[string_,debugWeight_,debugWeightThreshold_]:= If[debugWeight>=debugWeightThreshold,WriteString[$Output,string]];
