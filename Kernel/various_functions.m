(* ::Package:: *)

(*(*finding irreducible monomials for a system of equations*)
Options[FindIrreducibleMonomials] = {"MonomialOrder" -> Lexicographic};
FindIrreducibleMonomials[polySystem_,vars_,OptionsPattern[]]:=Module[{gb,leadingexps,maxexp,lessthanLists,intersection,monomials,params,paramsNsub},

	(*numerical substitution of the parameters*)
	params = Complement[polySystem // Variables,vars];
	paramsNsub =params->1+10^-1 (RandomSample[Range[1,10^3+(params//Length)],params//Length]//Map[Prime])(*/(RandomSample[Range[1,10+(params//Length)],params//Length]//Map[Prime])*)//Thread;
	
	gb = GroebnerBasis[polySystem // ReplaceAll[paramsNsub],vars,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions,Modulus->FFPrimeNo[RandomInteger[{0,200}]]];
	PrintTemporary["gb finished"];
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
FindIrreducibleMonomials[polySystem_,OptionsPattern[]]:=FindIrreducibleMonomials[polySystem,polySystem//Variables,OptionsPattern[]];*)


primeList = Range[201]-1//Map[FFPrimeNo];

(*leading exponent vector of each basis element*)
(*leadingExps[gb_,vars_,ord_]:=Exponent[First@MonomialList[#,vars,ord],vars]&/@gb;*)

(*coordinate-wise divisibility test for exponent vectors*)
dividesQ[e_,m_]:=And@@Thread[m>=e];

(*finding irreducible monomials for a system of equations*)
Options[FindIrreducibleMonomials] = {"MonomialOrder" -> Lexicographic,"Sort"->False};
FindIrreducibleMonomials[polySystem_,vars1_,OptionsPattern[]]:=Module[
	{
	ord,params,paramsNsub,gb,leadingexps,lt,n,pure,bounds,todo,seen=<||>,res={},v,vv,i,prime,vars
	},

	ord = OptionValue["MonomialOrder"];
	prime = RandomSample[primeList,1][[1]];
	
	(*numerical substitution of the parameters*)
	params = Complement[polySystem // Variables,vars1];
	(*paramsNsub =params->1+10^-1 (RandomSample[Range[1,10^3+(params//Length)],params//Length]//Map[Prime])(*/(RandomSample[Range[1,10+(params//Length)],params//Length]//Map[Prime])*)//Thread;*)
	paramsNsub = Thread[params->(RandomInteger[{1,10^8+(params//Length)},params//Length]//Map[Prime])];
	
	If[OptionValue["Sort"],
		Print["Warning: sorting monomials. Please use the new ordering provided as the second output of this function"];
		vars = GroebnerBasis`DistributedTermsList[polySystem // ReplaceAll[paramsNsub],vars1,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions,Modulus->prime,Sort->True]//Last;
		,
		vars = vars1;
	];
	
	gb = GroebnerBasis[polySystem // ReplaceAll[paramsNsub],vars,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions,Modulus->prime];
	PrintTemporary["gb finished"];
	
	lt = MonomialList[gb, vars, ord][[;;, 1]] // Map[Exponent[#, vars]&];
	n = Length[vars];
	
	pure=Table[Select[lt,#[[i]]>0&&Total[Drop[#,{i}]]==0&],{i,n}];
	
	(*check if non zero dimensional ideal*)
	(*If[MemberQ[pure,{}],Return[\[Infinity]]];*) (*<--- edge case for gb = {1} this check seems to fail. next line does not*)
	If[Select[lt,(Length[DeleteCases[#,0]]==1)&] // Apply[Plus] // MemberQ[#,0]&, Return[\[Infinity]]];
	
	(*coordinate bounds on dimensions: minimal pure powers + 1*)
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


printDebug[string_,debugWeight_,debugWeightThreshold_]:= If[debugWeight>=debugWeightThreshold,WriteString[$Output,string]];
