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


(*leading exponent vector of each basis element*)
leadingExps[gb_,vars_,ord_]:=Exponent[First@MonomialList[#,vars,ord],vars]&/@gb;

(*coordinate-wise divisibility test for exponent vectors*)
dividesQ[e_,m_]:=And@@Thread[m>=e];

(*finding irreducible monomials for a system of equations*)
Options[FindIrreducibleMonomials] = {"MonomialOrder" -> Lexicographic};
FindIrreducibleMonomials[polySystem_,vars_,OptionsPattern[]]:=Module[{ord,params,paramsNsub,gb,leadingexps,lt,n,pure,bounds,todo,seen=<||>,res={},v,vv,i},

	ord = OptionValue["MonomialOrder"];
	
	(*numerical substitution of the parameters*)
	params = Complement[polySystem // Variables,vars];
	(*paramsNsub =params->1+10^-1 (RandomSample[Range[1,10^3+(params//Length)],params//Length]//Map[Prime])(*/(RandomSample[Range[1,10+(params//Length)],params//Length]//Map[Prime])*)//Thread;*)
	paramsNsub =params->RandomInteger[{1,10^20+(params//Length)},params//Length]//Thread;
	
	gb = GroebnerBasis[polySystem // ReplaceAll[paramsNsub],vars,MonomialOrder->OptionValue["MonomialOrder"],CoefficientDomain->RationalFunctions,Modulus->FFPrimeNo[RandomInteger[{0,200}]]];
	PrintTemporary["gb finished"];
	
	lt=leadingExps[gb,vars,ord];
	n=Length[vars];
	
	pure=Table[Select[lt,#[[i]]>0&&Total[Drop[#,{i}]]==0&],{i,n}];
	If[MemberQ[pure,{}],Return[\[Infinity]]];
	
	(*check if non zero dimensional ideal*)
	If[MemberQ[pure,{}],Return[\[Infinity]]];
	
	(*coordinate bounds on dimensions: minimal pure powers + 1*)
	bounds=(Min/@MapThread[#1[[All,#2]]&,{pure,Range[n]}])-1;
	
	todo={ConstantArray[0,n]};
	
	(*breadth first walk inside staircase*)
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
	Return[MonomialList[Plus@@res,vars,ord]];
];


printDebug[string_,debugWeight_,debugWeightThreshold_]:= If[debugWeight>=debugWeightThreshold,WriteString[$Output,string]];
