(* ::Package:: *)

(* ::Title:: *)
(*System Builder and Solver*)


Condition[j[a___]*j[b___],Length[{a}] === Length[{b}]]^:=Apply[j,List[a]+List[b]];
j[a___]^n_^:=Apply[j,n*List[a]];


complementUnsorted[l1_,l2_] := Delete[l1,l2//ReplaceAll[PositionIndex[l1]]//DeleteCases[x_/;!ListQ[x]]];


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
	If[(Length[variables] == Length[ordering]) && (ordering // SquareMatrixQ) && (ordering // Flatten // Map[rationalQ] // Apply[And]) && (ordering // Transpose // Map[Max] // Thread[#>0]& // Apply[And]),
		Return[ordering]
	,
		Print["Warning: provided matrix is not a valid ordering"];
	];
	(*failsafe*)
	Print["Warning: No valid ordering specified"];
	Abort[];
];
generateWeightMatrix[variables1_,variables2_,ordering_] := BlockDiagonalMatrix[{generateWeightMatrix[variables2,ordering],generateWeightMatrix[variables1,ordering]}] // Normal;

ClearAll @ uniqueName;
uniqueName[{}, head_String] := head <> "1";
Condition[
    uniqueName[names_List, head_String],
    And[
        Length[names] > 0,
        names // AllTrue[StringQ],
        True
    ]
] := Module[{ids},
    ids = names // Map[StringDelete[head]] // ToExpression // Quiet; 
    If[ids // AllTrue[IntegerQ],
        Return[
            ids // Sort // Last // # + 1& // ToString // head <> #&
        ]
    ,
        Print["Error! Wrong names in unqueName: ", names];
        Throw[$Failed];
    ];
    (* names // Sort // Last // # + 1& // ToString // head <> #&; *)
];
uniqueName[xs__] := (
    Print["Error! Wrong arguments in uniqueName: ", {xs}];
    Throw[$Failed];
);


createSeeds[length_,weight_]:=Join @@ Permutations /@ IntegerPartitions[weight, {length}, Range[0, weight]];
createAllSeeds[length_,maxweight_]:=createSeeds[length,#]&/@Range[0,maxweight]//Flatten[#,1]&;

Options[BuildPolynomialSystem] = {"MonomialOrder" -> DegreeReverseLexicographic,"ExtraParams"->{}, "LinkGraph" -> <||>};
(**)
BuildPolynomialSystem[targets_,ideal_,variables_,opts : OptionsPattern[]]:= Module[
	{
	idealj,targetsj,jseeds,equations,sortedMonomials,sortedMonomialsj,monomials,systemSparse,
	params,nonzeromat,adjlists,graphName,learn,newEqnNumb,irreducibleMonomials,(*targ,*)(*extraparam,*)
	weightMatrix,varsToTrim,varsNotToTrim,trimDegree,positions,monomialsInIdeal,jseedMonomialsOuter,
	idealCoefficientMatrix,coefficients,tmp1,tmp2,tmp3,associationRules,systemAssociation,targetAssociation,
	valuesUniqueToPositions,numberOfRows,numberOfCols,valuesUnique,indexUniqueToPosition,triples,byI,pivots,
	adjLists,reverseIndex,takePattern,monomialsInTarget,printDebug1,tm,learn1
	},
	(*only prints the statement if the level set in the option value is equal to or higher than the second option here*)
	printDebug1[a_,c_] := printDebug[a,OptionValue["PrintDebugInfo"],c];
	
	ClearAll @ uniqueName;
	uniqueName[{}, head_String] := head <> "1";
	Condition[
    uniqueName[names_List, head_String],
    And[
        Length[names] > 0,
        names // AllTrue[StringQ],
        True
    ]
	] := Module[{ids},
    ids = names // Map[StringDelete[head]] // ToExpression // Quiet; 
    If[ids // AllTrue[IntegerQ],
        Return[
            ids // Sort // Last // # + 1& // ToString // head <> #&
        ]
    ,
        Print["Error! Wrong names in unqueName: ", names];
        Throw[$Failed];
    ];
    (* names // Sort // Last // # + 1& // ToString // head <> #&; *)
	];
	uniqueName[xs__] := (
    Print["Error! Wrong arguments in uniqueName: ", {xs}];
    Throw[$Failed];
	);
	
	solvedSystemNames = Cases[
	    FiniteFlow`Private`FFGraphNodes[graphName],
	    x_ /; StringMatchQ[ToString[x],"solvedSystem" ~~ ___]
    ];
	solvedSystemNames // Map[FFDeleteNode[graphName,#]&];
	(* solvedSystemName = "solvedSystem" // Unique; *)
	solvedSystemName = uniqueName[solvedSystemNames, "solvedSystem"];
	
	(*parameters of system*)
	params = Join[Complement[Join[ideal // Variables, targets // Variables],variables],OptionValue["ExtraParams"]];
	If[Length[params]<2,params=params~Join~{extraparam}];
	
	graphName = Unique[SPQRGraph]//ToString;
    FFDeleteGraph[graphName];
    FFNewGraph[graphName,"in",params];
    
    FFAlgPolyDiv[graphName,solvedSystemName,targets,ideal,variables,params];
    FFGraphOutput[graphName,solvedSystemName];
    learn1=FFPolyDivLearn[graphName,variables];
    learn= {
    "DepVars"->(targets//Length//Range//Map[targ]),
    "IndepVars"->(learn1//CoefficientRules[#,variables][[;;,1,1]]&//MapApply[j]),
    "SparseOutput"->False};
	
	Return[{graphName,params,learn,variables}];
];


Options[ReconstructPolynomialRemainder] = {"Vector" -> False,"PrintDebugInfo"->1,"DeleteGraph"->True,"NThreads"->FFNThreads};
ReconstructPolynomialRemainder[output_List,OptionsPattern[]] := Module[{reconstructed,ans},
	reconstructed = FFReconstructFunction[output[[1]],output[[2]],"PrintDebugInfo"->OptionValue["PrintDebugInfo"],"MaxPrimes"->200,"MaxDegree"->1000,"NThreads"->OptionValue["NThreads"]];
	If[OptionValue["Vector"],
		ans=ArrayReshape[reconstructed,{"DepVars","IndepVars"}//ReplaceAll[output[[3]]]//Map[Length]];
	,
		ans=ArrayReshape[reconstructed,{"DepVars","IndepVars"}//ReplaceAll[output[[3]]]//Map[Length]] // Dot[#,"IndepVars"//ReplaceAll[output[[3]]]]& // ReplaceAll[j[x__]:>Times@@(output[[4]]^{x})];
	];
	If[OptionValue["DeleteGraph"],FFDeleteGraph[output[[1]]//Evaluate]];
	Return[ans];
];
