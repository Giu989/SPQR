(* ::Package:: *)

(*tests for now*)


vars = {x,y,z};
monomials = {x^3,x^2*y^2,x*y^2*z,z^2,y^2,x*z};


lexW = {{1,0,0},{0,1,0},{0,0,1}};
deglexW = {{1,1,1},{0,-1,-1},{0,0,-1}}; (*seems wrong?*)
degrevlexW = {{1,1,1},{0,0,-1},{0,-1,0}};


a1=MonomialList[monomials//Apply[Plus],vars,Lexicographic]
a2=MonomialList[monomials//Apply[Plus],vars,DegreeLexicographic]
a3=MonomialList[monomials//Apply[Plus],vars,DegreeReverseLexicographic]


b1=MonomialList[monomials//Apply[Plus],vars,lexW]
b2=MonomialList[monomials//Apply[Plus],vars,deglexW]
b3=MonomialList[monomials//Apply[Plus],vars,degrevlexW]


a1==b1
a2==b2
a3==b3


monomialSortFunction[exponentVectors_,weightMatrix_]:=Module[{},
	weightVectors = weightMatrix . Transpose[exponentVectors];
	tieBreakLocation = FirstPosition[weightVectors // Map[Apply[Equal]],False,1] // Apply[Identity];
	weightVectors // Part[#, tieBreakLocation]& // Apply[Greater] // Return;
];


sortMonomialsWithWeightMatrix[jList_,weightMatrix_]:=Module[{},
	(*todo: check square matrix and full rank as well as first column index negative condition*)
	exponentVectors = jList // ReplaceAll[j[x___]:>List[x]];
	exponentVectors // Sort[#, monomialSortFunction[{#1,#2},weightMatrix]&]& // Map[Apply[j]] // Return;
];


monomials // CoefficientRules[#,vars]& // Flatten // Part[#,All,1]& // Map[Apply[j]] // sortMonomialsWithWeightMatrix[#,lexW]& // ReplaceAll[j[x___]:>vars^{x}] // Map[Apply[Times]]
monomials // CoefficientRules[#,vars]& // Flatten // Part[#,All,1]& // Map[Apply[j]] // sortMonomialsWithWeightMatrix[#,deglexW]& // ReplaceAll[j[x___]:>vars^{x}] // Map[Apply[Times]]
monomials // CoefficientRules[#,vars]& // Flatten // Part[#,All,1]& // Map[Apply[j]] // sortMonomialsWithWeightMatrix[#,degrevlexW]& // ReplaceAll[j[x___]:>vars^{x}] // Map[Apply[Times]]


IdentityMatrix[3] // Reverse // MatrixForm


lexW // MatrixForm
deglexW // MatrixForm
degrevlexW // MatrixForm


MatrixForm[UpperTriangularize[{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}]]


ConstantArray[1,{3,3}] // UpperTriangularize // Times[#,{1,-1,-1}]& // MatrixForm


generateWeightMatrix[variables_,ordering_] := Module[{},
	If[(variables // Length) == 1,
		Return[{{1}}];
	];
	If[ordering === Lexicographic,
		IdentityMatrix[variables//Length] // Return;
	];
	If[ordering === DegreeLexicographic,
		ConstantArray[1,{variables // Length,variables // Length}] // UpperTriangularize // Times[#, {1}~Join~ConstantArray[-1, (variables // Length)-1]]& // Return;
	];
	If[ordering === DegreeReverseLexicographic,
		Join[ConstantArray[1,variables // Length] // List,-IdentityMatrix[(variables // Length)-1] // Reverse] // PadLeft // Return;
	];
	(*implement failsafe here*)
	Print["Error: No valid ordering specified"];
	Return[$Failed];
];
generateWeightMatrix[variables1_,variables2_,ordering_] := BlockDiagonalMatrix[{generateWeightMatrix[variables2,ordering],generateWeightMatrix[variables1,ordering]}] // Normal;


(*note: to keep conventions with mathematica, the second set of variables, variables2 are eliminated. so they go first in the elimination order*)


generateWeightMatrix[{w,x,y,z},Lexicographic] // MatrixForm


poly = a*x^2 + b*y^2 + x*y + 3 + x^2y+c*x*y+z^2+x*z+z^3;
dx = D[poly,x];
dy = D[poly,y];
dz = D[poly,z];


GroebnerBasis[{poly,dx,dy,dz},{x,y,z,a},MonomialOrder->Lexicographic,CoefficientDomain->RationalFunctions] // First // Factor
GroebnerBasis[{poly,dx,dy,dz},{a},{x,y,z},CoefficientDomain->RationalFunctions] // First // Factor
GroebnerBasis[{poly,dx,dy,dz},{x,y,z,a},MonomialOrder->generateWeightMatrix[{a},{x,y,z},DegreeReverseLexicographic],CoefficientDomain->RationalFunctions] // First // Factor


GroebnerBasis[{poly,dx,dy,dz},{x,y,z,a},MonomialOrder->Lexicographic,CoefficientDomain->RationalFunctions] // Factor // ByteCount
GroebnerBasis[{poly,dx,dy,dz},{a},{x,y,z},CoefficientDomain->RationalFunctions] // Factor // ByteCount
GroebnerBasis[{poly,dx,dy,dz},{x,y,z,a},MonomialOrder->generateWeightMatrix[{a},{x,y,z},DegreeReverseLexicographic],CoefficientDomain->RationalFunctions] // Factor // ByteCount


GroebnerBasis[{poly,dx,dy,dz},{x,y,z,a},MonomialOrder->generateWeightMatrix[{a},{x,y,z},DegreeLexicographic],CoefficientDomain->RationalFunctions] // Factor // ByteCount


generateWeightMatrix[{a},{x,y,z},DegreeLexicographic]


variables = {x1,x2,x3,x4,x5,x6,x7,x8,newvar};
pList = {1-newvar x1 x5-newvar x2 x5+newvar x1 x2 x5-newvar x3 x5+2 newvar x1 x3 x5-newvar x4 x5+11 newvar x1 x4 x5+5 newvar x2 x4 x5-newvar x1 x6-newvar x2 x6+newvar x1 x2 x6-newvar x3 x6+2 newvar x1 x3 x6-newvar x4 x6+11 newvar x1 x4 x6+5 newvar x2 x4 x6-newvar x1 x7-newvar x2 x7+newvar x1 x2 x7-newvar x3 x7+2 newvar x1 x3 x7-newvar x4 x7+11 newvar x1 x4 x7+5 newvar x2 x4 x7-newvar x5 x7+3 newvar x2 x5 x7+7 newvar x3 x5 x7-newvar x6 x7+newvar x2 x6 x7+2 newvar x3 x6 x7+11 newvar x4 x6 x7-newvar x1 x8-newvar x2 x8+newvar x1 x2 x8-newvar x3 x8+2 newvar x1 x3 x8-newvar x4 x8+11 newvar x1 x4 x8+5 newvar x2 x4 x8-newvar x5 x8+11 newvar x1 x5 x8+5 newvar x2 x5 x8-newvar x6 x8-8 newvar x2 x6 x8-16 newvar x3 x6 x8,x5-x2 x5-2 x3 x5-11 x4 x5+x6-x2 x6-2 x3 x6-11 x4 x6+x7-x2 x7-2 x3 x7-11 x4 x7+x8-x2 x8-2 x3 x8-11 x4 x8-11 x5 x8,x5-x1 x5-5 x4 x5+x6-x1 x6-5 x4 x6+x7-x1 x7-5 x4 x7-3 x5 x7-x6 x7+x8-x1 x8-5 x4 x8-5 x5 x8+8 x6 x8,x5-2 x1 x5+x6-2 x1 x6+x7-2 x1 x7-7 x5 x7-2 x6 x7+x8-2 x1 x8+16 x6 x8,x5-11 x1 x5-5 x2 x5+x6-11 x1 x6-5 x2 x6+x7-11 x1 x7-5 x2 x7-11 x6 x7+x8-11 x1 x8-5 x2 x8,x1+x2-x1 x2+x3-2 x1 x3+x4-11 x1 x4-5 x2 x4+x7-3 x2 x7-7 x3 x7+x8-11 x1 x8-5 x2 x8,x1+x2-x1 x2+x3-2 x1 x3+x4-11 x1 x4-5 x2 x4+x7-x2 x7-2 x3 x7-11 x4 x7+x8+8 x2 x8+16 x3 x8,x1+x2-x1 x2+x3-2 x1 x3+x4-11 x1 x4-5 x2 x4+x5-3 x2 x5-7 x3 x5+x6-x2 x6-2 x3 x6-11 x4 x6,x1+x2-x1 x2+x3-2 x1 x3+x4-11 x1 x4-5 x2 x4+x5-11 x1 x5-5 x2 x5+x6+8 x2 x6+16 x3 x6};


generateWeightMatrix[variables,Lexicographic]


GroebnerBasis[pList,variables,MonomialOrder->generateWeightMatrix[variables,Lexicographic]]
GroebnerBasis[pList,variables,MonomialOrder->generateWeightMatrix[variables[[9;;9]],variables[[1;;8]],DegreeReverseLexicographic]]


monsList = pList // MonomialList // Flatten // CoefficientRules[#,variables]& // Flatten // Part[#,;;,1]& // Map[Apply[j]];


sortMonomialsWithWeightMatrix[monsList,generateWeightMatrix[variables,Lexicographic]];
sortMonomialsWithWeightMatrix[monsList,generateWeightMatrix[variables[[9;;9]],variables[[1;;8]],DegreeReverseLexicographic]];
%==%% // TrueQ


generateWeightMatrix[variables[[9;;9]],variables[[1;;8]],DegreeReverseLexicographic] // MatrixForm


degrevlexW // MatrixForm
