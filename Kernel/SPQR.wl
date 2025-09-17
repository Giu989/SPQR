(* ::Package:: *)

BeginPackage["SPQR`",{"FiniteFlow`","GeneralUtilities`"}];


(*SetUsage["FindIrreducibleMonomials[ideal$,vars$] finds the irreducible monomials of an ideal$ in the variables vars$ using a numerical Groebner Basis"]
(*needs a better description*)
SetUsage["BuildPolynomialSystem[targets$,ideal$,vars$,w$] builds and loads a system of linear equations to weight w$ into FiniteFlow to polynomially reduce the targets$ with respect to the ideal$"]*)


(*SetUsage["FindIrreducibleMonomials[ideal$,vars$] finds the irreducible monomials of an ideal$ in the variables vars$ using a numerical Groebner Basis"]*)


FindIrreducibleMonomials::usage = "FindIrreducibleMonomials[ideal,vars] Finds the irreducible monomials of an ideal in the variables vars using a numerical Groebner Basis.";
BuildPolynomialSystem::usage = "BuildPolynomialSystem[targets,ideal,vars,w] Builds and loads a system of linear equations to weight w into FiniteFlow to polynomially reduce the targets with respect to the ideal\n" <> "BuildPolynomialSystem[targets,ideal,vars,{wmin,wmax}] Increases the seed iteratively from wmin until the system closes or wmax is reached";
ReconstructPolynomialRemainder::usage = "ReconstructPolynomialRemainder[system] Reconstructs the remainder of a set of polynomials. system should be the output of BuildPolynomialSystem";
BuildCompanionMatrices::usage = "BuildCompanionMatrices[ideal,vars,w,irreds] Builds and loads a system of linear equations to weight w using the irreducible monomials irreds into FiniteFlow to generate the companion matrices for each of the vars in the ideal\n" <> "BuildCompanionMatrices[ideal,vars,{wmin,wmax},irreds] Increases the seed iteratively from wmin until the system closes or wmax is reached";
BuildTargetCompanionMatrices::usage = "BuildTargetCompanionMatrices[targets,cmatsystem] Builds companion matrices for given target polynomials. cmatsystem should be the output of BuildCompanionMatrices";
ReconstructTargetCompanionMatrices::usage = "ReconstructTargetCompanionMatrices[targetcmatsystem] Reconstructs the remainder of rational functions encoded in the companion matrices. targetcmatsystem should be the output of BuildTargetCompanionMatrices or BuildCompanionMatrices";
BuildCharacteristicPolynomials::usage = "BuildCharacteristicPolynomials[targetcmatsystem] Builds the characteristic polynomials for each companion matrix in targetcmatsystem\n" <> "BuildCharacteristicPolynomials[targetcmatsystem,indexlist] builds the characteristic polynomials of the matrices indexed in indexlist";
ReconstructCharacteristicPolynomials::usage = "ReconstructCharacteristicPolynomials[characteristicpolynomialsystem] reconstructs each coefficient of the characteristic polynomials produced by BuildCharacteristicPolynomials\n" <> "ReconstructCharacteristicPolynomials[characteristicpolynomialsystem,coefficientlist] reconstructs only the terms given in coefficientlist";
FFDet::usage = "FFDet[matrix] computes the determinant of a matrix using the Faddeev\[Dash]LeVerrier algorithm";
SortVariables::usage = "SortVariables[ideal,vars] Sorts variables in the list to try make Groebner Basis computations faster";
j::usage = " ";
extraparam::usage = " ";
targ::usage = " ";
SPQRGraph::usage = " ";
(*m::usage = " ";*)


Begin["`Private`"]


(*identify version*)
With[{pac = PacletFind["SPQR"]},
  If[Length[pac] > 0,
    version = ("Version" //ReplaceAll[pac[[1]][[1]]]);
  ];
];


(*welcome message*)
If[TrueQ@$Notebooks,
	Print["SP\!\(\*TemplateBox[{},\n\"Rationals\"]\)R v" <> version <> ": Vsevolod Chestnov and Giulio Crisanti (2025)"]
,
	Print["SPQR v" <> version <> ": Vsevolod Chestnov and Giulio Crisanti (2025)"]
];
(*documentation button*)
If[TrueQ@$Notebooks,
  Print @ Button["Open documentation",
    Documentation`HelpLookup["paclet:SPQR/guide/SPQR"],
    Method -> "Queued"
  ];
];


(*load in source code*)
Get[FileNameJoin[{DirectoryName[$InputFileName], "various_functions.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "build_system.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "construct_cmat.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "construct_target_cmat.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "characteristic_polynomials.m"}]];


End[]


SetAttributes[
	{
	FindIrreducibleMonomials,BuildPolynomialSystem,ReconstructPolynomialRemainder,
	BuildCompanionMatrices,BuildTargetCompanionMatrices,ReconstructTargetCompanionMatrices,
	j,extraparam,targ,BuildCharacteristicPolynomials,ReconstructCharacteristicPolynomials,FFDet,
	SortVariables,
	Nothing
	}
	, {ReadProtected}
];
(*Protect[{}];*)


EndPackage[]
