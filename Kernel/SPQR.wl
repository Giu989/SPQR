(* ::Package:: *)

BeginPackage["SPQR`",{"FiniteFlow`","GeneralUtilities`"}];


(*SetUsage["FindIrreducibleMonomials[ideal$,vars$] finds the irreducible monomials of an ideal$ in the variables vars$ using a numerical Groebner Basis"]
(*needs a better description*)
SetUsage["BuildPolynomialSystem[targets$,ideal$,vars$,w$] builds and loads a system of linear equations to weight w$ into FiniteFlow to polynomially reduce the targets$ with respect to the ideal$"]*)


(*SetUsage["FindIrreducibleMonomials[ideal$,vars$] finds the irreducible monomials of an ideal$ in the variables vars$ using a numerical Groebner Basis"]*)


FindIrreducibleMonomials::usage = "FindIrreducibleMonomials[ideal,vars] Finds the irreducible monomials of an ideal in the variables vars using a numerical Groebner Basis.";
BuildPolynomialSystem::usage = "BuildPolynomialSystem[targets,ideal,vars,w] Builds and loads a system of linear equations to weight w into FiniteFlow to polynomially reduce the targets with respect to the ideal ";
ReconstructPolynomialRemainder::usage = "ReconstructPolynomialRemainder[system] Reconstructs the remainder of a set of polynomials. system should be the output of BuildPolynomialSystem";
BuildCompanionMatrices::usage = "BuildCompanionMatrices[ideal,vars,w,irreds] Builds and loads a system of linear equations to weight w using the irreducible monomials irreds into FiniteFlow to generate the companion matrices for each of the vars in the ideal ";
BuildTargetCompanionMatrices::usage = "BuildTargetCompanionMatrices[targets,cmatsystem] Builds companion matrices for given target polynomials. cmatsystem should be the output of BuildCompanionMatrices";
ReconstructTargetCompanionMatrices::usage = "ReconstructTargetCompanionMatrices[targetcmatsystem] Reconstructs the remainder of rational functions encoded in the companion matrices. targetcmatsystem should be the output of BuildTargetCompanionMatrices or BuildCompanionMatrices";
BuildCharacteristicPolynomial::usage = " ";
j::usage = " ";
extraparam::usage = " ";
targ::usage = " ";
(*m::usage = " ";*)


Begin["`Private`"]


Print["SP\!\(\*TemplateBox[{},\n\"Rationals\"]\)R: loaded successfully"]


Get[FileNameJoin[{DirectoryName[$InputFileName], "various_functions.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "build_system.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "construct_cmat.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "construct_target_cmat.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "characteristic_polynomials.m"}]];


(*Get["various_functions.m"];
Get["build_system.m"];
Get["construct_cmat.m"];
Get["construct_target_cmat.m"];*)


End[]


SetAttributes[
	{
	FindIrreducibleMonomials,BuildPolynomialSystem,ReconstructPolynomialRemainder,
	BuildCompanionMatrices,BuildTargetCompanionMatrices,ReconstructTargetCompanionMatrices,
	j,extraparam,targ,BuildCharacteristicPolynomial
	}
	, {ReadProtected}
];
(*Protect[{}];*)


EndPackage[]
