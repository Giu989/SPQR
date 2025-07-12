(* ::Package:: *)

BeginPackage["SPQR`",{"FiniteFlow`","GeneralUtilities`"}];


(*SetUsage["FindIrreducibleMonomials[ideal$,vars$] finds the irreducible monomials of an ideal$ in the variables vars$ using a numerical Groebner Basis"]
(*needs a better description*)
SetUsage["BuildPolynomialSystem[targets$,ideal$,vars$,w$] builds and loads a system of linear equations to weight w$ into FiniteFlow to polynomially reduce the targets$ with respect to the ideal$"]*)


(*SetUsage["FindIrreducibleMonomials[ideal$,vars$] finds the irreducible monomials of an ideal$ in the variables vars$ using a numerical Groebner Basis"]*)


FindIrreducibleMonomials::usage = "FindIrreducibleMonomials[ideal,vars] Finds the irreducible monomials of an ideal in the variables vars using a numerical Groebner Basis.";
BuildPolynomialSystem::usage = "BuildPolynomialSystem[targets,ideal,vars,w] Builds and loads a system of linear equations to weight w into FiniteFlow to polynomially reduce the targets with respect to the ideal ";
ReconstructPolynomialRemainder::usage = " ";
BuildCompanionMatrices::usage = " ";
BuildTargetCompanionMatrix::usage = " ";
ReconstructTargetCompanionMatrix::usage = " ";
j::usage = " ";
extraparam::usage = " ";
targ::usage = " ";


Begin["`Private`"]


Print["SP\!\(\*TemplateBox[{},\n\"Rationals\"]\)R: loaded successfully"]


Get[FileNameJoin[{DirectoryName[$InputFileName], "various_functions.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "build_system.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "construct_cmat.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "construct_target_cmat.m"}]];


(*Get["various_functions.m"];
Get["build_system.m"];
Get["construct_cmat.m"];
Get["construct_target_cmat.m"];*)


End[]


SetAttributes[
	{
	FindIrreducibleMonomials,BuildPolynomialSystem,ReconstructPolynomialRemainder,
	BuildCompanionMatrices,BuildTargetCompanionMatrix,ReconstructTargetCompanionMatrix,
	j,extraparam,targ
	}
	, {ReadProtected}];

(*Protect[{}];*)


EndPackage[]
