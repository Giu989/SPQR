(* ::Package:: *)

BeginPackage["SPQR`",{"FiniteFlow`"}];


FindIrreducibleMonomials::usage = " ";
BuildPolynomialSystem::usage = "Builds a system of equations for a given Ideal and returns the irreducible monomials";
ReconstructPolynomialRemainder::usage = " ";
BuildCompanionMatrices::usage = " ";
BuildTargetCompanionMatrix::usage = " ";
ReconstructTargetCompanionMatrix::usage = " ";
j::usage = " ";
extraparam::usage = " ";
targ::usage = " ";


Begin["`Private`"]


Print["test print output"]


Get[FileNameJoin[{DirectoryName[$InputFileName], "various_functions.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "build_system.m"}]];
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
	, ReadProtected];

(*Protect[{}];*)


EndPackage[]
