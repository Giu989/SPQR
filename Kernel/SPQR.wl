(* ::Package:: *)

BeginPackage["SPQR`",{"FiniteFlow`"}];


findIrreducibleMonomials::usage = " ";
solveSystem::usage = "Builds a system of equations for a given Ideal and returns the irreducible monomials";
reconstructSystemOutput::usage = " ";
constructCompanionMatrices::usage = " ";
constructTargetCmat::usage = " ";
ffPolyDiv::usage = " ";
j::usage = " ";
extraparam::usage = " ";
targ::usage = " ";


Begin["`Private`"]


Print["loaded successfully"]


Get["various_functions.m"];
Get["build_system.m"];
Get["construct_cmat.m"];
Get["construct_target_cmat.m"];


End[]


SetAttributes[{solveSystem}, ReadProtected];

(*Protect[{solveSystem}];*)


EndPackage[]
