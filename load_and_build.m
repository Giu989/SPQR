(* ::Package:: *)

(* ::Title:: *)
(*Loading and Building (development file)*)


(* ::Section:: *)
(*Loading Package Without Needing to Install*)


Needs["PacletTools`"];
dir = If[$Notebooks,NotebookDirectory[],Directory[]]


PacletDirectoryLoad[dir]


PacletFind["SPQR"] (*not necessary for the next line*)
Get["SPQR`"]


?FindIrreducibleMonomials


?BuildPolynomialSystem


?ReconstructPolynomialRemainder


?BuildCompanionMatrices


(* ::Section:: *)
(*Building Documentation and Installing*)


Needs["PacletTools`"];
dir = If[$Notebooks,NotebookDirectory[],Directory[]]


(*building documentation*)
PacletDocumentationBuild[dir]


builtPac = PacletBuild[dir] // Quiet
PacletDirectoryUnload[dir];


installed = PacletInstall[builtPac["PacletArchive"],ForceVersionInstall -> True]


Get["SPQR`"]


?FindIrreducibleMonomials


?BuildPolynomialSystem


(* ::Section:: *)
(*Uninstalling*)


PacletUninstall["SPQR"]
