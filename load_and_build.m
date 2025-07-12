(* ::Package:: *)

(* ::Title:: *)
(*Loading and Building (development file)*)


Needs["PacletTools`"];
dir = If[$Notebooks,NotebookDirectory[],Directory[]]


(* ::Section:: *)
(*Loading Package Without Needing to Install*)


PacletDirectoryLoad[dir]


PacletFind["SPQR"] (*not necessary for the next line*)
Get["SPQR`"]


(* ::Section:: *)
(*Building Documentation and Installing*)


(*building documentation*)
PacletDocumentationBuild[dir]


builtPac = PacletBuild[dir]
PacletDirectoryUnload[dir];


installed = PacletInstall[builtPac["PacletArchive"],ForceVersionInstall -> True]


Get["SPQR`"]


?BuildPolynomialSystem


(* ::Section:: *)
(*Uninstalling*)


PacletUninstall["SPQR"]
