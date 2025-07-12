(* ::Package:: *)

(* ::Title:: *)
(*Loading and Building (development file)*)


dir = If[$Notebooks,NotebookDirectory[],Directory[]]


(* ::Section:: *)
(*Loading Package Without Needing to Install*)


Needs["PacletTools`"];
PacletDirectoryLoad[dir]


PacletFind["SPQR"] (*not necessary for the next line*)
Get["SPQR`"]


(* ::Section:: *)
(*Building Documentation*)


(*need to be run every time documentation is updated for changes to register in the package*)
PacletDocumentationBuild[Directory[]]


(* ::Section:: *)
(*Building and Installing (WIP)*)


PacletBuild[Directory[]]
PacletInstall[%]
