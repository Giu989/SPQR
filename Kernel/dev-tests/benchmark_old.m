(* ::Package:: *)

(* ::Title:: *)
(*BenchMarker*)


(* ::Section:: *)
(*Helper Functions*)


<<FiniteFlow`


Condition[j[a___]*j[b___],Length[{a}] === Length[{b}]]^:=Apply[j,List[a]+List[b]];
j[a___]^n_^:=Apply[j,n*List[a]];


complementUnsorted[l1_,l2_]:=Delete[l1,l2//ReplaceAll[PositionIndex[l1]]//DeleteCases[x_/;!ListQ[x]]];


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
	(*implement failsafe here*)
	Print["Error: No valid ordering specified"];
	Return[$Failed];
];
generateWeightMatrix[variables1_,variables2_,ordering_] := BlockDiagonalMatrix[{generateWeightMatrix[variables2,ordering],generateWeightMatrix[variables1,ordering]}] // Normal;


createSeeds[length_,weight_]:=Join @@ Permutations /@ IntegerPartitions[weight, {length}, Range[0, weight]];
createAllSeeds[length_,maxweight_]:=createSeeds[length,#]&/@Range[0,maxweight]//Flatten[#,1]&;


(* ::Section:: *)
(*System Setup*)


pList = {1 + Rational[-1, 16] x0 (9255 x10^2 - 54 s16 x10^2 - 112 s16^2 x10^2 - 2283 s56 x10^2 + 150 s16 s56 x10^2 + 7 s16^2 s56 x10^2 - 33 s56^2 x10^2 - 6 s16 s56^2 x10^2 + 30480 x10 x11 - 3665 s16 x10 x11 + 26 s16^2 x10 x11 - 9942 s56 x10 x11 + 1433 s16 s56 x10 x11 - 42 s16^2 s56 x10 x11 + 426 s56^2 x10 x11 - 99 s16 s56^2 x10 x11 + 4 s16^2 s56^2 x10 x11 + 42 s56^3 x10 x11 - 3 s16 s56^3 x10 x11 + 12284 x10^2 x11 - 1364 s16 x10^2 x11 - 1996 s56 x10^2 x11 + 228 s16 s56 x10^2 x11 - 100 s56^2 x10^2 x11 - 4 s16 s56^2 x10^2 x11 + 18765 x11^2 - 2859 s16 x11^2 + 86 s16^2 x11^2 - 6092 s56 x11^2 + 911 s16 s56 x11^2 - 28 s16^2 s56 x11^2 + 129 s56^2 x11^2 - 39 s16 s56^2 x11^2 + 2 s16^2 s56^2 x11^2 + 65 s56^3 x11^2 - 5 s16 s56^3 x11^2 + 21712 x10 x11^2 - 2020 s16 x10 x11^2 - 36 s16^2 x10 x11^2 - 1740 s56 x10 x11^2 - 24 s16 s56 x10 x11^2 + 22 s16^2 s56 x10 x11^2 - 692 s56^2 x10 x11^2 + 70 s16 s56^2 x10 x11^2 - 2 s16^2 s56^2 x10 x11^2 + 28 s56^3 x10 x11^2 - 2 s16 s56^3 x10 x11^2 + 1196 x10^2 x11^2 + 260 s56 x10^2 x11^2 - 52 s56^2 x10^2 x11^2 + 13428 x11^3 - 1816 s16 x11^3 + 36 s16^2 x11^3 - 2104 s56 x11^3 + 262 s16 s56 x11^3 - 4 s16^2 s56 x11^3 - 140 s56^2 x11^3 + 10 s16 s56^2 x11^3 + 4576 x10 x11^3 - 468 s16 x10 x11^3 - 208 s56 x10 x11^3 + 52 s16 s56 x10 x11^3 + 2340 x11^4 - 260 s16 x11^4 + 17730 x10 x12 - 1035 s16 x10 x12 - 106 s16^2 x10 x12 - 4647 s56 x10 x12 + 390 s16 s56 x10 x12 + 10 s16^2 s56 x10 x12 + 66 s56^2 x10 x12 - 21 s16 s56^2 x10 x12 + 9 s56^3 x10 x12 + 3684 x10^2 x12 - 332 s16 x10^2 x12 + 252 s56 x10^2 x12 + 32 s16 s56 x10^2 x12 - 12 s56^2 x10^2 x12 + 26055 x11 x12 - 3453 s16 x11 x12 + 62 s16^2 x11 x12 - 8394 s56 x11 x12 + 1181 s16 s56 x11 x12 - 30 s16^2 s56 x11 x12 + 363 s56^2 x11 x12 - 89 s16 s56^2 x11 x12 + 4 s16^2 s56^2 x11 x12 + 54 s56^3 x11 x12 - 3 s16 s56^3 x11 x12 + 35964 x10 x11 x12 - 4072 s16 x10 x11 x12 + 20 s16^2 x10 x11 x12 - 4024 s56 x10 x11 x12 + 568 s16 s56 x10 x11 x12 - 10 s16^2 s56 x10 x11 x12 - 440 s56^2 x10 x11 x12 + 20 s16 s56^2 x10 x11 x12 + 6 s56^3 x10 x11 x12 + 1016 x10^2 x11 x12 + 456 s56 x10^2 x11 x12 - 8 s56^2 x10^2 x11 x12 + 36864 x11^2 x12 - 5230 s16 x11^2 x12 + 126 s16^2 x11^2 x12 - 5992 s56 x11^2 x12 + 884 s16 s56 x11^2 x12 - 26 s16^2 s56 x11^2 x12 - 254 s56^2 x11^2 x12 + 16 s16 s56^2 x11^2 x12 + 10384 x10 x11^2 x12 - 1068 s16 x10 x11^2 x12 + 308 s56 x10 x11^2 x12 + 32 s16 s56 x10 x11^2 x12 + 9972 x11^3 x12 - 1360 s16 x11^3 x12 + 28 s16^2 x11^3 x12 + 7290 x12^2 - 594 s16 x12^2 - 24 s16^2 x12^2 - 2052 s56 x12^2 + 195 s16 s56 x12^2 + 3 s16^2 s56 x12^2 + 84 s56^2 x12^2 - 15 s16 s56^2 x12^2 + 9 s56^3 x12^2 + 9252 x10 x12^2 - 852 s16 x10 x12^2 - 16 s16^2 x10 x12^2 - 384 s56 x10 x12^2 + 76 s16 s56 x10 x12^2 - 60 s56^2 x10 x12^2 - 180 x10^2 x12^2 + 36 s56 x10^2 x12^2 + 28944 x11 x12^2 - 3972 s16 x11 x12^2 + 84 s16^2 x11 x12^2 - 4662 s56 x11 x12^2 + 706 s16 s56 x11 x12^2 - 22 s16^2 s56 x11 x12^2 - 132 s56^2 x11 x12^2 + 6 s16 s56^2 x11 x12^2 + 6240 x10 x11 x12^2 - 636 s16 x10 x11 x12^2 + 624 s56 x10 x11 x12^2 - 20 s16 s56 x10 x11 x12^2 + 13896 x11^2 x12^2 - 2048 s16 x11^2 x12^2 + 56 s16^2 x11^2 x12^2 + 5508 x12^3 - 558 s16 x12^3 - 6 s16^2 x12^3 - 774 s56 x12^3 + 84 s16 s56 x12^3 - 18 s56^2 x12^3 + 432 x10 x12^3 - 36 s16 x10 x12^3 + 108 s56 x10 x12^3 + 7236 x11 x12^3 - 1056 s16 x11 x12^3 + 28 s16^2 x11 x12^3 + 972 x12^4 - 108 s16 x12^4 + 4575 x10 x13 - 678 s16 x10 x13 - 1497 s56 x10 x13 + 204 s16 s56 x10 x13 + 63 s56^2 x10 x13 - 12 s16 s56^2 x10 x13 + 9 s56^3 x10 x13 - 108 x10^2 x13 - 448 s16 x10^2 x13 + 300 s56 x10^2 x13 + 28 s16 s56 x10^2 x13 - 12 s56^2 x10^2 x13 + 1695 x11 x13 - 334 s16 x11 x13 + 111 s56 x11 x13 + 38 s16 s56 x11 x13 - 183 s56^2 x11 x13 + 8 s16 s56^2 x11 x13 + 15 s56^3 x11 x13 - 4196 x10 x11 x13 - 112 s16 x10 x11 x13 + 2372 s56 x10 x11 x13 - 154 s16 s56 x10 x11 x13 - 282 s56^2 x10 x11 x13 + 14 s16 s56^2 x10 x11 x13 + 6 s56^3 x10 x11 x13 - 2728 x10^2 x11 x13 + 456 s56 x10^2 x11 x13 - 8 s56^2 x10^2 x11 x13 + 992 x11^2 x13 - 240 s16 x11^2 x13 - 542 s56 x11^2 x13 + 58 s16 s56 x11^2 x13 + 90 s56^2 x11^2 x13 - 10 s16 s56^2 x11^2 x13 - 1428 x10 x11^2 x13 - 72 s16 x10 x11^2 x13 - 380 s56 x10 x11^2 x13 + 44 s16 s56 x10 x11^2 x13 + 56 s56^2 x10 x11^2 x13 - 4 s16 s56^2 x10 x11^2 x13 + 100 x11^3 x13 - 40 s16 x11^3 x13 - 280 s56 x11^3 x13 + 20 s16 s56 x11^3 x13 + 2970 x12 x13 - 444 s16 x12 x13 - 849 s56 x12 x13 + 132 s16 s56 x12 x13 + 12 s56^2 x12 x13 - 12 s16 s56^2 x12 x13 + 9 s56^3 x12 x13 + 2244 x10 x12 x13 - 644 s16 x10 x12 x13 + 108 s56 x10 x12 x13 + 56 s16 s56 x10 x12 x13 - 84 s56^2 x10 x12 x13 - 664 x10^2 x12 x13 + 64 s56 x10^2 x12 x13 + 3714 x11 x12 x13 - 612 s16 x11 x12 x13 - 894 s56 x11 x12 x13 + 96 s16 s56 x11 x12 x13 + 54 s56^2 x11 x12 x13 - 6 s16 s56^2 x11 x12 x13 - 3216 x10 x11 x12 x13 + 40 s16 x10 x11 x12 x13 + 336 s56 x10 x11 x12 x13 - 20 s16 s56 x10 x11 x12 x13 + 12 s56^2 x10 x11 x12 x13 + 916 x11^2 x12 x13 - 168 s16 x11^2 x12 x13 - 508 s56 x11^2 x12 x13 + 32 s16 s56 x11^2 x12 x13 + 3222 x12^2 x13 - 432 s16 x12^2 x13 - 642 s56 x12^2 x13 + 72 s16 s56 x12^2 x13 - 588 x10 x12^2 x13 - 32 s16 x10 x12^2 x13 + 180 s56 x10 x12^2 x13 + 1572 x11 x12^2 x13 - 224 s16 x11 x12^2 x13 - 264 s56 x11 x12^2 x13 + 12 s16 s56 x11 x12^2 x13 + 756 x12^3 x13 - 96 s16 x12^3 x13 - 36 s56 x12^3 x13 - 420 x13^2 + 273 s56 x13^2 - 42 s56^2 x13^2 - 1356 x10 x13^2 + 408 s56 x10 x13^2 - 24 s56^2 x10 x13^2 - 448 x10^2 x13^2 + 28 s56 x10^2 x13^2 - 276 x11 x13^2 - 78 s56 x11 x13^2 + 30 s56^2 x11 x13^2 - 328 x10 x11 x13^2 - 140 s56 x10 x11 x13^2 + 12 s56^2 x10 x11 x13^2 - 40 x11^2 x13^2 - 80 s56 x11^2 x13^2 - 426 x12 x13^2 + 12 s56 x12 x13^2 + 18 s56^2 x12 x13^2 - 864 x10 x12 x13^2 + 72 s56 x10 x12 x13^2 - 156 x11 x12 x13^2 - 108 s56 x11 x12 x13^2 - 96 x12^2 x13^2 - 36 s56 x12^2 x13^2), Rational[1, 16] (18510 x10 - 108 s16 x10 - 224 s16^2 x10 - 4566 s56 x10 + 300 s16 s56 x10 + 14 s16^2 s56 x10 - 66 s56^2 x10 - 12 s16 s56^2 x10 + 30480 x11 - 3665 s16 x11 + 26 s16^2 x11 - 9942 s56 x11 + 1433 s16 s56 x11 - 42 s16^2 s56 x11 + 426 s56^2 x11 - 99 s16 s56^2 x11 + 4 s16^2 s56^2 x11 + 42 s56^3 x11 - 3 s16 s56^3 x11 + 24568 x10 x11 - 2728 s16 x10 x11 - 3992 s56 x10 x11 + 456 s16 s56 x10 x11 - 200 s56^2 x10 x11 - 8 s16 s56^2 x10 x11 + 21712 x11^2 - 2020 s16 x11^2 - 36 s16^2 x11^2 - 1740 s56 x11^2 - 24 s16 s56 x11^2 + 22 s16^2 s56 x11^2 - 692 s56^2 x11^2 + 70 s16 s56^2 x11^2 - 2 s16^2 s56^2 x11^2 + 28 s56^3 x11^2 - 2 s16 s56^3 x11^2 + 2392 x10 x11^2 + 520 s56 x10 x11^2 - 104 s56^2 x10 x11^2 + 4576 x11^3 - 468 s16 x11^3 - 208 s56 x11^3 + 52 s16 s56 x11^3 + 17730 x12 - 1035 s16 x12 - 106 s16^2 x12 - 4647 s56 x12 + 390 s16 s56 x12 + 10 s16^2 s56 x12 + 66 s56^2 x12 - 21 s16 s56^2 x12 + 9 s56^3 x12 + 7368 x10 x12 - 664 s16 x10 x12 + 504 s56 x10 x12 + 64 s16 s56 x10 x12 - 24 s56^2 x10 x12 + 35964 x11 x12 - 4072 s16 x11 x12 + 20 s16^2 x11 x12 - 4024 s56 x11 x12 + 568 s16 s56 x11 x12 - 10 s16^2 s56 x11 x12 - 440 s56^2 x11 x12 + 20 s16 s56^2 x11 x12 + 6 s56^3 x11 x12 + 2032 x10 x11 x12 + 912 s56 x10 x11 x12 - 16 s56^2 x10 x11 x12 + 10384 x11^2 x12 - 1068 s16 x11^2 x12 + 308 s56 x11^2 x12 + 32 s16 s56 x11^2 x12 + 9252 x12^2 - 852 s16 x12^2 - 16 s16^2 x12^2 - 384 s56 x12^2 + 76 s16 s56 x12^2 - 60 s56^2 x12^2 - 360 x10 x12^2 + 72 s56 x10 x12^2 + 6240 x11 x12^2 - 636 s16 x11 x12^2 + 624 s56 x11 x12^2 - 20 s16 s56 x11 x12^2 + 432 x12^3 - 36 s16 x12^3 + 108 s56 x12^3 + 4575 x13 - 678 s16 x13 - 1497 s56 x13 + 204 s16 s56 x13 + 63 s56^2 x13 - 12 s16 s56^2 x13 + 9 s56^3 x13 - 216 x10 x13 - 896 s16 x10 x13 + 600 s56 x10 x13 + 56 s16 s56 x10 x13 - 24 s56^2 x10 x13 - 4196 x11 x13 - 112 s16 x11 x13 + 2372 s56 x11 x13 - 154 s16 s56 x11 x13 - 282 s56^2 x11 x13 + 14 s16 s56^2 x11 x13 + 6 s56^3 x11 x13 - 5456 x10 x11 x13 + 912 s56 x10 x11 x13 - 16 s56^2 x10 x11 x13 - 1428 x11^2 x13 - 72 s16 x11^2 x13 - 380 s56 x11^2 x13 + 44 s16 s56 x11^2 x13 + 56 s56^2 x11^2 x13 - 4 s16 s56^2 x11^2 x13 + 2244 x12 x13 - 644 s16 x12 x13 + 108 s56 x12 x13 + 56 s16 s56 x12 x13 - 84 s56^2 x12 x13 - 1328 x10 x12 x13 + 128 s56 x10 x12 x13 - 3216 x11 x12 x13 + 40 s16 x11 x12 x13 + 336 s56 x11 x12 x13 - 20 s16 s56 x11 x12 x13 + 12 s56^2 x11 x12 x13 - 588 x12^2 x13 - 32 s16 x12^2 x13 + 180 s56 x12^2 x13 - 1356 x13^2 + 408 s56 x13^2 - 24 s56^2 x13^2 - 896 x10 x13^2 + 56 s56 x10 x13^2 - 328 x11 x13^2 - 140 s56 x11 x13^2 + 12 s56^2 x11 x13^2 - 864 x12 x13^2 + 72 s56 x12 x13^2), Rational[1, 16] (30480 x10 - 3665 s16 x10 + 26 s16^2 x10 - 9942 s56 x10 + 1433 s16 s56 x10 - 42 s16^2 s56 x10 + 426 s56^2 x10 - 99 s16 s56^2 x10 + 4 s16^2 s56^2 x10 + 42 s56^3 x10 - 3 s16 s56^3 x10 + 12284 x10^2 - 1364 s16 x10^2 - 1996 s56 x10^2 + 228 s16 s56 x10^2 - 100 s56^2 x10^2 - 4 s16 s56^2 x10^2 + 37530 x11 - 5718 s16 x11 + 172 s16^2 x11 - 12184 s56 x11 + 1822 s16 s56 x11 - 56 s16^2 s56 x11 + 258 s56^2 x11 - 78 s16 s56^2 x11 + 4 s16^2 s56^2 x11 + 130 s56^3 x11 - 10 s16 s56^3 x11 + 43424 x10 x11 - 4040 s16 x10 x11 - 72 s16^2 x10 x11 - 3480 s56 x10 x11 - 48 s16 s56 x10 x11 + 44 s16^2 s56 x10 x11 - 1384 s56^2 x10 x11 + 140 s16 s56^2 x10 x11 - 4 s16^2 s56^2 x10 x11 + 56 s56^3 x10 x11 - 4 s16 s56^3 x10 x11 + 2392 x10^2 x11 + 520 s56 x10^2 x11 - 104 s56^2 x10^2 x11 + 40284 x11^2 - 5448 s16 x11^2 + 108 s16^2 x11^2 - 6312 s56 x11^2 + 786 s16 s56 x11^2 - 12 s16^2 s56 x11^2 - 420 s56^2 x11^2 + 30 s16 s56^2 x11^2 + 13728 x10 x11^2 - 1404 s16 x10 x11^2 - 624 s56 x10 x11^2 + 156 s16 s56 x10 x11^2 + 9360 x11^3 - 1040 s16 x11^3 + 26055 x12 - 3453 s16 x12 + 62 s16^2 x12 - 8394 s56 x12 + 1181 s16 s56 x12 - 30 s16^2 s56 x12 + 363 s56^2 x12 - 89 s16 s56^2 x12 + 4 s16^2 s56^2 x12 + 54 s56^3 x12 - 3 s16 s56^3 x12 + 35964 x10 x12 - 4072 s16 x10 x12 + 20 s16^2 x10 x12 - 4024 s56 x10 x12 + 568 s16 s56 x10 x12 - 10 s16^2 s56 x10 x12 - 440 s56^2 x10 x12 + 20 s16 s56^2 x10 x12 + 6 s56^3 x10 x12 + 1016 x10^2 x12 + 456 s56 x10^2 x12 - 8 s56^2 x10^2 x12 + 73728 x11 x12 - 10460 s16 x11 x12 + 252 s16^2 x11 x12 - 11984 s56 x11 x12 + 1768 s16 s56 x11 x12 - 52 s16^2 s56 x11 x12 - 508 s56^2 x11 x12 + 32 s16 s56^2 x11 x12 + 20768 x10 x11 x12 - 2136 s16 x10 x11 x12 + 616 s56 x10 x11 x12 + 64 s16 s56 x10 x11 x12 + 29916 x11^2 x12 - 4080 s16 x11^2 x12 + 84 s16^2 x11^2 x12 + 28944 x12^2 - 3972 s16 x12^2 + 84 s16^2 x12^2 - 4662 s56 x12^2 + 706 s16 s56 x12^2 - 22 s16^2 s56 x12^2 - 132 s56^2 x12^2 + 6 s16 s56^2 x12^2 + 6240 x10 x12^2 - 636 s16 x10 x12^2 + 624 s56 x10 x12^2 - 20 s16 s56 x10 x12^2 + 27792 x11 x12^2 - 4096 s16 x11 x12^2 + 112 s16^2 x11 x12^2 + 7236 x12^3 - 1056 s16 x12^3 + 28 s16^2 x12^3 + 1695 x13 - 334 s16 x13 + 111 s56 x13 + 38 s16 s56 x13 - 183 s56^2 x13 + 8 s16 s56^2 x13 + 15 s56^3 x13 - 4196 x10 x13 - 112 s16 x10 x13 + 2372 s56 x10 x13 - 154 s16 s56 x10 x13 - 282 s56^2 x10 x13 + 14 s16 s56^2 x10 x13 + 6 s56^3 x10 x13 - 2728 x10^2 x13 + 456 s56 x10^2 x13 - 8 s56^2 x10^2 x13 + 1984 x11 x13 - 480 s16 x11 x13 - 1084 s56 x11 x13 + 116 s16 s56 x11 x13 + 180 s56^2 x11 x13 - 20 s16 s56^2 x11 x13 - 2856 x10 x11 x13 - 144 s16 x10 x11 x13 - 760 s56 x10 x11 x13 + 88 s16 s56 x10 x11 x13 + 112 s56^2 x10 x11 x13 - 8 s16 s56^2 x10 x11 x13 + 300 x11^2 x13 - 120 s16 x11^2 x13 - 840 s56 x11^2 x13 + 60 s16 s56 x11^2 x13 + 3714 x12 x13 - 612 s16 x12 x13 - 894 s56 x12 x13 + 96 s16 s56 x12 x13 + 54 s56^2 x12 x13 - 6 s16 s56^2 x12 x13 - 3216 x10 x12 x13 + 40 s16 x10 x12 x13 + 336 s56 x10 x12 x13 - 20 s16 s56 x10 x12 x13 + 12 s56^2 x10 x12 x13 + 1832 x11 x12 x13 - 336 s16 x11 x12 x13 - 1016 s56 x11 x12 x13 + 64 s16 s56 x11 x12 x13 + 1572 x12^2 x13 - 224 s16 x12^2 x13 - 264 s56 x12^2 x13 + 12 s16 s56 x12^2 x13 - 276 x13^2 - 78 s56 x13^2 + 30 s56^2 x13^2 - 328 x10 x13^2 - 140 s56 x10 x13^2 + 12 s56^2 x10 x13^2 - 80 x11 x13^2 - 160 s56 x11 x13^2 - 156 x12 x13^2 - 108 s56 x12 x13^2), Rational[1, 16] (17730 x10 - 1035 s16 x10 - 106 s16^2 x10 - 4647 s56 x10 + 390 s16 s56 x10 + 10 s16^2 s56 x10 + 66 s56^2 x10 - 21 s16 s56^2 x10 + 9 s56^3 x10 + 3684 x10^2 - 332 s16 x10^2 + 252 s56 x10^2 + 32 s16 s56 x10^2 - 12 s56^2 x10^2 + 26055 x11 - 3453 s16 x11 + 62 s16^2 x11 - 8394 s56 x11 + 1181 s16 s56 x11 - 30 s16^2 s56 x11 + 363 s56^2 x11 - 89 s16 s56^2 x11 + 4 s16^2 s56^2 x11 + 54 s56^3 x11 - 3 s16 s56^3 x11 + 35964 x10 x11 - 4072 s16 x10 x11 + 20 s16^2 x10 x11 - 4024 s56 x10 x11 + 568 s16 s56 x10 x11 - 10 s16^2 s56 x10 x11 - 440 s56^2 x10 x11 + 20 s16 s56^2 x10 x11 + 6 s56^3 x10 x11 + 1016 x10^2 x11 + 456 s56 x10^2 x11 - 8 s56^2 x10^2 x11 + 36864 x11^2 - 5230 s16 x11^2 + 126 s16^2 x11^2 - 5992 s56 x11^2 + 884 s16 s56 x11^2 - 26 s16^2 s56 x11^2 - 254 s56^2 x11^2 + 16 s16 s56^2 x11^2 + 10384 x10 x11^2 - 1068 s16 x10 x11^2 + 308 s56 x10 x11^2 + 32 s16 s56 x10 x11^2 + 9972 x11^3 - 1360 s16 x11^3 + 28 s16^2 x11^3 + 14580 x12 - 1188 s16 x12 - 48 s16^2 x12 - 4104 s56 x12 + 390 s16 s56 x12 + 6 s16^2 s56 x12 + 168 s56^2 x12 - 30 s16 s56^2 x12 + 18 s56^3 x12 + 18504 x10 x12 - 1704 s16 x10 x12 - 32 s16^2 x10 x12 - 768 s56 x10 x12 + 152 s16 s56 x10 x12 - 120 s56^2 x10 x12 - 360 x10^2 x12 + 72 s56 x10^2 x12 + 57888 x11 x12 - 7944 s16 x11 x12 + 168 s16^2 x11 x12 - 9324 s56 x11 x12 + 1412 s16 s56 x11 x12 - 44 s16^2 s56 x11 x12 - 264 s56^2 x11 x12 + 12 s16 s56^2 x11 x12 + 12480 x10 x11 x12 - 1272 s16 x10 x11 x12 + 1248 s56 x10 x11 x12 - 40 s16 s56 x10 x11 x12 + 27792 x11^2 x12 - 4096 s16 x11^2 x12 + 112 s16^2 x11^2 x12 + 16524 x12^2 - 1674 s16 x12^2 - 18 s16^2 x12^2 - 2322 s56 x12^2 + 252 s16 s56 x12^2 - 54 s56^2 x12^2 + 1296 x10 x12^2 - 108 s16 x10 x12^2 + 324 s56 x10 x12^2 + 21708 x11 x12^2 - 3168 s16 x11 x12^2 + 84 s16^2 x11 x12^2 + 3888 x12^3 - 432 s16 x12^3 + 2970 x13 - 444 s16 x13 - 849 s56 x13 + 132 s16 s56 x13 + 12 s56^2 x13 - 12 s16 s56^2 x13 + 9 s56^3 x13 + 2244 x10 x13 - 644 s16 x10 x13 + 108 s56 x10 x13 + 56 s16 s56 x10 x13 - 84 s56^2 x10 x13 - 664 x10^2 x13 + 64 s56 x10^2 x13 + 3714 x11 x13 - 612 s16 x11 x13 - 894 s56 x11 x13 + 96 s16 s56 x11 x13 + 54 s56^2 x11 x13 - 6 s16 s56^2 x11 x13 - 3216 x10 x11 x13 + 40 s16 x10 x11 x13 + 336 s56 x10 x11 x13 - 20 s16 s56 x10 x11 x13 + 12 s56^2 x10 x11 x13 + 916 x11^2 x13 - 168 s16 x11^2 x13 - 508 s56 x11^2 x13 + 32 s16 s56 x11^2 x13 + 6444 x12 x13 - 864 s16 x12 x13 - 1284 s56 x12 x13 + 144 s16 s56 x12 x13 - 1176 x10 x12 x13 - 64 s16 x10 x12 x13 + 360 s56 x10 x12 x13 + 3144 x11 x12 x13 - 448 s16 x11 x12 x13 - 528 s56 x11 x12 x13 + 24 s16 s56 x11 x12 x13 + 2268 x12^2 x13 - 288 s16 x12^2 x13 - 108 s56 x12^2 x13 - 426 x13^2 + 12 s56 x13^2 + 18 s56^2 x13^2 - 864 x10 x13^2 + 72 s56 x10 x13^2 - 156 x11 x13^2 - 108 s56 x11 x13^2 - 192 x12 x13^2 - 72 s56 x12 x13^2), Rational[1, 16] (4575 x10 - 678 s16 x10 - 1497 s56 x10 + 204 s16 s56 x10 + 63 s56^2 x10 - 12 s16 s56^2 x10 + 9 s56^3 x10 - 108 x10^2 - 448 s16 x10^2 + 300 s56 x10^2 + 28 s16 s56 x10^2 - 12 s56^2 x10^2 + 1695 x11 - 334 s16 x11 + 111 s56 x11 + 38 s16 s56 x11 - 183 s56^2 x11 + 8 s16 s56^2 x11 + 15 s56^3 x11 - 4196 x10 x11 - 112 s16 x10 x11 + 2372 s56 x10 x11 - 154 s16 s56 x10 x11 - 282 s56^2 x10 x11 + 14 s16 s56^2 x10 x11 + 6 s56^3 x10 x11 - 2728 x10^2 x11 + 456 s56 x10^2 x11 - 8 s56^2 x10^2 x11 + 992 x11^2 - 240 s16 x11^2 - 542 s56 x11^2 + 58 s16 s56 x11^2 + 90 s56^2 x11^2 - 10 s16 s56^2 x11^2 - 1428 x10 x11^2 - 72 s16 x10 x11^2 - 380 s56 x10 x11^2 + 44 s16 s56 x10 x11^2 + 56 s56^2 x10 x11^2 - 4 s16 s56^2 x10 x11^2 + 100 x11^3 - 40 s16 x11^3 - 280 s56 x11^3 + 20 s16 s56 x11^3 + 2970 x12 - 444 s16 x12 - 849 s56 x12 + 132 s16 s56 x12 + 12 s56^2 x12 - 12 s16 s56^2 x12 + 9 s56^3 x12 + 2244 x10 x12 - 644 s16 x10 x12 + 108 s56 x10 x12 + 56 s16 s56 x10 x12 - 84 s56^2 x10 x12 - 664 x10^2 x12 + 64 s56 x10^2 x12 + 3714 x11 x12 - 612 s16 x11 x12 - 894 s56 x11 x12 + 96 s16 s56 x11 x12 + 54 s56^2 x11 x12 - 6 s16 s56^2 x11 x12 - 3216 x10 x11 x12 + 40 s16 x10 x11 x12 + 336 s56 x10 x11 x12 - 20 s16 s56 x10 x11 x12 + 12 s56^2 x10 x11 x12 + 916 x11^2 x12 - 168 s16 x11^2 x12 - 508 s56 x11^2 x12 + 32 s16 s56 x11^2 x12 + 3222 x12^2 - 432 s16 x12^2 - 642 s56 x12^2 + 72 s16 s56 x12^2 - 588 x10 x12^2 - 32 s16 x10 x12^2 + 180 s56 x10 x12^2 + 1572 x11 x12^2 - 224 s16 x11 x12^2 - 264 s56 x11 x12^2 + 12 s16 s56 x11 x12^2 + 756 x12^3 - 96 s16 x12^3 - 36 s56 x12^3 - 840 x13 + 546 s56 x13 - 84 s56^2 x13 - 2712 x10 x13 + 816 s56 x10 x13 - 48 s56^2 x10 x13 - 896 x10^2 x13 + 56 s56 x10^2 x13 - 552 x11 x13 - 156 s56 x11 x13 + 60 s56^2 x11 x13 - 656 x10 x11 x13 - 280 s56 x10 x11 x13 + 24 s56^2 x10 x11 x13 - 80 x11^2 x13 - 160 s56 x11^2 x13 - 852 x12 x13 + 24 s56 x12 x13 + 36 s56^2 x12 x13 - 1728 x10 x12 x13 + 144 s56 x10 x12 x13 - 312 x11 x12 x13 - 216 s56 x11 x12 x13 - 192 x12^2 x13 - 72 s56 x12^2 x13)};
variables = {x10,x11,x12,x13,x0};
maxW = 27;
target = x0^20;


(*pList = {a*x+y^2+x^2*y,x*y+b*y^2}
variables = {x,y};
target = x+y+x*y^2+a*b;
maxW = 5;
PolynomialReduce[target,GroebnerBasis[pList,variables],variables]//Last*)


(* ::Section:: *)
(*Full Old Run*)


pListj = Outer[((CoefficientRules[#,variables][[;;,1]]//Map[Apply[j]])*(CoefficientRules[#,variables][[;;,2]]) // Apply[Plus])&,pList];
targetj = (CoefficientRules[target,variables][[;;,1]]//Map[Apply[j]])*(CoefficientRules[target,variables][[;;,2]]) // Apply[Plus];


(*not super fast, but not the current focus*)
jseeds = createAllSeeds[variables//Length,maxW] // Map[Apply[j]]; // AbsoluteTiming
positions = Position[jseeds,j[a___]/;Plus@@({a}[[1;;(variables // Length)-1]])<=(maxW)-21] // Flatten; // AbsoluteTiming
jseeds = jseeds[[positions]]; // AbsoluteTiming
(* 24361 *)
Print["Starting number of equations: ", 1+(pList//Length)*(jseeds//Length)];


(*slow: timing = 47.8915*)
equationsOld = {targ-targetj}~Join~(Outer[(#*jseeds // Reverse)&,pListj]//Expand//Flatten // DeleteDuplicates); // AbsoluteTiming
equationsOld//Length


weightMatrix = generateWeightMatrix[variables[[4;;4]],variables[[1;;4]],DegreeReverseLexicographic];


monomialsInIdeal = pListj // Cases[#,_j,\[Infinity]]& // DeleteDuplicates;


(*not super fast, will improve on*)
jseedMonomialsOuter = Outer[Times,jseeds,monomialsInIdeal]; // AbsoluteTiming
sortedMonomials = (Join[jseeds,jseedMonomialsOuter] // Flatten // DeleteDuplicates) // ReplaceAll[j[x__]:>Times@@(variables^{x})] // Apply[Plus] // MonomialList[#,variables,weightMatrix]&; // AbsoluteTiming
sortedMonomialsj = CoefficientRules[sortedMonomials,variables][[;;,;;,1]]// Map[Apply[j],#,{2}]& // Flatten; // AbsoluteTiming
monomials = {targ}~Join~sortedMonomialsj; // AbsoluteTiming

(*(* {4.82793, Null} *)
systemSparse = CoefficientArrays[equations,monomials] // Last; // AbsoluteTiming*)


idealCoefficientMatrix = CoefficientArrays[pListj,monomialsInIdeal] // Last // Normal;
pListj-idealCoefficientMatrix . monomialsInIdeal // Factor


equationsVector=idealCoefficientMatrix . (jseedMonomialsOuter//Transpose); // AbsoluteTiming
equations = Join[{targ-targetj}, equationsVector // Flatten];
equations // Length
systemSparse = CoefficientArrays[equations,monomials] // Last; // AbsoluteTiming
nonZeroPositions = systemSparse // ArrayRules // Most // GroupBy[Last] // Part[#, All, All, 1]&; // AbsoluteTiming


((jseedMonomialsOuter[[1;;2]])//ReplaceAll[PositionIndex[monomials]])//ReplaceAll[{x_}/;IntegerQ[x]->x];
idealCoefficientMatrix;


idealCoefficientMatrix -> ((jseedMonomialsOuter)//ReplaceAll[PositionIndex[monomials]])//ReplaceAll[{x_}/;IntegerQ[x]->x] // Map[Thread] // Thread; // AbsoluteTiming


(*must be a better way of finding the parameters*)
params = (systemSparse["NonzeroValues"]//Variables)~Join~{extraparam}; // AbsoluteTiming
FFDeleteGraph["rowReduce"];
FFNewGraph["rowReduce","in",params];

(*slow: timing = 62.1645*)
FFAlgRatFunEval["rowReduce","nonZeroes",{"in"},params,(systemSparse//Flatten)["NonzeroValues"]]; // AbsoluteTiming


(* {1.31586, Null} *)
AbsoluteTiming[
nonzeromat = (systemSparse//Flatten)["AdjacencyLists"];
adjlists = (systemSparse//SparseArray)["AdjacencyLists"] (*bug here, for some reason we need //SparseArray again for some reason*); // AbsoluteTiming
FFAlgNodeSparseSolver["rowReduce","solvedSystem",{"nonZeroes"},adjlists,monomials,"NeededVars"->{targ}];
FFGraphOutput["rowReduce", "solvedSystem"];
FFSolverOnlyHomogeneous["rowReduce", "solvedSystem"];
]


(*slow: timing = 6.56594*)
learn = FFSparseSolverLearn["rowReduce", monomials]; // AbsoluteTiming


newEqnNumb=FFSparseSolverMarkAndSweepEqs["rowReduce", "solvedSystem"];
FFSparseSolverDeleteUnneededEqs["rowReduce", "solvedSystem"];

Print["Learn info: ", learn];
Print["Final number of needed equations: ", newEqnNumb];


(*check*)
FFGraphEvaluate["rowReduce",{21,33,45}]=={4481618269119748698,1393889815306612964,2985252870267917914,3585632485228829669,5122948336750702974,3118723064775569102,6197058233470957597,3073378575800070766,6860377105804030900,5661711035314395461,5061895759599983270,261345749203317161,7228614590086715849,2265600260990260656,4967761561736007728,6788304983774981379,4484750312108899147,1229116151219926057,4268762670227327564,450925859983264465} // AbsoluteTiming


(*FFReconstructFunction["rowReduce",params]*)
