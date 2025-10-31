(* ::Package:: *)

createIndexes[length_]:= Module[{mat1, list2},
	mat1 = Partition[Table[{1, b}, {b, 1, length^2}], length];
	list2 = Table[{2, i}, {i, 1,length}];
	
	Return[Flatten[Join[mat1, list2 // List // Transpose, 2], 1]];
];
joinTwoMatricesIndex[length_]:=Module[{mat1,mat2,result},
	mat1 = Partition[Table[{1, b}, {b, 1, length^2}], length];
	mat2 = Partition[Table[{2, b}, {b, 1, length^2}], length];
	result = Join[mat1 // Transpose, mat2 // Transpose] // Transpose //Flatten[#,1]&;
	Return[result];
];
createAdjacencyLists[matdim1_,matdim2_, nonZeroList_] := Module[{array, mat, adjLists},
	array = ConstantArray[0, matdim1 * matdim2];
	array[[nonZeroList]] = 1;
	
	mat=Partition[array, matdim2]//SparseArray;
	adjLists=mat["AdjacencyLists"];
	
	Return[adjLists];
];


ffMultiplyMatrices[matrixList_,positions_,matSize_]:=Module[{},
		matricesInMultiplication = matrixList[[positions]];
		(*newMatrixNames = matricesInMultiplication[[1;;1]]~Join~Table[t@@matricesInMultiplication[[;;j]],{j,2,length}];*)
		newMatrixNames = matricesInMultiplication[[1;;1]]~Join~Table[onnF/@(t@@matricesInMultiplication[[;;j]]),{j,2,length-1}]~Join~{onnF[(matrixList//Length)+1]};
		Table[FFAlgMatMul[graphName,newMatrixNames[[j+1]],{newMatrixNames[[j]],matricesInMultiplication[[j+1]]},matSize,matSize,matSize],{j,1,length - 1}];
		Return[newMatrixNames];
];
ffdotEval[input___]:=Module[{i},
		inputList = {input};
		(*inputList = List@@input;*)
		length = inputList//Length;
		positions = inputList//Map[Position[matrixList,#,{1}]&]//Flatten;
		
		mathematicaOutput = (*t@@inputList;*)onnF[(matrixList//Length)+1];
		(*finite flow graph construction*)
		matrixList=matrixList~Join~ffMultiplyMatrices[matrixList,positions,matSize] //DeleteDuplicates;
		(**)
		(*matrixList = matrixList~Join~{mathematicaOutput} //DeleteDuplicates;*)
		Return[mathematicaOutput];
];
ffAddMatrices[inputList_,matrixList_,positions_]:=Module[{},
		matricesInAddition = matrixList[[positions]];
		sumMatrixName = (*a@@inputList*)onnF[(matrixList//Length)+1];
		FFAlgAdd[graphName,sumMatrixName,matricesInAddition];
		Return[{sumMatrixName}];
];
ffplusEval[input___]:=Module[{i},
		inputList = {input};
		(*inputList = List@@input;*)
		length = inputList//Length;
		positions = inputList//Map[Position[matrixList,#,{1}]&]//Flatten;
		
		mathematicaOutput = (*a@@inputList*)onnF[(matrixList//Length)+1];
		(*finite flow graph construction*)
		matrixList=matrixList~Join~ffAddMatrices[inputList,matrixList,positions] //DeleteDuplicates;
		(**)
		Return[mathematicaOutput];
];
ffinvMatrix[input_,matrixList_,position_]:=Module[{},
		invName = (*i[input]*)onnF[(matrixList//Length)+1];
		
		FFAlgTake[graphName,onnF[aid@@invName],{input,m[-1]},joinTwoMatricesIndex[matSize]];
		
		FFAlgNonZeroes[graphName, onnF[nz@@invName], {onnF[aid@@invName]}];
		FFGraphOutput[graphName, onnF[nz@@invName]];
		
		nonzeromat = "NonZero" /. FFNonZeroesLearn[graphName];
		adjlists = createAdjacencyLists[matSize,2*matSize,nonzeromat];
		
		FFAlgNodeSparseSolver[graphName, onnF[is@@invName], {onnF[nz@@invName]}, adjlists, Range[2*matSize]];
		(*Print[FFAlgNodeSparseSolver1[graphName, is@@invName, {nz@@invName}, adjlists, Range[2*matSize]]];*)
		FFGraphOutput[graphName, onnF[is@@invName]];
		FFSolverOnlyHomogeneous[graphName,onnF[is@@invName]];
		learn = FFSparseSolverLearn[graphName, Range[2*matSize]];
		
		(*invalid permutation <-> the system cannot be inverted*)
		If[Range[Length[("DepVars"/.learn)]]=!=Sort["DepVars"/.learn],
			Print["ERROR: a (sub)expression of the target companion matrices could not be inverted. Probable cause is zero determinant.\nCheck if you are trying to invert a term which is inside the ideal."];
			Abort[];
		];
		
		shuffleIndex = (Table[{1,i},{i,1,matSize^2}]//Partition[#,matSize]&)[[InversePermutation["DepVars"/.learn]]]//Flatten[#,1]&;
		FFAlgTake[graphName,invName,{onnF[is@@invName]},shuffleIndex];
		
		Return[invName];
];
ffinvEval[input___]:=Module[{},
		position = {input}//Map[Position[matrixList,#,{1}]&]//Flatten;
		mathematicaOutput = (*i[input]*)onnF[(matrixList//Length)+1];
		(*finite flow graph construction*)
		If[True,
			matrixList=matrixList~Join~{ffinvMatrix[input,matrixList,position]} // DeleteDuplicates;
			,
			matrixList=matrixList~Join~{onnF[(matrixList//Length)+1]};
		];
		
		Return[mathematicaOutput];
];


timesToDotRep:=ReplaceAll[#,{Times->dot}]&
powerToDotRep:=#//.{Power[input1___,input2_]/;input2>=1 :>dot@@ConstantArray[input1,input2],Power[input1___,input2_]/;input2<=-1 :>inv[dot@@ConstantArray[input1,-input2]]} &


ffPolyReduce[expression_, parameters_, matricesSymbolic_, matricesSubstituted_,extraNodes_,outputNodeName_]:= Module[{},
	
	matSize = matricesSubstituted // First // Length;
	
	rationalFunctionSub = ((expression//timesToDotRep//powerToDotRep)//.{dot[x_]:>x,times[x_]:>x})//.Plus->plus;
	(*find all numbers and variables appearing in the expression*)
	(*horrendous spaghetti code to fix at some point...*)
	variablesList=(((If[rationalFunctionSub//NumberQ,{rationalFunctionSub},{}])~Join~(rationalFunctionSub//Cases[#,dot[x___]:>{x},{0,\[Infinity]}]&)~Join~(rationalFunctionSub//Cases[#,plus[x___]:>{x},{0,\[Infinity]}]&)~Join~(rationalFunctionSub//Cases[#,inv[x___]:>{x},{0,\[Infinity]}]&)~Join~parameters~Join~matricesSymbolic)//Flatten//DeleteDuplicates//DeleteCases[#,_inv,\[Infinity]]&//DeleteCases[#,_dot,\[Infinity]]&)/.Plus->List/.plus->List//Flatten//DeleteDuplicates // Complement[#,extraNodes]&;
	constants = Complement[variablesList,matricesSymbolic]~Join~{-1} // DeleteDuplicates // Complement[#,extraNodes]&;
	
	constantsMatricesSub = ((constants//Map[IdentityMatrix[matSize]*#&]))~Join~{0*IdentityMatrix[matSize]} // DeleteDuplicates;
	
	constantsMatricesNames = (constants~Join~{0} // DeleteDuplicates)//Map[m];
	
	matrixList = (m/@matricesSymbolic)~Join~constantsMatricesNames~Join~extraNodes;
	matListSub = matricesSubstituted~Join~constantsMatricesSub;
	params = parameters;
	
	onnF = outputNodeName // First;
	
	rationalFunctionSub2 = rationalFunctionSub /. ((matrixList/._[x_]:>x)->matrixList // Thread) //. m[m[x___]]:>m[x](*/.{(-1) :>m[-id]}*);

	(*begin graph setup*)
	Table[FFAlgRatFunEval[graphName, (matrixList[[i]](*/.m->mat*)), {"in"}, params, matListSub[[i]] // Flatten//Normal],{i,1,matListSub//Length}];
	(*FFAlgRatFunEval[graphName, m[-id], {"in"}, params, -IdentityMatrix[matSize] // Flatten//Normal];*)
	(*end of graph setup*)
	
	(*begin graph construction*)
	output = rationalFunctionSub2 /.{plus->ffplusEval,dot->ffdotEval,inv->ffinvEval};
	If[SubsetQ[(m/@matricesSymbolic)~Join~constantsMatricesNames~Join~extraNodes,{output}]
	,
		FFAlgTake[graphName,outputNodeName,{output},Table[{1,j},{j,1,matSize^2}]];
	,
		FFAlgTake[graphName,outputNodeName,{(*n[output/.n[x_]:>x-1]*)matrixList// Cases[#,_onnF]& // Sort // Last},Table[{1,j},{j,1,matSize^2}]];
	];
	
	FFGraphOutput[graphName,outputNodeName];
	(*end graph construction*)
	
	matrixList = matrixList~Join~{outputNodeName};
	(*end graph construction*)
];


BuildTargetCompanionMatrices[target_List,cmatOutput_]:=Module[
	{
		vars,varsSub,targetSub,params,placeHolders,irredMons,cmatSize,placeHolderMatrices,cmatNames,outputNode,extraVars,
		Nothing
	}
	,
	
	vars = cmatOutput[[5]];
	varsSub = vars -> cmatOutput[[4]] // Thread;
	targetSub = target // ReplaceAll[varsSub];
	graphName = cmatOutput[[1]]; (*this should really be an option for all the functions in the parser*)
	params = cmatOutput[[2]];
	
	(*check and process if the target functions introduce any new parameters*)
	extraVars = Complement[target // Variables, Join[vars,params]];
	If[{} =!= extraVars,
		Print["WARNING: Extra parameters ", extraVars ," found in targets not present in the ideal"];
		Print["To fix this pass the following option to BuildCompanionMatrices"];
		Print[Rule["ExtraParams", extraVars]];
		Return[$Failed];
	];
	
	placeHolders = vars // Length // Range // Map[placeHolder];
	irredMons = (*"IndepVars"//ReplaceAll[cmatOutput[[3]]]*)cmatOutput[[3]];
	cmatSize = irredMons // Length;
	placeHolderMatrices = Table[ConstantArray[0,{cmatSize,cmatSize}],{i,1,vars // Length}];
	cmatNames = varsSub[[;;,2]];
	outputNode = Table[StringJoin["o",Unique[]//ToString],{i,1,target//Length}];
	Table[ffPolyReduce[targetSub[[i]],params,placeHolders,placeHolderMatrices,cmatOutput[[4]],m[outputNode[[i]]]],{i,1,target//Length}];
	Return[{graphName,params,irredMons,Table[m[outputNode[[i]]],{i,1,target//Length}](*cmatSize*),vars}];
];


Options[ReconstructTargetCompanionMatrices] = {"cmat"->False,"DeleteGraph"->True,"Vector"->False,"PrintDebugInfo"->1};
ReconstructTargetCompanionMatrices[targetOutput_,(*irredMons_,*)OptionsPattern[]]:=Module[
	{reconstructed,cmat,polyRed,takePattern,cmatSize,irredMons,ncmats}
	,
	irredMons = targetOutput[[3]];
	cmatSize = irredMons // Length;
	ncmats = targetOutput[[4]] // Length;
	
	If[OptionValue["cmat"],
		FFAlgChain[targetOutput[[1]],"chainCmats",targetOutput[[4]]];
		FFGraphOutput[targetOutput[[1]],"chainCmats"];
		reconstructed = FFReconstructFunction[targetOutput[[1]],targetOutput[[2]],"MaxDegree"->1000,"MaxPrimes"->200,"PrintDebugInfo"->OptionValue["PrintDebugInfo"]];
		If[OptionValue["DeleteGraph"],FFDeleteGraph[targetOutput[[1]]//Evaluate]];
		cmat = reconstructed // Partition[#,{cmatSize^2}]& // Map[Partition[#,{cmatSize}]&];
		Return[cmat];
	,
		takePattern = Range[1+cmatSize^2-cmatSize,cmatSize^2] // {Range[ncmats],#}& // Tuples;
		FFAlgTake[targetOutput[[1]],"takeCmatComponents",targetOutput[[4]],takePattern];
		FFGraphOutput[targetOutput[[1]],"takeCmatComponents"];
		reconstructed = FFReconstructFunction[targetOutput[[1]],targetOutput[[2]],"MaxDegree"->1000,"MaxPrimes"->200,"PrintDebugInfo"->OptionValue["PrintDebugInfo"]];
		If[OptionValue["DeleteGraph"],FFDeleteGraph[targetOutput[[1]]//Evaluate]];
		
		If[OptionValue["Vector"],
			Return[reconstructed // Partition[#,{cmatSize}]&];
		,
			Return[(reconstructed // Partition[#,{cmatSize}]&) . irredMons // ReplaceAll[j[x__]:>Times@@(targetOutput[[5]]^{x})]];
		];
	];
];
