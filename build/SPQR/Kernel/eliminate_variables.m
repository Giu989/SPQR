(* ::Package:: *)

BuildCharacteristicPolynomial[cmatData_,index_]:=Module[
	{
	params,n,traceTakePattern,diagMat,sumPatternVars,sumPattern,
	dvar,dvars,c,J,AM,M,cptsAM,trAM,numprefactor,sgnchar,char
	},
	
	params = cmatData[[2]];
	(*nxn cmatrix*)
	n = cmatData[[3]] // Length;
	
	traceTakePattern = Table[1+i*n+i,{i,0,n-1}] // {{1},#}& // Tuples;
	diagMat = dvar*IdentityMatrix[n]//Flatten;
	(*sumPatternVars = Array[dvars,n];
	sumPattern = sumPatternVars // Apply[Plus];*)
	
	(*Faddeev\[Dash]LeVerrier algorithm*)
	FFAlgRatFunEval[cmatData[[1]],c[n+1]//ToString,{"in"},params,{0}];
	FFAlgRatFunEval[cmatData[[1]],c[n]//ToString,{"in"},params,{1}];
	FFAlgRatFunEval[cmatData[[1]],J[n]//ToString,{c[n]//ToString},{dvar},diagMat];
	FFAlgRatFunEval[cmatData[[1]],AM[0]//ToString,{c[n+1]//ToString},{dvar},diagMat];
	
	Do[
		FFAlgAdd[cmatData[[1]],M[k]//ToString,{AM[k-1]//ToString,J[n-k+1]//ToString}];
		FFAlgMatMul[cmatData[[1]],AM[k]//ToString,{cmatData[[4]][[index]],M[k]//ToString},n,n,n];

		(*FFAlgTake[cmatData[[1]],cptsAM[k]//ToString,{AM[k]//ToString},traceTakePattern];
		FFAlgRatExprEval[cmatData[[1]],trAM[k]//ToString,{cptsAM[k]//ToString},sumPatternVars,{sumPattern}];*)
		FFAlgTakeAndAdd[cmatData[[1]],trAM[k]//ToString,{AM[k]//ToString},{traceTakePattern}];

		FFAlgRatFunEval[cmatData[[1]],numprefactor[k]//ToString,{"in"},params,{-1/k}];
		FFAlgMul[cmatData[[1]],c[n-k]//ToString,{numprefactor[k]//ToString,trAM[k]//ToString}];
		If[k<n,FFAlgRatFunEval[cmatData[[1]],J[n-k]//ToString,{c[n-k]//ToString},{dvar},diagMat]];
		,
		{k,n}
	];
	
	(*collect outputs*)
	FFAlgTake[cmatData[[1]],sgnchar[cmatData[[4]][[index]][[1]]]//ToString,Array[c,n+1,0]//Map[ToString],Range[n+1]//{#,{1}}&//Tuples];
	
	(*flip sign for odd dimensional matrices*)
	FFAlgRatExprEval[cmatData[[1]],char[cmatData[[4]][[index]][[1]]]//ToString,{sgnchar[cmatData[[4]][[index]][[1]]]//ToString},Array[dvars,n+1],(-1)^n Array[dvars,n+1]];
	
	FFGraphOutput[cmatData[[1]],char[cmatData[[4]][[index]][[1]]]//ToString];
	Return[{cmatData[[1]],params,cmatData[[3]],char[cmatData[[4]][[index]][[1]]]//ToString}];
];


BuildCharacteristicPolynomials[cmatData_,inputList_List]:=Module[
	{outputs},
	outputs = Table[BuildCharacteristicPolynomial[cmatData,index],{index,inputList}];
	Return[outputs[[1,1;;3]]~Join~{outputs[[;;,4]]}];
];
BuildCharacteristicPolynomials[cmatData_]:=BuildCharacteristicPolynomials[cmatData,cmatData[[4]]//Length//Range];
BuildCharacteristicPolynomials[cmatData_,input_Integer]:=BuildCharacteristicPolynomials[cmatData,{input}];


Options[ReconstructCharacteristicPolynomials] = {"PrintDebugInfo"->1,"DeleteGraph"->True,"Mod"->False,"FFPrimeNo"->0};
ReconstructCharacteristicPolynomials[characteristicPolynomialData_,elements_List,opts : OptionsPattern[]]:=Module[
	{chain,reconstructed,nmats,n,takePattern},
	nmats = characteristicPolynomialData // Last // Length;
	n = characteristicPolynomialData[[3]] // Length;
	takePattern = {nmats//Range,elements} // Tuples;
	
	FFAlgTake[characteristicPolynomialData[[1]],chain//ToString,characteristicPolynomialData[[4]],takePattern];
	FFGraphOutput[characteristicPolynomialData[[1]],chain//ToString];
	
	If[OptionValue["Mod"],
		reconstructed = FFReconstructFunctionMod[
			characteristicPolynomialData[[1]],characteristicPolynomialData[[2]],
			"MaxDegree"->1000,"MaxPrimes"->200,"PrintDebugInfo"->OptionValue["PrintDebugInfo"],"StartingPrimeNo"->OptionValue["FFPrimeNo"]
		];
	,
		If[OptionValue["FFPrimeNo"]!=0,Print["Note: FFPrimeNo value will be ignored"]];
		reconstructed = FFReconstructFunction[
			characteristicPolynomialData[[1]],characteristicPolynomialData[[2]],
			"MaxDegree"->1000,"MaxPrimes"->200,"PrintDebugInfo"->OptionValue["PrintDebugInfo"]
		];
	];
	If[OptionValue["DeleteGraph"],FFDeleteGraph[characteristicPolynomialData[[1]]//Evaluate]];
	Return[reconstructed // ArrayReshape[#,{nmats,elements//Length}]&];
];
ReconstructCharacteristicPolynomials[characteristicPolynomialData_,opts : OptionsPattern[]]:=ReconstructCharacteristicPolynomials[characteristicPolynomialData,1+(characteristicPolynomialData[[3]]//Length)//Range,opts];


(*high level determinant calculator using FiniteFlow*)
Options[FFDet] = {"PrintDebugInfo"->1,"Mod"->False,"FFPrimeNo"->0};
FFDet[matrix_,opts : OptionsPattern[]]:=Module[
	{
		graphname,matname,params,irredsPlaceholder,data,output,rec
	},
	
	If[!SquareMatrixQ[matrix],Print["not a square matrix"];Return[$Failed]];
	
	params = matrix // Variables;
	If[Length[params]<2,params=params~Join~{extraparam}];
	
	FFNewGraph[graphname//ToString,"in",params];
	FFAlgRatExprEval[graphname//ToString,m[matname//ToString],{"in"},params,matrix//Flatten];
	
	irredsPlaceholder = matrix // Length // Range;
	data = {graphname//ToString,params,irredsPlaceholder,{m[matname//ToString]},{}};
	
	output = BuildCharacteristicPolynomials[data,1];
	rec = ReconstructCharacteristicPolynomials[output,{1},opts] // First // First;
	
	Return[rec];
];


BuildEliminationSystem[cmatData_,monomialList_] := Module[
	{
		cmatSize,cmats,processedMonomials,monomialCmats,takePattern,transposePattern,
		intermediatenode1,intermediatenode2,vars,outputNode,f,learn,survivingVarsIndex
		,
		Nothing
	},
	
	cmatSize = cmatData[[3]] // Length;
	processedMonomials = monomialList // Reverse // Times[#,Join[ConstantArray[-1,(Length[#]-1)],{1}]]&;
	
	monomialCmats = BuildTargetCompanionMatrices[processedMonomials,cmatData];
	takePattern = Range[1+cmatSize^2-cmatSize,cmatSize^2] // {Range[processedMonomials//Length],#}& // Tuples;
	transposePattern = Range[1,cmatSize*(processedMonomials // Length)] // ArrayReshape[#,{processedMonomials // Length,cmatSize}]& // Transpose // Flatten // {{1},#}& // Tuples;
	
	intermediatenode1 = "cmatRemainders$" // Unique // ToString;
	FFAlgTake[monomialCmats[[1]],intermediatenode1,monomialCmats[[4]],takePattern];
	
	intermediatenode2 = "cmatRemaindersTransposed$" // Unique // ToString;
	FFAlgTake[monomialCmats[[1]],intermediatenode2,{intermediatenode1},transposePattern];
	
	vars = Array[f,(processedMonomials // Length)-1];
	outputNode = "coefficients$" // Unique // ToString;
	FFAlgNodeDenseSolver[monomialCmats[[1]],outputNode,{intermediatenode2},cmatSize,vars];
	If[learn==$Failed,Print["WARNING: Failed to fit ansatz. Are the given monomials corret?"]; Abort[];];
	
	FFGraphOutput[monomialCmats[[1]],outputNode];
	learn = FFDenseSolverLearn[monomialCmats[[1]],vars];
	(*Print[learn];*)
	If[(Length["IndepVars"//ReplaceAll[learn]])>0,Print["Warning: Ansatz could not be fixed correctly, too many monomials."]; Abort[];];
	survivingVarsIndex = "DepVars" // ReplaceAll[learn] // MapApply[Identity];
	
	Return[{cmatData[[1]],cmatData[[2]],(*cmatData[[3]]*)survivingVarsIndex,outputNode}];
];


BuildEliminationSystems[cmatData_,monomialLists_] := Module[
	{
		data
	},
	
	data = Table[BuildEliminationSystem[cmatData,monomiaList],{monomiaList,monomialLists}];
	Return[Join[data[[1]][[1;;2]],{data[[;;,3]]},{data[[;;,4]]},{monomialLists}]];
];


Options[ReconstructEliminationSystems] = {"DeleteGraph"->True,"PrintDebugInfo"->1,"Vector"->False,"Mod"->False,"FFPrimeNo"->0};
ReconstructEliminationSystems[elimData_, opts : OptionsPattern[]] := Module[
	{
		cmatSize,takePattern,nodeName,reconstructed,reconstructedProcessed,nonZeroVars,Nothing
	},
	
	takePattern = elimData[[3]]//Map[Length]// Map[Range]//Table[{i,#[[i]]}//Thread,{i,1,#//Length}]&//Flatten[#,1]&;
	
	nodeName = "elimOutput$" // Unique // ToString;
	FFAlgTake[elimData[[1]],nodeName,elimData[[4]],takePattern];
	FFGraphOutput[elimData[[1]],nodeName];
	
	If[OptionValue["Mod"],
		reconstructed = FFReconstructFunctionMod[
			elimData[[1]],elimData[[2]],
			"MaxDegree"->1000,"MaxPrimes"->200,"PrintDebugInfo"->OptionValue["PrintDebugInfo"],"StartingPrimeNo"->OptionValue["FFPrimeNo"]
		];
	,
		If[OptionValue["FFPrimeNo"]!=0,Print["Note: FFPrimeNo value will be ignored"]];
		reconstructed = FFReconstructFunction[
			elimData[[1]],elimData[[2]],"MaxDegree"->1000,"MaxPrimes"->200,"PrintDebugInfo"->OptionValue["PrintDebugInfo"]
		];
	];
	
	reconstructedProcessed = reconstructed // TakeList[#,elimData[[3]]//Map[Length]]& // Map[Reverse] // Map[Join[{1},#]&];
	
	If[OptionValue["DeleteGraph"],FFDeleteGraph[elimData[[1]]//Evaluate]];
	If[OptionValue["Vector"],
		Return[reconstructedProcessed];
	,
		nonZeroVars = Table[Join[elimData[[5]][[i]][[elimData[[3]][[i]]]],{1}],{i,1,elimData[[3]]//Length}];
		Return[nonZeroVars*reconstructedProcessed // MapApply[Plus]];
	];
];
