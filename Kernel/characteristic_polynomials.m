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
	{graphname,matname,params,irredsPlaceholder,data,output,rec},
	
	If[!SquareMatrixQ[matrix],Print["not a square matrix"];Return[$Failed]];
	
	params = matrix // Variables;
	If[params=={},params={extraparam}];
	
	FFNewGraph[graphname//ToString,"in",params];
	FFAlgRatExprEval[graphname//ToString,m[matname//ToString],{"in"},params,matrix//Flatten];
	
	irredsPlaceholder = matrix // Length // Range;
	data = {graphname//ToString,params,irredsPlaceholder,{m[matname//ToString]},{}};
	
	output = BuildCharacteristicPolynomials[data,1];
	rec = ReconstructCharacteristicPolynomials[output,{1},opts] // First // First;
	
	Return[rec];
];
