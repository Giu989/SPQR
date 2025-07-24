(* ::Package:: *)

BuildCharacteristicPolynomial[cmatData_,index_]:=Module[
	{
	params,n,traceTakePattern,diagMat,sumPatternVars,sumPattern,
	dvar,dvars,c,J,AM,M,cptsAM,trAM,numprefactor,sgnchar,char
	},
	
	params = cmatData[[2]];
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
	
	(*flip sign on odd dimensional matrices*)
	FFAlgRatExprEval[cmatData[[1]],char[cmatData[[4]][[index]][[1]]]//ToString,{sgnchar[cmatData[[4]][[index]][[1]]]//ToString},Array[dvars,n+1],(-1)^n Array[dvars,n+1]];
	
	FFGraphOutput[cmatData[[1]],char[cmatData[[4]][[index]][[1]]]//ToString];
	Return[{cmatData[[1]],params,char[cmatData[[4]][[index]][[1]]]//ToString}];
];
