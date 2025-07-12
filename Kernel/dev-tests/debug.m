(* ::Package:: *)

Get["poly_div.m"]


pList = {-1+a+x^2+z,-2+a x+c y+c x+z^2,a+b+b x +x^2 y^2,1-c+w x z+x y z} // ReplaceAll[{a->1,b->2,c->3}];
(*pList = {-1+a+x^2+z,x+z^2,a+b+b x +x^2 y^2,1-c+w x z} // ReplaceAll[{a->1,b->2,c->3}];*)
(*pList = {-1+a+x^2+z,-2+a x+c y+c x,a+b+b x +x^2 y^2,1-c+w^2*x x^2 z+x y z^2} // ReplaceAll[{a->1,b->2,c->3}];*)


variables = {x,y,z,w};
monOrder = DegreeReverseLexicographic;


gb = GroebnerBasis[pList,variables,MonomialOrder->monOrder,CoefficientDomain->RationalFunctions];
irreds=findIrreducibleMonomials[pList,variables,"MonomialOrder"->monOrder]
leadingMonomials = gb//Map[MonomialList[#,variables,monOrder]&] // Map[First]//Cases[#,x_.*y_/;IntegerQ[x]:>y]&//Apply[Plus]//MonomialList[#,variables,monOrder]&;


target = x*w*z^3y+y^2;


a1=PolynomialReduce[target,gb,variables,MonomialOrder->monOrder] // Last // Together


solveSystem[{target},pList,variables,6,"IrreducibleMonomials"->irreds,"MonomialOrder"->monOrder]
a2=reconstructSystemOutput[%] // First // Together


compMats = constructCompanionMatrices[pList,variables,6,irreds,"MonomialOrder"->monOrder];
targetCmat = constructTargetCmat[target,compMats];
a3=ffPolyDiv[targetCmat,irreds] // Together
