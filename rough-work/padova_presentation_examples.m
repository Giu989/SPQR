(* ::Package:: *)

(* ::Title:: *)
(*Examples*)


Get["spqr_load.m"]
<<Singular`
ResourceFunction["PolynomialHomogenize"];


(* ::Section:: *)
(*j invariant*)


jinvariantRoots = (256 (r3^2 r4^2-r2 r3 r4 (r3+r4)+r2^2 (r3^2-r3 r4+r4^2)+r1^2 (r2^2+r3^2-r3 r4+r4^2-r2 (r3+r4))-r1 (r2^2 (r3+r4)+r3 r4 (r3+r4)+r2 (r3^2-6 r3 r4+r4^2)))^3)/((r1-r2)^2 (r1-r3)^2 (r2-r3)^2 (r1-r4)^2 (r2-r4)^2 (r3-r4)^2) // Factor


variables = {r1,r2,r3,r4}
ideal=SolveAlways[(x-r1)(x-r2)(x-r3)(x-r4)==c0+c1*x+c2*x^2+c3*x^3+c4*x^4,x]//Flatten//Map[#[[1]]-#[[2]]&,#]& // ReplaceAll[c4->1] // Part[#,2;;]&
ideal == 0 // Thread // MatrixForm


numericalSub = {c0->1,c1->2,c2->3,c3->5};
numericalSub // MatrixForm


solNumerical = ideal // ReplaceAll[numericalSub] // Equal[#,0]& // Solve[#,{r1,r2,r3,r4}]& // First


jinvariantRoots // ReplaceAll[solNumerical]
% // RootReduce


irredMonomials=FindIrreducibleMonomials[ideal,variables,"MonomialOrder"->DegreeReverseLexicographic];
variableCmats = BuildCompanionMatrices[ideal,variables,6,irredMonomials,"MonomialOrder"->DegreeReverseLexicographic];


jinvCmat = BuildTargetCompanionMatrix[jinvariantRoots,variableCmats];


jinvCmat // First // FFGraphDraw // FullForm // ReplaceAll[Text[___]:>Text[""]] // Normal // First


jinvCoefficients = ReconstructTargetCompanionMatrix[jinvCmat,"DeleteGraph"->False]


jinvCoefficients // Factor


jinvCoefficients // ReplaceAll[numericalSub]


(* ::Section:: *)
(*Variable Elimination*)


(* ::Subsection:: *)
(*Example 1*)


ideal = {a*x+y^2+b,x*y+2*x^2 - y^2};
ideal // MatrixForm
variables = {x,y};


Solve[ideal==0,{x,y}]
y // ReplaceAll[%] // MatrixForm


irredMonomials = FindIrreducibleMonomials[ideal,variables]
rowReduction = BuildPolynomialSystem[{y^4},ideal,variables,2,"IrreducibleMonomials"->irredMonomials,"EliminateVariables"->{{x},1},"MonomialOrder"->DegreeReverseLexicographic]


r = ReconstructPolynomialRemainder[rowReduction] // First


f = y^4 - r // Factor


y // ReplaceAll[Solve[f==0,y]] // MatrixForm


(* ::Subsection:: *)
(*Example 2*)


p1 = x^2*y^2 -3 + a+y^3;
p2 = a*x+c* y - 5+x*y^2*c;
p3 = a + b +b x*y^2+x^2y^2;
pList = {p1,p2,p3};
pList // MatrixForm


resultant = SingularEliminate[pList,{x,y},{a,b,c}] //First // Factor; //AbsoluteTiming
resultant


variables = {x,y,c};
rowReduction = BuildPolynomialSystem[{c^6},pList,variables,8,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x,y},5}]; // AbsoluteTiming
rowReduction


reconstructed  = ReconstructPolynomialRemainder[rowReduction] // First; // AbsoluteTiming
reconstructed // Short


resultantFF= c^6 - reconstructed // Together // Numerator // Factor; // AbsoluteTiming
resultantFF


resultantFF/resultant
