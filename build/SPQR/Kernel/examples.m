(* ::Package:: *)

(* ::Title:: *)
(*Examples*)


(*(*Get["poly_div.m"]*)
Get["polydiv`"]*)


(* ::Subsection::Closed:: *)
(*Example 1 \[LongDash] Overview of main functions*)


(*default ordering, to keep convention with Mathematica, is Lexicographic*)
FindIrreducibleMonomials // Options
BuildPolynomialSystem//Options
BuildCompanionMatrices//Options


(*we construct an ideal in two variables {x,y} with parameters {a,b}*)
(*we also have a list of target integrals to reduce*)
variables = {x,y};
ideal = {a*y^2*x^2-2x+3,y^2-3y+b};
targetList = {a*x^5*y^5*y^2+3+x*y^2+b^2,x*y+s+3,x^3};


(*we first find the irreducible monomials via a numerical GB*)
irredMonomials=FindIrreducibleMonomials[ideal,variables]


(*to be able to reduce all targets, we need to generate the matrix to rank 8*)
BuildPolynomialSystem[targetList,ideal,variables,7,"IrreducibleMonomials"->irredMonomials]


output = BuildPolynomialSystem[targetList,ideal,variables,8,"IrreducibleMonomials"->irredMonomials]


(*if the irreducible monomials option is not specified, the system will assume whatever irreducible monomials it finds are correct*)


(*we now reconstruct the polynomial division using ReconstructPolynomialRemainder*)
results=ReconstructPolynomialRemainder[output]


(*and check it agrees against the mathematica computation*)
gb = GroebnerBasis[ideal,variables,CoefficientDomain->RationalFunctions];
ansGB = PolynomialReduce[targetList,gb,variables] // Map[Last] // Factor;
ansGB - results // Factor // ContainsOnly[{0}]


(*we can also generate the companion matrices associated to the ideal with the command BuildCompanionMatrices[]*)


(*as one can see, the required weight is far lower in this case than for the direct polynomial reductions. At weight 2 the system fails, but it works for weight 3, far lower than weight 8!*)
BuildCompanionMatrices[ideal,variables,2,irredMonomials]


cmats = BuildCompanionMatrices[ideal,variables,3,irredMonomials]


(*the command BuildTargetCompanionMatrix[] will take the companion matrices and a target, and will construct the new target c matrix. Let us take the first polynomial from the list as an example*)
targetList // First


(*NOTE: Currently there is a bug where the ideal must contain all the parameters that end up in the polynomial to reduce, or else the system will fail to solve*)
(*to do: only reconstruct necessary part of companion matrix?*)
targetCmat = BuildTargetCompanionMatrix[targetList // First,cmats]


(*we can see the final recursively built graph with*)
targetCmat // First // FFGraphDraw


(*finally, we can reconstruct the output of the polynomial division with*)
reductionWithCmats = ReconstructTargetCompanionMatrix[targetCmat,irredMonomials,"DeleteGraph"->False]
(*note currently the full cmat is reconstructed and then post processed in mathematica, which means potentially more sample points than necessary are used*)
(*check agreement*)
(ansGB // First) - reductionWithCmats // Factor // SameQ[#, 0]&


ReconstructTargetCompanionMatrix[targetCmat,irredMonomials,"DeleteGraph"->False,"Vector"->True]


ReconstructTargetCompanionMatrix[targetCmat,irredMonomials,"DeleteGraph"->False,"cmat"->True]


(*we can also do more complicated rational expressions with the companion matrix reducer*)
expression = (x+y^2)^17 (3*a*x^2 y - (x-y)^3/(a-b-x-1/y))^-2


targetCmat = BuildTargetCompanionMatrix[expression,cmats]
(*targetCmat // First // FFGraphDraw*)
targetCmat // First // FFGraphDraw // FullForm // ReplaceAll[Text[___]:>Text[""]] // Normal // First
reductionWithCmats = ReconstructTargetCompanionMatrix[targetCmat,irredMonomials,"DeleteGraph"->False]


(*we can check against the GB approach by separating out the numerator and denominator, as Mathematica does not support the reduction of rational functions per se*)


PolynomialReduce[reductionWithCmats*(expression//Together//Denominator),gb,variables][[2]]//Factor;
PolynomialReduce[(expression//Together//Numerator),gb,variables][[2]]//Factor;
(* Please avoid percent symbols. Give variables names. Don't be lazy *)
%==%%//Factor


(*a few final things to note: the current recursive parser is very janky. as such, so is the piece of code that interfaces to it, namely BuildTargetCompanionMatrix[]*)
(*essentially everything in ff_cmat_mult.m will probably need to be completely rewritten*)
(*the rest will also have to be heavily modified also, but personally I am quite happy with the input and outputs of each function*)


(* ::Subsection::Closed:: *)
(*Example 2 \[LongDash]\[NonBreakingSpace]Two Loop Triangle Landau*)


gpol = x1 x2+x1^2 x2+v^2 x1 x2^2+x1 x3+x1^2 x3+x2 x3+2 v^2 x1 x2 x3+v^2 x2^2 x3+v^2 x1 x3^2+v^2 x2 x3^2+x2 x4+2 x1 x2 x4+u x1 x2 x4+v^2 x2^2 x4+x3 x4+2 x1 x3 x4+u x1 x3 x4+2 v^2 x2 x3 x4+v^2 x3^2 x4+x2 x4^2+x3 x4^2;
pList = {1-newvar*gpol}~Join~D[gpol,{{x1,x2,x3,x4}}];


variables = {x1,x2,x3,x4,newvar};
target = gpol;


irredMonomials=FindIrreducibleMonomials[pList,variables]


(*using the system solver*)
output = BuildPolynomialSystem[{target},pList,variables,5,"IrreducibleMonomials"->irredMonomials]
results=ReconstructPolynomialRemainder[output] // First  // Collect[#,irredMonomials,Factor]&;
results


Coefficient[results,newvar,1]//Numerator//Factor//FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort
Coefficient[results,newvar,0]//Denominator//Factor //FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort
letters = %~Join~%%


(*using companion matrices in this case is worse as we need to go to weight 6*)
cmats = BuildCompanionMatrices[pList,variables,6,irredMonomials]


gpolcmat = BuildTargetCompanionMatrix[target,cmats]
gpolcmat // First // FFGraphDraw


results2 = ReconstructTargetCompanionMatrix[gpolcmat,irredMonomials] // Collect[#,irredMonomials,Factor]&;
results==results2


(* ::Subsection::Closed:: *)
(*Example 3 \[LongDash] j-invariant: from roots to coefficients*)


jinvariantRoots = (256 (r3^2 r4^2-r2 r3 r4 (r3+r4)+r2^2 (r3^2-r3 r4+r4^2)+r1^2 (r2^2+r3^2-r3 r4+r4^2-r2 (r3+r4))-r1 (r2^2 (r3+r4)+r3 r4 (r3+r4)+r2 (r3^2-6 r3 r4+r4^2)))^3)/((r1-r2)^2 (r1-r3)^2 (r2-r3)^2 (r1-r4)^2 (r2-r4)^2 (r3-r4)^2) // Factor


variables = {r1,r2,r3,r4}
ideal=SolveAlways[(x-r1)(x-r2)(x-r3)(x-r4)==c0+c1*x+c2*x^2+c3*x^3+c4*x^4,x]//Flatten//Map[#[[1]]-#[[2]]&,#]& // ReplaceAll[c[4]->1] // Part[#,2;;]&


irredMonomials=FindIrreducibleMonomials[ideal,variables]


variableCmats = BuildCompanionMatrices[ideal,variables,6,irredMonomials];


jinvCmat = BuildTargetCompanionMatrix[jinvariantRoots,variableCmats];


jinvCoefficients = ReconstructTargetCompanionMatrix[jinvCmat,irredMonomials,"DeleteGraph"->False] // Factor


(* ::Subsection::Closed:: *)
(*Example 4 \[LongDash] Resultant computation with Elimination Order*)


p1 = x^2*y^2 -3 + a+y^3;
p2 = a*x+c* y - 5+x*y^2*c;
p3 = a + b +b x*y^2+x^2y^2;
pList = {p1,p2,p3};
pList // MatrixForm


cexp = GroebnerBasis[pList//ReplaceAll[{a->2,b->3}],{c},{x,y}] // First // Exponent[#,c]&


variables = {x,y,c};
rowReduction = BuildPolynomialSystem[{c^cexp},pList,variables,8,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x,y},5}]; // AbsoluteTiming
rowReduction


reconstructed  = ReconstructPolynomialRemainder[rowReduction] // First; // AbsoluteTiming
reconstructed // Short


resultantFF= c^cexp - reconstructed // Together // Numerator // Factor; // AbsoluteTiming
resultantFF


(* ::Subsection::Closed:: *)
(*Example 5 \[LongDash] More complicated resultant computation with Elimination Order WIP*)


p1 = x^2*y^2 -1 + a+y^3+z;
p2 = a*x+c* y - 2+x*y^2*c+z^2;
p3 = a + b +b x*y^2+x^2y^2;
p4 = x*y*z-c+1+d*x*z;
pList = {p1,p2,p3,p4};
pList // MatrixForm


dexp = GroebnerBasis[pList//ReplaceAll[{a->2,b->3,c->7}],{d},{x,y,z}] // First // Exponent[#,d]&


variables = {x,y,z,d};
rowReduction = BuildPolynomialSystem[{d^dexp},pList,variables,18,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x,y,z},14},"PrintDebugInfo"->2]; // AbsoluteTiming
rowReduction


reconstructed  = ReconstructPolynomialRemainder[rowReduction] // First; // AbsoluteTiming
reconstructed // Short


resultantFF= d^dexp - reconstructed // Together // Numerator // Factor; // AbsoluteTiming
resultantFF


(*companion matrix side*)


irredMonomials = FindIrreducibleMonomials[pList,variables]


out = BuildPolynomialSystem[{z},pList,variables,18,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x,y,z},14},"IrreducibleMonomials"->irredMonomials,"PrintDebugInfo"->2]


(*ans = ReconstructPolynomialRemainder[out]; // AbsoluteTiming*)
(*ans*)
(*variableCmats = BuildCompanionMatrices[pList,variables,18,irredMonomials,"PrintDebugInfo"->2];*)
(*targ = BuildTargetCompanionMatrix[d^dexp,variableCmats]*)


(*GroebnerBasis[pList//ReplaceAll[{a->2,b->3,c->7}],{x,y,z,d}][[4]]*)


(* ::Subsection:: *)
(*Example 6 \[LongDash] Ice Cream Cone with Elimination Order*)


difficultLetter = p3sq(m4sq-p2sq)(m3sq-p1sq)+(m3sq-m4sq-p1sq+p2sq)(m3sq*p2sq-m4sq*p1sq) // Factor;
difficultLetter


gpol = x1 x2+m1sq x1^2 x2+m2sq x1 x2^2+x1 x3+m1sq x1^2 x3+x2 x3+m1sq x1 x2 x3+m2sq x1 x2 x3+m3sq x1 x2 x3-p1sq x1 x2 x3+m2sq x2^2 x3+m3sq x1 x3^2+m3sq x2 x3^2+x1 x4+m1sq x1^2 x4+x2 x4+m1sq x1 x2 x4+m2sq x1 x2 x4+m4sq x1 x2 x4-p2sq x1 x2 x4+m2sq x2^2 x4+m3sq x1 x3 x4+m4sq x1 x3 x4-p3sq x1 x3 x4+m3sq x2 x3 x4+m4sq x2 x3 x4-p3sq x2 x3 x4+m4sq x1 x4^2+m4sq x2 x4^2;
pList = {1-x0*gpol}~Join~D[gpol,{{x1,x2,x3,x4}}];


(* ::Subsubsection::Closed:: *)
(*x0 analysis*)


target = x0^4;


systemOutput = BuildPolynomialSystem[{target},pList,{x1,x2,x3,x4,x0},9,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x1,x2,x3,x4},5},"PrintDebugInfo"->2]


reconstructed = ReconstructPolynomialRemainder[systemOutput,"Vector"->True] // First; // AbsoluteTiming


reconstructed // Dimensions


x0L1=reconstructed[[-1]] // Factor // Denominator //FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort;
x0L2=reconstructed[[-1]] // Factor // Numerator //FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort;


(* ::Subsubsection::Closed:: *)
(*x1 analysis*)


target = x1^4;


systemOutput = BuildPolynomialSystem[{target},pList,{x0,x2,x3,x4,x1},9,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x0,x2,x3,x4},5}]


reconstructed = ReconstructPolynomialRemainder[systemOutput,"Vector"->True] // First; // AbsoluteTiming


x1L2 = reconstructed[[-1]] // Factor // Denominator //FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort;


Complement[x1L2,x0L2]


(* ::Subsubsection::Closed:: *)
(*x2 analysis*)


target = x2^4;


systemOutput = BuildPolynomialSystem[{target},pList,{x0,x1,x3,x4,x2},9,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x0,x1,x3,x4},5}]


reconstructed = ReconstructPolynomialRemainder[systemOutput,"Vector"->True] // First; // AbsoluteTiming


x2L2 = reconstructed[[-1]] // Factor // Denominator //FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort;


Complement[x2L2,x0L2]


(* ::Subsubsection::Closed:: *)
(*x3 analysis*)


target = x3^4;


systemOutput = BuildPolynomialSystem[{target},pList,{x0,x1,x2,x4,x3},9,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x0,x1,x2,x4},5}]


reconstructed = ReconstructPolynomialRemainder[systemOutput,"Vector"->True] // First; // AbsoluteTiming


x3L2 = reconstructed[[-1]] // Factor // Denominator //FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort;


Complement[x3L2,x0L2]
%/difficultLetter


(* ::Subsubsection::Closed:: *)
(*x4 analysis*)


target = x4^4;


systemOutput = BuildPolynomialSystem[{target},pList,{x0,x1,x2,x3,x4},9,"MonomialOrder"->DegreeLexicographic,"EliminateVariables"->{{x0,x1,x2,x3},5}]


reconstructed = ReconstructPolynomialRemainder[systemOutput,"Vector"->True] // First; // AbsoluteTiming


x4L2 = reconstructed[[-1]] // Factor // Denominator //FactorList // Part[#,;;,1]& // DeleteCases[#,_?(NumericQ[#]&)]&//Sort;


Complement[x4L2,x0L2]
%/difficultLetter


(* ::Subsubsection::Closed:: *)
(*Bringing it all together*)


totalLandau = (Join[x0L1,x0L2,x1L2,x2L2,x3L2,x4L2]^2//Factor // DeleteDuplicates)^(1/2)//PowerExpand // SortBy[ByteCount];


sofiaSols = {m1sq, m2sq, p3sq, m1sq - m2sq, p1sq^2 - 2 p1sq p2sq + p2sq^2 - 2 p1sq p3sq - 2 p2sq p3sq + p3sq^2, m3sq^2 - 2 m3sq m4sq + m4sq^2 - 2 m3sq p3sq - 2 m4sq p3sq + p3sq^2, -m3sq m4sq p1sq + m4sq^2 p1sq + m4sq p1sq^2 + m3sq^2 p2sq - m3sq m4sq p2sq - m3sq p1sq p2sq - m4sq p1sq p2sq + m3sq p2sq^2 + m3sq m4sq p3sq - m4sq p1sq p3sq - m3sq p2sq p3sq + p1sq p2sq p3sq, m1sq^2 m3sq^2 p1sq^2 - 2 m1sq m2sq m3sq^2 p1sq^2 + m2sq^2 m3sq^2 p1sq^2 - 2 m1sq^2 m3sq m4sq p1sq^2 + 4 m1sq m2sq m3sq m4sq p1sq^2 - 2 m2sq^2 m3sq m4sq p1sq^2 - 2 m1sq m3sq^2 m4sq p1sq^2 - 2 m2sq m3sq^2 m4sq p1sq^2 + m1sq^2 m4sq^2 p1sq^2 - 2 m1sq m2sq m4sq^2 p1sq^2 + m2sq^2 m4sq^2 p1sq^2 + 4 m1sq m3sq m4sq^2 p1sq^2 + 4 m2sq m3sq m4sq^2 p1sq^2 + m3sq^2 m4sq^2 p1sq^2 - 2 m1sq m4sq^3 p1sq^2 - 2 m2sq m4sq^3 p1sq^2 - 2 m3sq m4sq^3 p1sq^2 + m4sq^4 p1sq^2 + 2 m1sq m3sq m4sq p1sq^3 + 2 m2sq m3sq m4sq p1sq^3 - 2 m1sq m4sq^2 p1sq^3 - 2 m2sq m4sq^2 p1sq^3 - 2 m3sq m4sq^2 p1sq^3 + 2 m4sq^3 p1sq^3 + m4sq^2 p1sq^4 - 2 m1sq^2 m3sq^2 p1sq p2sq + 4 m1sq m2sq m3sq^2 p1sq p2sq - 2 m2sq^2 m3sq^2 p1sq p2sq + 2 m1sq m3sq^3 p1sq p2sq + 2 m2sq m3sq^3 p1sq p2sq + 4 m1sq^2 m3sq m4sq p1sq p2sq - 8 m1sq m2sq m3sq m4sq p1sq p2sq + 4 m2sq^2 m3sq m4sq p1sq p2sq - 2 m1sq m3sq^2 m4sq p1sq p2sq - 2 m2sq m3sq^2 m4sq p1sq p2sq - 2 m3sq^3 m4sq p1sq p2sq - 2 m1sq^2 m4sq^2 p1sq p2sq + 4 m1sq m2sq m4sq^2 p1sq p2sq - 2 m2sq^2 m4sq^2 p1sq p2sq - 2 m1sq m3sq m4sq^2 p1sq p2sq - 2 m2sq m3sq m4sq^2 p1sq p2sq + 4 m3sq^2 m4sq^2 p1sq p2sq + 2 m1sq m4sq^3 p1sq p2sq + 2 m2sq m4sq^3 p1sq p2sq - 2 m3sq m4sq^3 p1sq p2sq - 2 m1sq m3sq^2 p1sq^2 p2sq - 2 m2sq m3sq^2 p1sq^2 p2sq - 2 m1sq m3sq m4sq p1sq^2 p2sq - 2 m2sq m3sq m4sq p1sq^2 p2sq + 4 m3sq^2 m4sq p1sq^2 p2sq + 4 m1sq m4sq^2 p1sq^2 p2sq + 4 m2sq m4sq^2 p1sq^2 p2sq - 2 m3sq m4sq^2 p1sq^2 p2sq - 2 m4sq^3 p1sq^2 p2sq - 2 m3sq m4sq p1sq^3 p2sq - 2 m4sq^2 p1sq^3 p2sq + m1sq^2 m3sq^2 p2sq^2 - 2 m1sq m2sq m3sq^2 p2sq^2 + m2sq^2 m3sq^2 p2sq^2 - 2 m1sq m3sq^3 p2sq^2 - 2 m2sq m3sq^3 p2sq^2 + m3sq^4 p2sq^2 - 2 m1sq^2 m3sq m4sq p2sq^2 + 4 m1sq m2sq m3sq m4sq p2sq^2 - 2 m2sq^2 m3sq m4sq p2sq^2 + 4 m1sq m3sq^2 m4sq p2sq^2 + 4 m2sq m3sq^2 m4sq p2sq^2 - 2 m3sq^3 m4sq p2sq^2 + m1sq^2 m4sq^2 p2sq^2 - 2 m1sq m2sq m4sq^2 p2sq^2 + m2sq^2 m4sq^2 p2sq^2 - 2 m1sq m3sq m4sq^2 p2sq^2 - 2 m2sq m3sq m4sq^2 p2sq^2 + m3sq^2 m4sq^2 p2sq^2 + 4 m1sq m3sq^2 p1sq p2sq^2 + 4 m2sq m3sq^2 p1sq p2sq^2 - 2 m3sq^3 p1sq p2sq^2 - 2 m1sq m3sq m4sq p1sq p2sq^2 - 2 m2sq m3sq m4sq p1sq p2sq^2 - 2 m3sq^2 m4sq p1sq p2sq^2 - 2 m1sq m4sq^2 p1sq p2sq^2 - 2 m2sq m4sq^2 p1sq p2sq^2 + 4 m3sq m4sq^2 p1sq p2sq^2 + m3sq^2 p1sq^2 p2sq^2 + 4 m3sq m4sq p1sq^2 p2sq^2 + m4sq^2 p1sq^2 p2sq^2 - 2 m1sq m3sq^2 p2sq^3 - 2 m2sq m3sq^2 p2sq^3 + 2 m3sq^3 p2sq^3 + 2 m1sq m3sq m4sq p2sq^3 + 2 m2sq m3sq m4sq p2sq^3 - 2 m3sq^2 m4sq p2sq^3 - 2 m3sq^2 p1sq p2sq^3 - 2 m3sq m4sq p1sq p2sq^3 + m3sq^2 p2sq^4 + 2 m1sq^3 m3sq p1sq p3sq - 2 m1sq^2 m2sq m3sq p1sq p3sq - 2 m1sq m2sq^2 m3sq p1sq p3sq + 2 m2sq^3 m3sq p1sq p3sq - 2 m1sq^2 m3sq^2 p1sq p3sq + 4 m1sq m2sq m3sq^2 p1sq p3sq - 2 m2sq^2 m3sq^2 p1sq p3sq - 2 m1sq^3 m4sq p1sq p3sq + 2 m1sq^2 m2sq m4sq p1sq p3sq + 2 m1sq m2sq^2 m4sq p1sq p3sq - 2 m2sq^3 m4sq p1sq p3sq - 2 m1sq^2 m3sq m4sq p1sq p3sq - 12 m1sq m2sq m3sq m4sq p1sq p3sq - 2 m2sq^2 m3sq m4sq p1sq p3sq + 4 m1sq m3sq^2 m4sq p1sq p3sq + 4 m2sq m3sq^2 m4sq p1sq p3sq + 4 m1sq^2 m4sq^2 p1sq p3sq + 8 m1sq m2sq m4sq^2 p1sq p3sq + 4 m2sq^2 m4sq^2 p1sq p3sq - 2 m1sq m3sq m4sq^2 p1sq p3sq - 2 m2sq m3sq m4sq^2 p1sq p3sq - 2 m3sq^2 m4sq^2 p1sq p3sq - 2 m1sq m4sq^3 p1sq p3sq - 2 m2sq m4sq^3 p1sq p3sq + 2 m3sq m4sq^3 p1sq p3sq - 2 m1sq^2 m3sq p1sq^2 p3sq + 4 m1sq m2sq m3sq p1sq^2 p3sq - 2 m2sq^2 m3sq p1sq^2 p3sq + 4 m1sq^2 m4sq p1sq^2 p3sq + 8 m1sq m2sq m4sq p1sq^2 p3sq + 4 m2sq^2 m4sq p1sq^2 p3sq - 2 m1sq m3sq m4sq p1sq^2 p3sq - 2 m2sq m3sq m4sq p1sq^2 p3sq - 2 m1sq m4sq^2 p1sq^2 p3sq - 2 m2sq m4sq^2 p1sq^2 p3sq + 4 m3sq m4sq^2 p1sq^2 p3sq - 2 m4sq^3 p1sq^2 p3sq - 2 m1sq m4sq p1sq^3 p3sq - 2 m2sq m4sq p1sq^3 p3sq - 2 m4sq^2 p1sq^3 p3sq - 2 m1sq^3 m3sq p2sq p3sq + 2 m1sq^2 m2sq m3sq p2sq p3sq + 2 m1sq m2sq^2 m3sq p2sq p3sq - 2 m2sq^3 m3sq p2sq p3sq + 4 m1sq^2 m3sq^2 p2sq p3sq + 8 m1sq m2sq m3sq^2 p2sq p3sq + 4 m2sq^2 m3sq^2 p2sq p3sq - 2 m1sq m3sq^3 p2sq p3sq - 2 m2sq m3sq^3 p2sq p3sq + 2 m1sq^3 m4sq p2sq p3sq - 2 m1sq^2 m2sq m4sq p2sq p3sq - 2 m1sq m2sq^2 m4sq p2sq p3sq + 2 m2sq^3 m4sq p2sq p3sq - 2 m1sq^2 m3sq m4sq p2sq p3sq - 12 m1sq m2sq m3sq m4sq p2sq p3sq - 2 m2sq^2 m3sq m4sq p2sq p3sq - 2 m1sq m3sq^2 m4sq p2sq p3sq - 2 m2sq m3sq^2 m4sq p2sq p3sq + 2 m3sq^3 m4sq p2sq p3sq - 2 m1sq^2 m4sq^2 p2sq p3sq + 4 m1sq m2sq m4sq^2 p2sq p3sq - 2 m2sq^2 m4sq^2 p2sq p3sq + 4 m1sq m3sq m4sq^2 p2sq p3sq + 4 m2sq m3sq m4sq^2 p2sq p3sq - 2 m3sq^2 m4sq^2 p2sq p3sq - 2 m1sq^2 m3sq p1sq p2sq p3sq - 12 m1sq m2sq m3sq p1sq p2sq p3sq - 2 m2sq^2 m3sq p1sq p2sq p3sq - 2 m1sq m3sq^2 p1sq p2sq p3sq - 2 m2sq m3sq^2 p1sq p2sq p3sq - 2 m1sq^2 m4sq p1sq p2sq p3sq - 12 m1sq m2sq m4sq p1sq p2sq p3sq - 2 m2sq^2 m4sq p1sq p2sq p3sq + 12 m1sq m3sq m4sq p1sq p2sq p3sq + 12 m2sq m3sq m4sq p1sq p2sq p3sq - 2 m3sq^2 m4sq p1sq p2sq p3sq - 2 m1sq m4sq^2 p1sq p2sq p3sq - 2 m2sq m4sq^2 p1sq p2sq p3sq - 2 m3sq m4sq^2 p1sq p2sq p3sq + 4 m1sq m3sq p1sq^2 p2sq p3sq + 4 m2sq m3sq p1sq^2 p2sq p3sq - 2 m1sq m4sq p1sq^2 p2sq p3sq - 2 m2sq m4sq p1sq^2 p2sq p3sq - 2 m3sq m4sq p1sq^2 p2sq p3sq + 4 m4sq^2 p1sq^2 p2sq p3sq + 2 m4sq p1sq^3 p2sq p3sq + 4 m1sq^2 m3sq p2sq^2 p3sq + 8 m1sq m2sq m3sq p2sq^2 p3sq + 4 m2sq^2 m3sq p2sq^2 p3sq - 2 m1sq m3sq^2 p2sq^2 p3sq - 2 m2sq m3sq^2 p2sq^2 p3sq - 2 m3sq^3 p2sq^2 p3sq - 2 m1sq^2 m4sq p2sq^2 p3sq + 4 m1sq m2sq m4sq p2sq^2 p3sq - 2 m2sq^2 m4sq p2sq^2 p3sq - 2 m1sq m3sq m4sq p2sq^2 p3sq - 2 m2sq m3sq m4sq p2sq^2 p3sq + 4 m3sq^2 m4sq p2sq^2 p3sq - 2 m1sq m3sq p1sq p2sq^2 p3sq - 2 m2sq m3sq p1sq p2sq^2 p3sq + 4 m3sq^2 p1sq p2sq^2 p3sq + 4 m1sq m4sq p1sq p2sq^2 p3sq + 4 m2sq m4sq p1sq p2sq^2 p3sq - 2 m3sq m4sq p1sq p2sq^2 p3sq - 2 m3sq p1sq^2 p2sq^2 p3sq - 2 m4sq p1sq^2 p2sq^2 p3sq - 2 m1sq m3sq p2sq^3 p3sq - 2 m2sq m3sq p2sq^3 p3sq - 2 m3sq^2 p2sq^3 p3sq + 2 m3sq p1sq p2sq^3 p3sq + m1sq^4 p3sq^2 - 4 m1sq^3 m2sq p3sq^2 + 6 m1sq^2 m2sq^2 p3sq^2 - 4 m1sq m2sq^3 p3sq^2 + m2sq^4 p3sq^2 - 2 m1sq^3 m3sq p3sq^2 + 2 m1sq^2 m2sq m3sq p3sq^2 + 2 m1sq m2sq^2 m3sq p3sq^2 - 2 m2sq^3 m3sq p3sq^2 + m1sq^2 m3sq^2 p3sq^2 - 2 m1sq m2sq m3sq^2 p3sq^2 + m2sq^2 m3sq^2 p3sq^2 - 2 m1sq^3 m4sq p3sq^2 + 2 m1sq^2 m2sq m4sq p3sq^2 + 2 m1sq m2sq^2 m4sq p3sq^2 - 2 m2sq^3 m4sq p3sq^2 + 4 m1sq^2 m3sq m4sq p3sq^2 + 8 m1sq m2sq m3sq m4sq p3sq^2 + 4 m2sq^2 m3sq m4sq p3sq^2 - 2 m1sq m3sq^2 m4sq p3sq^2 - 2 m2sq m3sq^2 m4sq p3sq^2 + m1sq^2 m4sq^2 p3sq^2 - 2 m1sq m2sq m4sq^2 p3sq^2 + m2sq^2 m4sq^2 p3sq^2 - 2 m1sq m3sq m4sq^2 p3sq^2 - 2 m2sq m3sq m4sq^2 p3sq^2 + m3sq^2 m4sq^2 p3sq^2 - 2 m1sq^3 p1sq p3sq^2 + 2 m1sq^2 m2sq p1sq p3sq^2 + 2 m1sq m2sq^2 p1sq p3sq^2 - 2 m2sq^3 p1sq p3sq^2 + 4 m1sq^2 m3sq p1sq p3sq^2 - 8 m1sq m2sq m3sq p1sq p3sq^2 + 4 m2sq^2 m3sq p1sq p3sq^2 - 2 m1sq^2 m4sq p1sq p3sq^2 - 12 m1sq m2sq m4sq p1sq p3sq^2 - 2 m2sq^2 m4sq p1sq p3sq^2 - 2 m1sq m3sq m4sq p1sq p3sq^2 - 2 m2sq m3sq m4sq p1sq p3sq^2 + 4 m1sq m4sq^2 p1sq p3sq^2 + 4 m2sq m4sq^2 p1sq p3sq^2 - 2 m3sq m4sq^2 p1sq p3sq^2 + m1sq^2 p1sq^2 p3sq^2 - 2 m1sq m2sq p1sq^2 p3sq^2 + m2sq^2 p1sq^2 p3sq^2 + 4 m1sq m4sq p1sq^2 p3sq^2 + 4 m2sq m4sq p1sq^2 p3sq^2 + m4sq^2 p1sq^2 p3sq^2 - 2 m1sq^3 p2sq p3sq^2 + 2 m1sq^2 m2sq p2sq p3sq^2 + 2 m1sq m2sq^2 p2sq p3sq^2 - 2 m2sq^3 p2sq p3sq^2 - 2 m1sq^2 m3sq p2sq p3sq^2 - 12 m1sq m2sq m3sq p2sq p3sq^2 - 2 m2sq^2 m3sq p2sq p3sq^2 + 4 m1sq m3sq^2 p2sq p3sq^2 + 4 m2sq m3sq^2 p2sq p3sq^2 + 4 m1sq^2 m4sq p2sq p3sq^2 - 8 m1sq m2sq m4sq p2sq p3sq^2 + 4 m2sq^2 m4sq p2sq p3sq^2 - 2 m1sq m3sq m4sq p2sq p3sq^2 - 2 m2sq m3sq m4sq p2sq p3sq^2 - 2 m3sq^2 m4sq p2sq p3sq^2 + 4 m1sq^2 p1sq p2sq p3sq^2 + 8 m1sq m2sq p1sq p2sq p3sq^2 + 4 m2sq^2 p1sq p2sq p3sq^2 - 2 m1sq m3sq p1sq p2sq p3sq^2 - 2 m2sq m3sq p1sq p2sq p3sq^2 - 2 m1sq m4sq p1sq p2sq p3sq^2 - 2 m2sq m4sq p1sq p2sq p3sq^2 + 4 m3sq m4sq p1sq p2sq p3sq^2 - 2 m1sq p1sq^2 p2sq p3sq^2 - 2 m2sq p1sq^2 p2sq p3sq^2 - 2 m4sq p1sq^2 p2sq p3sq^2 + m1sq^2 p2sq^2 p3sq^2 - 2 m1sq m2sq p2sq^2 p3sq^2 + m2sq^2 p2sq^2 p3sq^2 + 4 m1sq m3sq p2sq^2 p3sq^2 + 4 m2sq m3sq p2sq^2 p3sq^2 + m3sq^2 p2sq^2 p3sq^2 - 2 m1sq p1sq p2sq^2 p3sq^2 - 2 m2sq p1sq p2sq^2 p3sq^2 - 2 m3sq p1sq p2sq^2 p3sq^2 + p1sq^2 p2sq^2 p3sq^2 + 2 m1sq^3 p3sq^3 - 2 m1sq^2 m2sq p3sq^3 - 2 m1sq m2sq^2 p3sq^3 + 2 m2sq^3 p3sq^3 - 2 m1sq^2 m3sq p3sq^3 + 4 m1sq m2sq m3sq p3sq^3 - 2 m2sq^2 m3sq p3sq^3 - 2 m1sq^2 m4sq p3sq^3 + 4 m1sq m2sq m4sq p3sq^3 - 2 m2sq^2 m4sq p3sq^3 + 2 m1sq m3sq m4sq p3sq^3 + 2 m2sq m3sq m4sq p3sq^3 - 2 m1sq^2 p1sq p3sq^3 + 4 m1sq m2sq p1sq p3sq^3 - 2 m2sq^2 p1sq p3sq^3 - 2 m1sq m4sq p1sq p3sq^3 - 2 m2sq m4sq p1sq p3sq^3 - 2 m1sq^2 p2sq p3sq^3 + 4 m1sq m2sq p2sq p3sq^3 - 2 m2sq^2 p2sq p3sq^3 - 2 m1sq m3sq p2sq p3sq^3 - 2 m2sq m3sq p2sq p3sq^3 + 2 m1sq p1sq p2sq p3sq^3 + 2 m2sq p1sq p2sq p3sq^3 + m1sq^2 p3sq^4 - 2 m1sq m2sq p3sq^4 + m2sq^2 p3sq^4};


(*spurious singularity*)
Complement[sofiaSols,totalLandau]
