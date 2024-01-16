(* ::Package:: *)

BeginPackage["SUSYHelper`"];


BracketRep::usage = "BracketRep[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"str\",\nFontSlant->\"Italic\"]\)] Prints in LaTeX a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)of L matrices in vector bracket notation with label \!\(\*
StyleBox[\"str\",\nFontSlant->\"Italic\"]\)";
PauliRep::usage = "PauliRep[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"str\",\nFontSlant->\"Italic\"]\)] Prints elements of \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) as a kronecker product of pauli matrices, or if not applicable, in bracket notation, enumerated with the label \!\(\*
StyleBox[\"str\",\nFontSlant->\"Italic\"]\).";
PauliRepRetH::usage = "PauliRepRetH[\!\(\*
StyleBox[\"Matrix\",\nFontSlant->\"Italic\"]\)] Represents a single permutation \!\(\*
StyleBox[\"matrix\",\nFontSlant->\"Italic\"]\) as a kronecker product of pauli matrices, or if not applicable, in bracket notation";
PauliRepRetB::usage = "PauliRepRetB[\!\(\*
StyleBox[\"Matrix\",\nFontSlant->\"Italic\"]\)] Represents a single sign factor \!\(\*
StyleBox[\"matrix\",\nFontSlant->\"Italic\"]\) as a kronecker product of pauli matrices, or if not applicable, in boolean factor notation";
FindHRs::usage = "FindHRs[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Find the right-handed hopping operators for a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of L matrices using the inverse operation";
FindHLs::usage = "FindHLs[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Find the left-handed hopping operators for a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of L matrices using the inverse operation";
FindHRs2::usage = "FindHRs2[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Find the right-handed hopping operators for a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of L matrices using the transpose operation";
FindHLs2::usage = " FindHLs2[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Find the right-handed hopping operators for a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of L matrices using the transpose operation";
FindBs::usage = "FindBs[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Find the sign factors of a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of L matrices";
FindHBRs::usage = "FindHBRs[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Find the connections between a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of sign factors";
WeightOrder::usage = "WeightOrder[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Weigh and return a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)of L matrices by order; in other words, the value of the number which describes the matrix in bracket notation";
PlotHanging::usage = "PlotHanging[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"optional\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)] Shows the graph of all sign factor matrices and their connections for N=1 Off-shell, AKA the Klein Vierergruppe, and highlights the nodes green for a \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of corresponding matrices";
PlotHanging4G::usage = "PlotHanging4G[\!\(\*
StyleBox[\"nested\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Shows the graph of all sign factor matrices and their connections for N=1 Off-shell, AKA the Klein Vierergruppe, and higlights multiple \!\(\*
StyleBox[\"lists\",\nFontSlant->\"Italic\"]\) of nodes in different colors";
PlotTesseract::usage = "PlotTesseract[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Shows the graph of all sign factor matrices and their connections for N=1 Off-shell, AKA the Klein Vierergruppe, and highlights the nodes green for \!\(\*
StyleBox[\"a\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\) of corresponding matrices";
PlotTesseract4G::usage = "PlotTesseract4G[\!\(\*
StyleBox[\"nested\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)] Shows the graph of all sign factor matrices and their connections for N=1 Off-shell, AKA the Klein Vierergruppe, and higlights multiple \!\(\*
StyleBox[\"lists\",\nFontSlant->\"Italic\"]\) of nodes in different colors";
GraphVAdinkra::usage = "GraphVAdinkra[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"L\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"matrices\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"R\",\nFontSlant->\"Italic\"]\) \!\(\*
StyleBox[\"matrices\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"int\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"pixel\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"size\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)] Shows the Valise Adinkra for given \!\(\*
StyleBox[\"lists\",\nFontSlant->\"Italic\"]\) of L and R matrices";
GraphBigVAdinkra::usage = "GraphBigVAdinkra[\!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"L\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"matrices\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"list\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"R\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"matrices\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"int\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"(\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"pixel\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"size\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\")\",\nFontSlant->\"Italic\"]\)] Shows the Valise Adinkra for a given \!\(\*
StyleBox[\"lists\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)of L and R matrices";
LRMatricesSM::usage = "LRMatricesSM[] Returns the L and R Matrices for the 4D N=4 Super-Maxwell Theory with N=1 off-shell";
LRMatricesVT::usage = "LRMatricesVT[] Returns the L and R Matrices for the 4D N=4 Vector Tensor Theory with N=2 off-shell";
LRMatricesSP::usage = "LRMatricesSP[\!\(\*
StyleBox[\"N\",\nFontSlant->\"Italic\"]\)] Returns the L and R Matrices for the 1D \!\(\*
StyleBox[\"N\",\nFontSlant->\"Italic\"]\)=2,4,8,9,10,12, and 16 Spinning Particle Theories";
LRMatricesSM10D::usage = "LRMatricesSM10D[] Returns the L and R matrices for the on-shell 10D N=1 Super-Maxwell Multiplet";
LRMatricesSM4D::usage = "LRMatricesSM4D[] Returns the L and R matrices for the on-shell 4D N=4 Super-Maxwell multiplet";
GRdN::usage = "GRdN[\!\(\*
StyleBox[\"Ls\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\",\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"Rs\",\nFontSlant->\"Italic\"]\)] Returs True if \!\(\*
StyleBox[\"Ls\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\)and \!\(\*
StyleBox[\"Rs\",\nFontSlant->\"Italic\"]\)\*
StyleBox[\(\!\(\*
StyleBox[\" \",\nFontSlant->\"Italic\"]\) \)]satisfy a Garden Algebra and False otherwise."


Begin["`Private`"];


Needs["Adinkra`"]


\[Sigma]0 = IdentityMatrix[2];
\[Sigma]1 = PauliMatrix[1];
\[Sigma]2 = PauliMatrix[2];
\[Sigma]3 = PauliMatrix[3];
\[Sigma]s = {\[Sigma]0, \[Sigma]1, \[Sigma]2, \[Sigma]3};
\[Sigma]ts = {\[Sigma]0, \[Sigma]3};
\[Sigma]bs = {\[Sigma]0, \[Sigma]1};
kp = KroneckerProduct;


BracketRep[Ls_, str_] := (dim = Dimensions[Ls]; For[n = 1, n < dim[[1]] + 1, n++, 
    STR = Null; vstr = ""; STR = Catch[Vect = Table[ii, {ii, 1, dim[[3]]}]; 
        Vect = Ls[[n]] . Vect; For[m = 1, m < dim[[2]] + 1, m++, If[Vect[[m]] < 0, 
          vstr = StringJoin[vstr, "\\Bar{", ToString[-Vect[[m]]], "}\\;"], 
          vstr = StringJoin[vstr, ToString[Vect[[m]]], "\\;"]]]; 
        Throw[StringJoin[str, "_{", ToString[n], "} = \\langle", vstr, "\\rangle"]]]; 
     Print[STR]])


PauliRep[Ls_,st_] :=( 

dim = Dimensions[Ls];
If[dim[[2]]==dim[[3]],

Bs = Table[0,{i, 1, dim[[1]]}];
For[n=1, n<dim[[1]]+1, n++,Bs[[n]] = LinearSolve[Ls[[n]],Abs[Ls[[n]]]]];
Bstr = Table[0,{i, 1, dim[[1]]}];

If[dim[[2]]==4,
For[n=1, n<dim[[1]]+1, n++,Bstr[[n]]=Null;Bstr[[n]] =
Catch[For[a=1,a<3, a++,For[b=1, b<3, b++,If[Bs[[n]]== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]]], Throw[StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"]],If[Bs[[n]]== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]]], Throw[StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],"\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"] ]]]]]];If[Bstr[[n]]==Null, Bstr[[n]] = StringJoin["B",ToString[Catch[Num = 0;For[ii=1, ii<5, ii++,If[Bs[[n,ii,ii]]==0,Num+=0,Num+=2^(ii-1)*(Bs[[n,ii,ii]]*(-1)+1)/2]];Throw[Num]]]]]];

For[n=1, n<dim[[1]]+1, n++,STR = Null;STR = Catch[For[a=1, a<3, a++,For[b=1, b<3, b++,If[Abs[Ls[[n]]]== KroneckerProduct[\[Sigma]bs[[a]],\[Sigma]bs[[b]]], Throw[StringJoin[st,ToString[n]," = (",ToString[a-1],",",ToString[b-1], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(i\)], \(k\)]\)", Bstr[[n]]]] ]]];If[STR==Null, Vect = Table[ii, {ii, 1, 4}]; Vect =Abs[Ls[[n]]].Vect; Throw[StringJoin[st,ToString[n]," = <", ToString[Vect[[;;]]],"\!\(\*SuperscriptBox[SubscriptBox[\(>\), \(i\)], \(k\)]\)", Bstr[[n]]] ]]];Print[STR]],

If[dim[[2]]==8,

For[n=1, n<dim[[1]]+1, n++,Bstr[[n]]=Null;Bstr[[n]] =
Catch[For[a=1,a<3, a++,For[b=1, b<3, b++,For[c=1,c<3, c++,If[Bs[[n]]== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]]], Throw[StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],"\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"]],If[Bs[[n]]== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]]],Throw[StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"]] ]]]]]];If[Bstr[[n]]==Null, Bstr[[n]] = StringJoin["B",ToString[Catch[Num = 0;For[ii=1, ii<7, ii++,If[Bs[[n,ii,ii]]==0,Num+=0,Num+=2^(ii-1)*(Bs[[n,ii,ii]]*(-1)+1)/2]];Throw[Num]]]]]];;

For[n=1, n<dim[[1]]+1, n++,STR = Null;STR = Catch[For[a=1, a<3, a++,For[b=1, b<3, b++,For[c=1, c<3, c++,If[Abs[Ls[[n]]]== KroneckerProduct[\[Sigma]bs[[a]],\[Sigma]bs[[b]],\[Sigma]bs[[c]]], Throw[StringJoin[st,ToString[n]," = (",ToString[a-1],",",ToString[b-1],",",ToString[c-1],"\!\(\*SuperscriptBox[SubscriptBox[\()\), \(i\)], \(k\)]\)", Bstr[[n]]]] ]]]];If[STR==Null, Vect = Table[ii, {ii, 1, 8}]; Vect =Abs[Ls[[n]]].Vect; Throw[StringJoin[st,ToString[n]," = <", ToString[Vect[[;;]]],"\!\(\*SuperscriptBox[SubscriptBox[\(>\), \(i\)], \(k\)]\)", Bstr[[n]]] ]]];Print[STR]],

If[dim[[3]]==16,
For[n=1, n<dim[[1]]+1, n++,Bstr[[n]]=Null;Bstr[[n]] =Catch[For[a=1,a<3, a++,For[b=1, b<3, b++,For[c=1,c<3, c++,For[d=1, d<3, d++,If[Bs[[n]]== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]]], Bstr[[n]]=StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"],If[Bs[[n]]== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]]], Bstr[[n]]=StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"] ]]]]]]];If[Bstr[[n]]==Null, Bstr[[n]] = StringJoin["B",ToString[Catch[Num = 0;For[ii=1, ii<17, ii++,If[Bs[[n,ii,ii]]==0,Num=Num+0,Num=Num+2^(ii-1)*(Bs[[n,ii,ii]]*(-1)+1)/2]];Throw[Num]]]]]];

For[n=1, n<dim[[1]]+1, n++,STR = Null;STR = Catch[For[a=1, a<3, a++,For[b=1, b<3, b++,For[c=1, c<3, c++,For[d=1, d<3, d++,If[Abs[Ls[[n]]]== KroneckerProduct[\[Sigma]bs[[a]],\[Sigma]bs[[b]],\[Sigma]bs[[c]],\[Sigma]bs[[d]]], Throw[StringJoin[st,ToString[n]," = (",ToString[a-1],",",ToString[b-1],",",ToString[c-1],",",ToString[d-1], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(i\)], \(k\)]\)", Bstr[[n]]]] ]]]]];If[STR==Null, Vect = Table[ii, {ii, 1, 16}]; Vect =Abs[Ls[[n]]].Vect; Throw[StringJoin[st,ToString[n]," = <", ToString[Vect[[;;]]],"\!\(\*SuperscriptBox[SubscriptBox[\(>\), \(i\)], \(k\)]\)"(*, Bstr[[n]]*)]]] ];Print[STR]],

If[dim[[2]]==128, 

For[n=1, n<dim[[1]]+1, n++,Bstr[[n]]= Null; For[a=1,a<3, a++,For[b=1, b<3, b++,For[c=1,c<3, c++,For[d=1, d<3, d++,For[e=1, e<3, e++,For[f=1, f<3, f++,For[h=1, h<3, h++,If[Bs[[n]]== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]],\[Sigma]ts[[e]],\[Sigma]ts[[f]],\[Sigma]ts[[h]]], Bstr[[n]]=StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)],",",ToString[3(e-1)],",",ToString[3(f-1)],",",ToString[3(h-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"],If[Bs[[n]]== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]],\[Sigma]ts[[e]],\[Sigma]ts[[f]],\[Sigma]ts[[h]]], Bstr[[n]]=StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)],",",ToString[3(e-1)],",",ToString[3(f-1)],",",ToString[3(h-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"]]]]]]]]]];
If[Bstr[[n]]== Null, Bstr[[n]]="(...)"]];

For[n=1, n<dim[[1]]+1, n++,STR = Null;STR = Catch[For[a=1, a<3, a++,For[b=1, b<3, b++,For[c=1, c<3, c++,For[d=1, d<3, d++,For[aa=1,aa<3,aa++,For[bb=1, bb<3, bb++,For[cc=1, cc<3, cc++,If[Abs[Ls[[n]]]== KroneckerProduct[\[Sigma]bs[[a]],\[Sigma]bs[[b]],\[Sigma]bs[[c]],\[Sigma]bs[[d]],\[Sigma]bs[[aa]],\[Sigma]bs[[bb]],\[Sigma]bs[[cc]]], Throw[StringJoin[st,ToString[n]," = (",ToString[a-1],",",ToString[b-1],",",ToString[c-1],",",ToString[d-1],",",ToString[aa-1],",",ToString[bb-1],",",ToString[cc-1], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(i\)], \(k\)]\)", Bstr[[n]]]] ]]]]]]]];If[STR==Null, Vect = Table[ii, {ii, 1, 128}]; Vect =Abs[Ls[[n]]].Vect; Throw[StringJoin[st,ToString[n]," = <", ToString[Vect[[;;]]],"\!\(\*SuperscriptBox[SubscriptBox[\(>\), \(i\)], \(k\)]\)", Bstr[[n]]]]] ];Print[STR]]

]]]],If[dim[[3]]==16,
For[n=1, n<dim[[1]]+1, n++,STR = Null;STR = Catch[Vect = Table[ii, {ii, 1, 16}]; Vect =Abs[Ls[[n]]].Vect; Throw[StringJoin[st,ToString[n]," = <", ToString[Vect[[;;]]],"\!\(\*SuperscriptBox[SubscriptBox[\(>\), \(i\)], \(k\)]\)"]]] ;Print[STR]]]]

)


PauliRepRetH[H_]:=(

If[Dimensions[H][[1]] == 8, 
STR = Null;STR = Catch[For[a=1, a<3, a++,For[b=1, b<3, b++,For[c=1, c<3, c++,If[H== KroneckerProduct[\[Sigma]bs[[a]],\[Sigma]bs[[b]],\[Sigma]bs[[c]]], Throw[StringJoin["(",ToString[a-1],",",ToString[b-1],",",ToString[c-1], ")"]] ]]]];If[STR==Null, Vect = Table[ii, {ii, 1, 8}]; Vect =H.Vect; Throw[StringJoin["<", ToString[Vect[[;;]]],">"]]]],

If[Dimensions[H][[1]] == 128, 
STR = Null;STR = Catch[For[a=1, a<3, a++,For[b=1, b<3, b++,For[c=1, c<3, c++,For[d=1, d<3, d++,For[aa=1,aa<3,aa++,For[bb=1, bb<3, bb++,For[cc=1, cc<3, cc++,If[H== KroneckerProduct[\[Sigma]bs[[a]],\[Sigma]bs[[b]],\[Sigma]bs[[c]],\[Sigma]bs[[d]],\[Sigma]bs[[aa]],\[Sigma]bs[[bb]],\[Sigma]bs[[cc]]], Throw[StringJoin["(",ToString[a-1],",",ToString[b-1],",",ToString[c-1],",",ToString[d-1],",",ToString[aa-1],",",ToString[bb-1],",",ToString[cc-1], ")"]] ]]]]]]]];If[STR==Null, Vect = Table[ii, {ii, 1, 128}]; Vect =H.Vect; Throw[StringJoin["<", ToString[Vect[[;;]]],">"]]]]]];
Return[STR]
)


PauliRepRetB[B_]:= (
If[Dimensions[B][[1]]==4,
Bstr=Null;
For[a=1,a<3, a++,For[b=1, b<3, b++,If[B== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]]], Bstr=StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)],")"],If[B== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]]], Bstr=StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],")"] ]]]];
If[Bstr==Null, Bstr = StringJoin["B",ToString[Catch[Num = 0;For[ii=1, ii<5, ii++,If[B[[ii,ii]]==0,Num=Num+0,Num=Num+2^(ii-1)*(B[[ii,ii]]*(-1)+1)/2]];Throw[Num]]]]],

If[Dimensions[B][[1]]==8,
Bstr=Null;
For[a=1,a<3, a++,For[b=1, b<3, b++,For[c=1,c<3, c++,If[B== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]]], Bstr=StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)], ")"],If[B== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]]], Bstr=StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],")"] ]]]]];
If[Bstr==Null, Bstr = StringJoin["B",ToString[Catch[Num = 0;For[ii=1, ii<9, ii++,If[B[[ii,ii]]==0,Num=Num+0,Num=Num+2^(ii-1)*(B[[ii,ii]]*(-1)+1)/2]];Throw[Num]]]]],

If[Dimensions[B][[1]]==16,
Bstr=Null;
For[a=1,a<3, a++,For[b=1, b<3, b++,For[c=1,c<3, c++,For[d=1, d<3, d++,If[B== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]]], Bstr=StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)], ")"],If[B== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]]], Bstr=StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)],")"] ]]]]];
If[Bstr==Null, Bstr = StringJoin["B",ToString[Catch[Num = 0;For[ii=1, ii<17, ii++,If[B[[ii,ii]]==0,Num=Num+0,Num=Num+2^(ii-1)*(B[[ii,ii]]*(-1)+1)/2]];Throw[Num]]]]]],

If[Dimensions[B][[1]]==128, 
Bstr=Null;
For[a=1,a<3, a++,For[b=1, b<3, b++,For[c=1,c<3, c++,For[d=1, d<3, d++,For[e=1, e<3, e++,For[f=1, f<3, f++,For[h=1, h<3, h++,If[B== KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]],\[Sigma]ts[[e]],\[Sigma]ts[[f]],\[Sigma]ts[[h]]], Bstr=StringJoin["(",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)],",",ToString[3(e-1)],",",ToString[3(f-1)],",",ToString[3(h-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"],If[B== -KroneckerProduct[\[Sigma]ts[[a]],\[Sigma]ts[[b]],\[Sigma]ts[[c]],\[Sigma]ts[[d]],\[Sigma]ts[[e]],\[Sigma]ts[[f]],\[Sigma]ts[[h]]], Bstr=StringJoin["(-",ToString[3*(a-1)],",",ToString[3*(b-1)],",",ToString[3*(c-1)],",",ToString[3*(d-1)],",",ToString[3(e-1)],",",ToString[3(f-1)],",",ToString[3(h-1)], "\!\(\*SuperscriptBox[SubscriptBox[\()\), \(k\)], \(j\)]\)"] ]]]]]]]]];
If[Bstr==Null, Bstr = StringJoin["B",ToString[Catch[Num = 0;For[ii=1, ii<129, i++,If[B[[ii,ii]]==0,Num=Num+0,Num=Num+2^(ii-1)*(B[[ii,ii]]*(-1)+1)/2]];Throw[Num]]]]]]]]];
Return[Bstr]
)


FindHRs[Ls_]:=(
HHs = Table[Abs[Inverse[Ls[[j]]].Ls[[i]]],{j,Dimensions[Ls][[1]]},{i,Dimensions[Ls][[1]]}];
Return[HHs]
)
FindHLs[Ls_]:=(
HHs = Table[Abs[Ls[[i]].Inverse[Ls[[j]]]],{j,Dimensions[Ls][[1]]},{i,Dimensions[Ls][[1]]}];
Return[HHs]
)
FindHRs2[Ls_]:=(
HHs = Table[Abs[Transpose[Ls[[j]]].Ls[[i]]],{j,Dimensions[Ls][[1]]},{i,Dimensions[Ls][[1]]}];
Return[HHs]
)
FindHLs2[Ls_]:=(
HHs = Table[Abs[Ls[[i]].Transpose[Ls[[j]]]],{j,Dimensions[Ls][[1]]},{i,Dimensions[Ls][[1]]}];
Return[HHs]
)
FindBs[Ls_]:=(
BBs = Table[Ls[[i]].Abs[Transpose[Ls[[i]]]],{i, Dimensions[Ls][[1]]}];
Return[BBs]
)
FindHBRs[Bs_]:=(
HHBs = Table[LinearSolve[Bs[[i]],Bs[[j]]],{j,Dimensions[Bs][[1]]},{i,Dimensions[Bs][[1]]}];
Return[HHBs]
)


WeightOrder[Ls_]:= (

dim = Dimensions[Ls];
NL =dim[[1]];
d1 = dim[[2]];
d2 = dim[[3]];
LsWtd = Table[0,{i, 1, NL}];
AA = Table[0,{i,NL}];
For[ii=1, ii<NL+1, ii++, Vect = Table[a, {a, 1, d2}]; Vect =Abs[Ls[[ii]]].Vect;AA[[ii]]=FromDigits[Flatten[Vect]]];
BB = Sort[AA];
For[ii=1, ii<NL+1, ii++,LsWtd[[ii]] = Ls[[Position[AA,BB[[ii]]][[1,1]]]] ];
Return[LsWtd]
)


GraphVAdinkra[L\[LetterSpace]I_, R\[LetterSpace]I_,size_]:=(
numFermions=Length[L\[LetterSpace]I[[1,1,;;]]];
numBosons = Length[L\[LetterSpace]I[[1,;;,1]]];
numColors = Length[L\[LetterSpace]I[[;;,1,1]]];
If[numFermions > numBosons,numConnections = numBosons,numConnections = numFermions];
If[numFermions > numBosons,AdinkraWidth = numFermions,AdinkraWidth = numBosons] ;
xStartingPosFermions = (AdinkraWidth - numFermions)/2 ;
xStartingPosBosons = (AdinkraWidth - numBosons )/2;

AdinkraCoordinates = Table[0, {n, 1, numColors},{h, 1, numConnections},{i,1,2},{j, 1, 2}];
Dashes = Table[0, {n, 1, numColors},{h, 1, numConnections}];
For[n=1, n<numColors+1, n++,h=1;For[j=1, j<numFermions+1, j++,For[i=1, i<numBosons+1, i++, If[L\[LetterSpace]I[[n, i, j]] =!= 0&& h<numConnections+1,Dashes[[n,h]] = L\[LetterSpace]I[[n, i, j]];
AdinkraCoordinates[[n,h, 1,1]] = i+xStartingPosBosons;AdinkraCoordinates[[n,h, 1,2]] = 1;AdinkraCoordinates[[n,h,2, 1]] = j + xStartingPosFermions;
AdinkraCoordinates[[n,h,2, 2]] = 2;
h++
]]]];

(*The connection for each boson-fermion pair is sorted into solid and dashed collections:*)

AdinkraCoordinatesSolid =  Table[0, {n, 1, numColors},{h, 1, numConnections},{i,1,2},{j, 1, 2}];
AdinkraCoordinatesDashed =  Table[0, {n, 1, numColors},{h, 1, numConnections},{i,1,2},{j, 1, 2}];
For[n=1, n<numColors+1, n++,For[h=1, h<numConnections+1, h++,If[Dashes[[n,h]] == 1, AdinkraCoordinatesSolid[[n,h]] = AdinkraCoordinates[[n,h]],AdinkraCoordinatesDashed[[n,h]] = AdinkraCoordinates[[n,h]]]]];

(*The connections are generated using the ListLinePlot function.*)

g = Table[0,{d,1,2},{n,1,numColors}];
For[n=1, n<numColors+1, n++,g[[1,n]] =ListLinePlot[AdinkraCoordinatesSolid[[n,;;,;;,;;]],PlotStyle->Hue[n/numColors],ImageSize->size];g[[2,n]] =ListLinePlot[AdinkraCoordinatesDashed[[n,;;,;;,;;]],PlotStyle->Directive[Hue[n/numColors],Dashed]]];

(*The vertices are generated using the ListPlot function.Vertex graphics were rendered using the Graphics function,and numbered using plot labeling.*)

BVertices = ListPlot[Table[{i+xStartingPosBosons,1},{i,1,numBosons}]->Table[ToString[i],{i,1,numBosons}],LabelingFunction->Center, PlotMarkers->{Graphics[{EdgeForm[Directive[Thick,Black]],White,Disk[]}],.05} ];
FVertices = ListPlot[Table[{i+xStartingPosFermions,2},{i,1,numFermions}]->Table[ToString[i],{i,1,numFermions}], LabelingFunction->Center,LabelStyle->White,PlotMarkers->{Graphics[{Black,Disk[]}],.05}];

(*All of the plots must be displayed using the Show function.*)

Return[Show[g[[1,;;]],g[[2,;;]],BVertices,FVertices,PlotRange->{{0.1,AdinkraWidth+1},{.9,2.1}},Axes ->False]])


GraphBigVAdinkra[L\[LetterSpace]I_, R\[LetterSpace]I_,size_]:=(
numFermions=Length[L\[LetterSpace]I[[1,1,;;]]];
numBosons = Length[L\[LetterSpace]I[[1,;;,1]]];
numColors = Length[L\[LetterSpace]I[[;;,1,1]]];
If[numFermions > numBosons,numConnections = numBosons,numConnections = numFermions];
If[numFermions > numBosons,AdinkraWidth = numFermions,AdinkraWidth = numBosons] ;
xStartingPosFermions = (AdinkraWidth - numFermions)/2 ;
xStartingPosBosons = (AdinkraWidth - numBosons )/2;

AdinkraCoordinates = Table[0, {n, 1, numColors},{h, 1, numConnections},{i,1,2},{j, 1, 2}];
Dashes = Table[0, {n, 1, numColors},{h, 1, numConnections}];
For[n=1, n<numColors+1, n++,h=1;For[j=1, j<numFermions+1, j++,For[i=1, i<numBosons+1, i++, If[L\[LetterSpace]I[[n, i, j]] =!= 0&& h<numConnections+1,Dashes[[n,h]] = L\[LetterSpace]I[[n, i, j]];
AdinkraCoordinates[[n,h, 1,1]] = i+xStartingPosBosons;AdinkraCoordinates[[n,h, 1,2]] = 1;AdinkraCoordinates[[n,h,2, 1]] = j + xStartingPosFermions;
AdinkraCoordinates[[n,h,2, 2]] = 2;
h++
]]]];

AdinkraCoordinatesSolid =  Table[0, {n, 1, numColors},{h, 1, numConnections},{i,1,2},{j, 1, 2}];
AdinkraCoordinatesDashed =  Table[0, {n, 1, numColors},{h, 1, numConnections},{i,1,2},{j, 1, 2}];
For[n=1, n<numColors+1, n++,For[h=1, h<numConnections+1, h++,If[Dashes[[n,h]] == 1, AdinkraCoordinatesSolid[[n,h]] = AdinkraCoordinates[[n,h]],AdinkraCoordinatesDashed[[n,h]] = AdinkraCoordinates[[n,h]]]]];

g = Table[0,{d,1,2},{n,1,numColors}];
For[n=1, n<numColors+1, n++,g[[1,n]] =ListLinePlot[AdinkraCoordinatesSolid[[n,;;,;;,;;]],PlotStyle->Hue[n/numColors], ImageSize->size];g[[2,n]] =ListLinePlot[AdinkraCoordinatesDashed[[n,;;,;;,;;]],PlotStyle->Directive[Hue[n/numColors],Dashed]]];

BVertices = BVertices = ListPlot[Table[{i+xStartingPosBosons,1},{i,1,numBosons}], PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],White,Disk[]}],.01} ];
FVertices = ListPlot[Table[{i+xStartingPosFermions,2},{i,1,numFermions}],PlotMarkers->{Graphics[{Black,Disk[]}],.01}];

Return[Show[g[[1,;;]],g[[2,;;]],BVertices,FVertices,PlotRange->{{0.1,AdinkraWidth+1},{.9,2.1}}, Axes ->False]])
(*
	BVertices = ListPlot[Table[{i+xStartingPosBosons,1},{i,1,numBosons}], PlotMarkers\[Rule]{Graphics[{EdgeForm[Directive[Thin,Black]],White,Disk[]}],.01} ];
    FVertices = ListPlot[Table[{i+xStartingPosFermions,2},{i,1,numFermions}],PlotMarkers\[Rule]{Graphics[{Black,Disk[]}],.01}];*)


PlotHanging[] :=(
Nodes = {{0,0,0,0},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,1,0,0},{1,0,1,0},{1,0,0,1},{0,1,1,0},{0,1,0,1},{0,0,1,1},{1,1,1,0},{1,1,0,1},{1,0,1,1},{0,1,1,1},{1,1,1,1}};
Points = {{0, 0},{-1.5,1},{-.5,1},{.5,1},{1.5,1},{-2.5,2},{-1.5,2},{-.5,2},{.5,2},{1.5,2},{2.5,2},{-1.5,3},{-.5,3},{.5,3},{1.5,3},{0,4}};
Lines = {{Points[[1]],Points[[2]]},{Points[[1]],Points[[3]]},{Points[[1]],Points[[4]]},{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[2]],Points[[7]]},{Points[[2]],Points[[8]]},{Points[[3]],Points[[6]]},{Points[[3]],Points[[9]]},{Points[[3]],Points[[10]]},{Points[[4]],Points[[7]]},{Points[[4]],Points[[9]]},{Points[[4]],Points[[11]]},{Points[[5]],Points[[8]]},{Points[[5]],Points[[10]]},{Points[[5]],Points[[11]]},{Points[[6]],Points[[12]]},{Points[[6]],Points[[13]]},{Points[[7]],Points[[12]]},{Points[[7]],Points[[14]]},{Points[[9]],Points[[12]]},{Points[[9]],Points[[15]]},{Points[[8]],Points[[14]]},{Points[[8]],Points[[13]]},{Points[[10]],Points[[15]]},{Points[[10]],Points[[13]]},{Points[[11]],Points[[15]]},{Points[[11]],Points[[14]]},{Points[[12]],Points[[16]]},{Points[[15]],Points[[16]]},{Points[[14]],Points[[16]]},{Points[[13]],Points[[16]]}};
Show[ListLinePlot[Lines,PlotStyle->Black], ListPlot[Points->Nodes,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],White,Disk[]}],.1}],Axes->False, ImageSize->{1000},PlotRangePadding->.5]
)
PlotHanging[Bs_] := (
Highlightd = Table[(Bs[[nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[1]]},{ii,1,Dimensions[Bs][[2]]}];
Nodes = {{0,0,0,0},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,1,0,0},{1,0,1,0},{1,0,0,1},{0,1,1,0},{0,1,0,1},{0,0,1,1},{1,1,1,0},{1,1,0,1},{1,0,1,1},{0,1,1,1},{1,1,1,1}};
Points = {{0, 0},{-1.5,1},{-.5,1},{.5,1},{1.5,1},{-2.5,2},{-1.5,2},{-.5,2},{.5,2},{1.5,2},{2.5,2},{-1.5,3},{-.5,3},{.5,3},{1.5,3},{0,4}};
Colored = Table[Points[[Position[Nodes,Highlightd[[nn]]][[1,1]]]],{nn,1,Dimensions[Bs][[1]]}];
Lines = {{Points[[1]],Points[[2]]},{Points[[1]],Points[[3]]},{Points[[1]],Points[[4]]},{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[2]],Points[[7]]},{Points[[2]],Points[[8]]},{Points[[3]],Points[[6]]},{Points[[3]],Points[[9]]},{Points[[3]],Points[[10]]},{Points[[4]],Points[[7]]},{Points[[4]],Points[[9]]},{Points[[4]],Points[[11]]},{Points[[5]],Points[[8]]},{Points[[5]],Points[[10]]},{Points[[5]],Points[[11]]},{Points[[6]],Points[[12]]},{Points[[6]],Points[[13]]},{Points[[7]],Points[[12]]},{Points[[7]],Points[[14]]},{Points[[9]],Points[[12]]},{Points[[9]],Points[[15]]},{Points[[8]],Points[[14]]},{Points[[8]],Points[[13]]},{Points[[10]],Points[[15]]},{Points[[10]],Points[[13]]},{Points[[11]],Points[[15]]},{Points[[11]],Points[[14]]},{Points[[12]],Points[[16]]},{Points[[15]],Points[[16]]},{Points[[14]],Points[[16]]},{Points[[13]],Points[[16]]}};
Show[ListLinePlot[Lines,PlotStyle->Black], ListPlot[Points->Nodes,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],White,Disk[]}],.1}],ListPlot[Colored->Highlightd,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],Green,Disk[]}],.1}],Axes->False, ImageSize->{1000},PlotRangePadding->.5]
)
PlotHanging4G[Bs_] := (
Rainbow ={Lighter[Red],Lighter[Orange], Lighter[Yellow],Lighter[Green], Darker[Cyan],Lighter[Blue], Lighter[Magenta],Lighter[Purple]};
Highlight1 = Table[(Bs[[1,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Highlight2 = Table[(Bs[[2,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Highlight3 = Table[(Bs[[3,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Highlight4 = Table[(Bs[[4,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Nodes = {{0,0,0,0},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,1,0,0},{1,0,1,0},{1,0,0,1},{0,1,1,0},{0,1,0,1},{0,0,1,1},{1,1,1,0},{1,1,0,1},{1,0,1,1},{0,1,1,1},{1,1,1,1}};
Points = {{0, 0},{-1.5,1},{-.5,1},{.5,1},{1.5,1},{-2.5,2},{-1.5,2},{-.5,2},{.5,2},{1.5,2},{2.5,2},{-1.5,3},{-.5,3},{.5,3},{1.5,3},{0,4}};
Colored1 = Table[{Points[[Position[Nodes,Highlight1[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Colored2 = Table[{Points[[Position[Nodes,Highlight2[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Colored3 = Table[{Points[[Position[Nodes,Highlight3[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Colored4 = Table[{Points[[Position[Nodes,Highlight4[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Lines = {{Points[[1]],Points[[2]]},{Points[[1]],Points[[3]]},{Points[[1]],Points[[4]]},{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[2]],Points[[7]]},{Points[[2]],Points[[8]]},{Points[[3]],Points[[6]]},{Points[[3]],Points[[9]]},{Points[[3]],Points[[10]]},{Points[[4]],Points[[7]]},{Points[[4]],Points[[9]]},{Points[[4]],Points[[11]]},{Points[[5]],Points[[8]]},{Points[[5]],Points[[10]]},{Points[[5]],Points[[11]]},{Points[[6]],Points[[12]]},{Points[[6]],Points[[13]]},{Points[[7]],Points[[12]]},{Points[[7]],Points[[14]]},{Points[[9]],Points[[12]]},{Points[[9]],Points[[15]]},{Points[[8]],Points[[14]]},{Points[[8]],Points[[13]]},{Points[[10]],Points[[15]]},{Points[[10]],Points[[13]]},{Points[[11]],Points[[15]]},{Points[[11]],Points[[14]]},{Points[[12]],Points[[16]]},{Points[[15]],Points[[16]]},{Points[[14]],Points[[16]]},{Points[[13]],Points[[16]]}};
Show[ListLinePlot[Lines,PlotStyle->Black],
ListPlot[Points->Nodes,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],White,Disk[]}],.1}],
ListPlot[Colored1->Highlight1,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],Rainbow[[1]],Disk[]}],.1}],ListPlot[Colored2->Highlight2,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],Rainbow[[3]],Disk[]}],.1}],ListPlot[Colored3->Highlight3,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],Rainbow[[5]],Disk[]}],.1}],ListPlot[Colored4->Highlight4,LabelingFunction->Center,PlotMarkers->{Graphics[{EdgeForm[Directive[Thin,Black]],Rainbow[[7]],Disk[]}],.1}],Axes->False, ImageSize->{1000},PlotRangePadding->.5]
)


PlotTesseract[]:= (

Nodes = {{0,1,0,0},{0,1,0,1},{0,1,1,1},{0,1,1,0},{1,1,0,0},{1,1,0,1},{1,1,1,1},{1,1,1,0},{0,0,0,0},{0,0,0,1},{0,0,1,1},{0,0,1,0},{1,0,0,0},{1,0,0,1},{1,0,1,1},{1,0,1,0}};
NodesB = {"(4\!\(\*SubscriptBox[\()\), \(b\)]\)","(5\!\(\*SubscriptBox[\()\), \(b\)]\)","(7\!\(\*SubscriptBox[\()\), \(b\)]\)","(6\!\(\*SubscriptBox[\()\), \(b\)]\)","(12\!\(\*SubscriptBox[\()\), \(b\)]\)","(13\!\(\*SubscriptBox[\()\), \(b\)]\)","(15\!\(\*SubscriptBox[\()\), \(b\)]\)","(14\!\(\*SubscriptBox[\()\), \(b\)]\)","(0\!\(\*SubscriptBox[\()\), \(b\)]\)","(1\!\(\*SubscriptBox[\()\), \(b\)]\)","(3\!\(\*SubscriptBox[\()\), \(b\)]\)","(2\!\(\*SubscriptBox[\()\), \(b\)]\)","(8\!\(\*SubscriptBox[\()\), \(b\)]\)","(9\!\(\*SubscriptBox[\()\), \(b\)]\)","(11\!\(\*SubscriptBox[\()\), \(b\)]\)","(10\!\(\*SubscriptBox[\()\), \(b\)]\)"};
Points = {{1,1,-1},{-1,1,-1},{-1,-1,-1},{1,-1,-1},{1,1,1},{-1,1,1},{-1,-1,1},{1,-1,1},{3,3,-3},{-3,3,-3},{-3,-3,-3},{3,-3,-3},{3,3,3},{-3,3,3},{-3,-3,3},{3,-3,3}};
Lines = 
{{{Points[[1]],Points[[2]]},{Points[[3]],Points[[4]]},{Points[[5]],Points[[6]]},{Points[[7]],Points[[8]]},{Points[[9]],Points[[10]]},{Points[[11]],Points[[12]]},{Points[[13]],Points[[14]]},{Points[[15]],Points[[16]]}},{{Points[[2]],Points[[3]]},{Points[[4]],Points[[1]]},{Points[[6]],Points[[7]]},{Points[[8]],Points[[5]]},{Points[[10]],Points[[11]]},{Points[[12]],Points[[9]]},{Points[[13]],Points[[16]]},{Points[[14]],Points[[15]]}},{{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[3]],Points[[7]]},{Points[[4]],Points[[8]]},{Points[[9]],Points[[13]]},{Points[[10]],Points[[14]]},{Points[[11]],Points[[15]]},{Points[[16]],Points[[12]]}},
{{Points[[1]],Points[[9]]},{Points[[2]],Points[[10]]},{Points[[3]],Points[[11]]},{Points[[4]],Points[[12]]},{Points[[5]],Points[[13]]},{Points[[6]],Points[[14]]},{Points[[7]],Points[[15]]},{Points[[8]],Points[[16]]}}};
Ss = Graphics3D[Table[{Sphere[Points[[i]],.4],Text[NodesB[[i]],Points[[i]],Background-> White]},{i,16}],Lighting->"Neutral",Boxed->False];
Ts1 = Graphics3D[Table[{Gray, Tube[Lines[[1,i]],.05]},{i,8}]];
Ts2 = Graphics3D[Table[{Gray, Tube[Lines[[2,i]],.05]},{i,8}]];
Ts3 = Graphics3D[Table[{Gray, Tube[Lines[[3,i]],.05]},{i,8}]];
Ts4 = Graphics3D[Table[{Gray, Tube[Lines[[4,i]],.05]},{i,8}]];
Show[{Ss,Ts1,Ts2,Ts3,Ts4},Axes->False, ImageSize->{700},BaseStyle->{FontWeight->"Bold",FontSize->11}]
)
PlotTesseract[Bs_]:= (
Rainbow ={Lighter[Red],Lighter[Orange], Lighter[Yellow],Lighter[Green], Darker[Cyan],Lighter[Blue], Lighter[Magenta],Lighter[Purple]};

Highlightd = Table[(Bs[[nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[1]]},{jj,1},{ii,1,Dimensions[Bs][[2]]}];
Nodes = {{0,1,0,0},{0,1,0,1},{0,1,1,1},{0,1,1,0},{1,1,0,0},{1,1,0,1},{1,1,1,1},{1,1,1,0},{0,0,0,0},{0,0,0,1},{0,0,1,1},{0,0,1,0},{1,0,0,0},{1,0,0,1},{1,0,1,1},{1,0,1,0}};
NodesB = {"(4\!\(\*SubscriptBox[\()\), \(b\)]\)","(5\!\(\*SubscriptBox[\()\), \(b\)]\)","(7\!\(\*SubscriptBox[\()\), \(b\)]\)","(6\!\(\*SubscriptBox[\()\), \(b\)]\)","(12\!\(\*SubscriptBox[\()\), \(b\)]\)","(13\!\(\*SubscriptBox[\()\), \(b\)]\)","(15\!\(\*SubscriptBox[\()\), \(b\)]\)","(14\!\(\*SubscriptBox[\()\), \(b\)]\)","(0\!\(\*SubscriptBox[\()\), \(b\)]\)","(1\!\(\*SubscriptBox[\()\), \(b\)]\)","(3\!\(\*SubscriptBox[\()\), \(b\)]\)","(2\!\(\*SubscriptBox[\()\), \(b\)]\)","(8\!\(\*SubscriptBox[\()\), \(b\)]\)","(9\!\(\*SubscriptBox[\()\), \(b\)]\)","(11\!\(\*SubscriptBox[\()\), \(b\)]\)","(10\!\(\*SubscriptBox[\()\), \(b\)]\)"};
Points = {{1,1,-1},{-1,1,-1},{-1,-1,-1},{1,-1,-1},{1,1,1},{-1,1,1},{-1,-1,1},{1,-1,1},{3,3,-3},{-3,3,-3},{-3,-3,-3},{3,-3,-3},{3,3,3},{-3,3,3},{-3,-3,3},{3,-3,3}};
Colored = Table[{Points[[Position[Nodes,Highlightd[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[1]]}];
Lines = 
{{{Points[[1]],Points[[2]]},{Points[[3]],Points[[4]]},{Points[[5]],Points[[6]]},{Points[[7]],Points[[8]]},{Points[[9]],Points[[10]]},{Points[[11]],Points[[12]]},{Points[[13]],Points[[14]]},{Points[[15]],Points[[16]]}},{{Points[[2]],Points[[3]]},{Points[[4]],Points[[1]]},{Points[[6]],Points[[7]]},{Points[[8]],Points[[5]]},{Points[[10]],Points[[11]]},{Points[[12]],Points[[9]]},{Points[[13]],Points[[16]]},{Points[[14]],Points[[15]]}},{{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[3]],Points[[7]]},{Points[[4]],Points[[8]]},{Points[[9]],Points[[13]]},{Points[[10]],Points[[14]]},{Points[[11]],Points[[15]]},{Points[[16]],Points[[12]]}},
{{Points[[1]],Points[[9]]},{Points[[2]],Points[[10]]},{Points[[3]],Points[[11]]},{Points[[4]],Points[[12]]},{Points[[5]],Points[[13]]},{Points[[6]],Points[[14]]},{Points[[7]],Points[[15]]},{Points[[8]],Points[[16]]}}};
Ss = Graphics3D[Table[{Sphere[Points[[i]],.4],Text[NodesB[[i]],Points[[i]],Background-> White]},{i,16}],Lighting->"Neutral",Boxed->False];
Ts1 = Graphics3D[Table[{Gray, Tube[Lines[[1,i]],.05]},{i,8}]];
Ts2 = Graphics3D[Table[{Gray, Tube[Lines[[2,i]],.05]},{i,8}]];
Ts3 = Graphics3D[Table[{Gray, Tube[Lines[[3,i]],.05]},{i,8}]];
Ts4 = Graphics3D[Table[{Gray, Tube[Lines[[4,i]],.05]},{i,8}]];
Cs = Graphics3D[Table[{Rainbow[[4]],Sphere[Colored[[i]],.4]},{i,1,4}]];
Show[{Ss,Ts1,Ts2,Ts3,Ts4,Cs},Axes->False, ImageSize->{700},BaseStyle->{FontWeight->"Bold",FontSize->11}]
)
PlotTesseractColored[Bs_]:= (
Rainbow ={Lighter[Red],Lighter[Orange], Lighter[Yellow],Lighter[Green], Darker[Cyan],Lighter[Blue], Lighter[Magenta],Lighter[Purple]};

Highlightd = Table[(Bs[[nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[1]]},{jj,1},{ii,1,Dimensions[Bs][[2]]}];
Nodes = {{0,1,0,0},{0,1,0,1},{0,1,1,1},{0,1,1,0},{1,1,0,0},{1,1,0,1},{1,1,1,1},{1,1,1,0},{0,0,0,0},{0,0,0,1},{0,0,1,1},{0,0,1,0},{1,0,0,0},{1,0,0,1},{1,0,1,1},{1,0,1,0}};
NodesB = {"(4\!\(\*SubscriptBox[\()\), \(b\)]\)","(5\!\(\*SubscriptBox[\()\), \(b\)]\)","(7\!\(\*SubscriptBox[\()\), \(b\)]\)","(6\!\(\*SubscriptBox[\()\), \(b\)]\)","(12\!\(\*SubscriptBox[\()\), \(b\)]\)","(13\!\(\*SubscriptBox[\()\), \(b\)]\)","(15\!\(\*SubscriptBox[\()\), \(b\)]\)","(14\!\(\*SubscriptBox[\()\), \(b\)]\)","(0\!\(\*SubscriptBox[\()\), \(b\)]\)","(1\!\(\*SubscriptBox[\()\), \(b\)]\)","(3\!\(\*SubscriptBox[\()\), \(b\)]\)","(2\!\(\*SubscriptBox[\()\), \(b\)]\)","(8\!\(\*SubscriptBox[\()\), \(b\)]\)","(9\!\(\*SubscriptBox[\()\), \(b\)]\)","(11\!\(\*SubscriptBox[\()\), \(b\)]\)","(10\!\(\*SubscriptBox[\()\), \(b\)]\)"};
Points = {{1,1,-1},{-1,1,-1},{-1,-1,-1},{1,-1,-1},{1,1,1},{-1,1,1},{-1,-1,1},{1,-1,1},{3,3,-3},{-3,3,-3},{-3,-3,-3},{3,-3,-3},{3,3,3},{-3,3,3},{-3,-3,3},{3,-3,3}};
Colored = Table[{Points[[Position[Nodes,Highlightd[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[1]]}];
Lines = 
{{{Points[[1]],Points[[2]]},{Points[[3]],Points[[4]]},{Points[[5]],Points[[6]]},{Points[[7]],Points[[8]]},{Points[[9]],Points[[10]]},{Points[[11]],Points[[12]]},{Points[[13]],Points[[14]]},{Points[[15]],Points[[16]]}},{{Points[[2]],Points[[3]]},{Points[[4]],Points[[1]]},{Points[[6]],Points[[7]]},{Points[[8]],Points[[5]]},{Points[[10]],Points[[11]]},{Points[[12]],Points[[9]]},{Points[[13]],Points[[16]]},{Points[[14]],Points[[15]]}},{{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[3]],Points[[7]]},{Points[[4]],Points[[8]]},{Points[[9]],Points[[13]]},{Points[[10]],Points[[14]]},{Points[[11]],Points[[15]]},{Points[[16]],Points[[12]]}},
{{Points[[1]],Points[[9]]},{Points[[2]],Points[[10]]},{Points[[3]],Points[[11]]},{Points[[4]],Points[[12]]},{Points[[5]],Points[[13]]},{Points[[6]],Points[[14]]},{Points[[7]],Points[[15]]},{Points[[8]],Points[[16]]}}};
Ss = Graphics3D[Table[{Sphere[Points[[i]],.4],Text[NodesB[[i]],Points[[i]],Background-> White]},{i,16}],Lighting->"Neutral",Boxed->False];
Ts1 = Graphics3D[Table[{Rainbow[[1]], Tube[Lines[[1,i]],.05]},{i,8}]];
Ts2 = Graphics3D[Table[{Rainbow[[2]], Tube[Lines[[2,i]],.05]},{i,8}]];
Ts3 = Graphics3D[Table[{Rainbow[[4]], Tube[Lines[[3,i]],.05]},{i,8}]];
Ts4 = Graphics3D[Table[{Rainbow[[6]], Tube[Lines[[4,i]],.05]},{i,8}]];
Cs = Graphics3D[Table[{Darker[Gray],Sphere[Colored[[i]],.4]},{i,1,4}]];
Show[{Ss,Ts1,Ts2,Ts3,Ts4,Cs},Axes->False, ImageSize->{700},BaseStyle->{FontWeight->"Bold",FontSize->11}]
)
PlotTesseract4G[Bs_]:= (
Rainbow ={Lighter[Red],Lighter[Orange], Lighter[Yellow],Lighter[Green], Darker[Cyan],Lighter[Blue], Lighter[Magenta],Lighter[Purple]};
Highlight1 = Table[(Bs[[1,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Highlight2 = Table[(Bs[[2,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Highlight3 = Table[(Bs[[3,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Highlight4 = Table[(Bs[[4,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Nodes = {{0,1,0,0},{0,1,0,1},{0,1,1,1},{0,1,1,0},{1,1,0,0},{1,1,0,1},{1,1,1,1},{1,1,1,0},{0,0,0,0},{0,0,0,1},{0,0,1,1},{0,0,1,0},{1,0,0,0},{1,0,0,1},{1,0,1,1},{1,0,1,0}};
NodesB = {"(4\!\(\*SubscriptBox[\()\), \(b\)]\)","(5\!\(\*SubscriptBox[\()\), \(b\)]\)","(7\!\(\*SubscriptBox[\()\), \(b\)]\)","(6\!\(\*SubscriptBox[\()\), \(b\)]\)","(12\!\(\*SubscriptBox[\()\), \(b\)]\)","(13\!\(\*SubscriptBox[\()\), \(b\)]\)","(15\!\(\*SubscriptBox[\()\), \(b\)]\)","(14\!\(\*SubscriptBox[\()\), \(b\)]\)","(0\!\(\*SubscriptBox[\()\), \(b\)]\)","(1\!\(\*SubscriptBox[\()\), \(b\)]\)","(3\!\(\*SubscriptBox[\()\), \(b\)]\)","(2\!\(\*SubscriptBox[\()\), \(b\)]\)","(8\!\(\*SubscriptBox[\()\), \(b\)]\)","(9\!\(\*SubscriptBox[\()\), \(b\)]\)","(11\!\(\*SubscriptBox[\()\), \(b\)]\)","(10\!\(\*SubscriptBox[\()\), \(b\)]\)"};
Points = {{1,1,-1},{-1,1,-1},{-1,-1,-1},{1,-1,-1},{1,1,1},{-1,1,1},{-1,-1,1},{1,-1,1},{3,3,-3},{-3,3,-3},{-3,-3,-3},{3,-3,-3},{3,3,3},{-3,3,3},{-3,-3,3},{3,-3,3}};
Colored1 = Table[{Points[[Position[Nodes,Highlight1[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Colored2 = Table[{Points[[Position[Nodes,Highlight2[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Colored3 = Table[{Points[[Position[Nodes,Highlight3[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Colored4 = Table[{Points[[Position[Nodes,Highlight4[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Lines = 
{{{Points[[1]],Points[[2]]},{Points[[3]],Points[[4]]},{Points[[5]],Points[[6]]},{Points[[7]],Points[[8]]},{Points[[9]],Points[[10]]},{Points[[11]],Points[[12]]},{Points[[13]],Points[[14]]},{Points[[15]],Points[[16]]}},{{Points[[2]],Points[[3]]},{Points[[4]],Points[[1]]},{Points[[6]],Points[[7]]},{Points[[8]],Points[[5]]},{Points[[10]],Points[[11]]},{Points[[12]],Points[[9]]},{Points[[13]],Points[[16]]},{Points[[14]],Points[[15]]}},{{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[3]],Points[[7]]},{Points[[4]],Points[[8]]},{Points[[9]],Points[[13]]},{Points[[10]],Points[[14]]},{Points[[11]],Points[[15]]},{Points[[16]],Points[[12]]}},
{{Points[[1]],Points[[9]]},{Points[[2]],Points[[10]]},{Points[[3]],Points[[11]]},{Points[[4]],Points[[12]]},{Points[[5]],Points[[13]]},{Points[[6]],Points[[14]]},{Points[[7]],Points[[15]]},{Points[[8]],Points[[16]]}}};
Ss = Graphics3D[Table[{Sphere[Points[[i]],.4],Text[NodesB[[i]],Points[[i]],Background-> White]},{i,16}],Lighting->"Neutral",Boxed->False];
Ts1 = Graphics3D[Table[{Gray, Tube[Lines[[1,i]],.05]},{i,8}]];
Ts2 = Graphics3D[Table[{Gray, Tube[Lines[[2,i]],.05]},{i,8}]];
Ts3 = Graphics3D[Table[{Gray, Tube[Lines[[3,i]],.05]},{i,8}]];
Ts4 = Graphics3D[Table[{Gray, Tube[Lines[[4,i]],.05]},{i,8}]];
Cs1 = Graphics3D[Table[{Rainbow[[1]],Sphere[Colored1[[i]],.4]},{i,1,4}]];
Cs2 = Graphics3D[Table[{Rainbow[[3]],Sphere[Colored2[[i]],.4]},{i,1,4}]];
Cs3 = Graphics3D[Table[{Rainbow[[5]],Sphere[Colored3[[i]],.4]},{i,1,4}]];
Cs4 = Graphics3D[Table[{Rainbow[[7]],Sphere[Colored4[[i]],.4]},{i,1,4}]];
Show[{Ss,Ts1,Ts2,Ts3,Ts4,Cs1, Cs2, Cs3, Cs4},Axes->False, ImageSize->{700},BaseStyle->{FontWeight->"Bold",FontSize->11}]
)
PlotTesseract2G[Bs_]:= (
Rainbow ={Lighter[Red],Lighter[Orange], Lighter[Yellow],Lighter[Green], Darker[Cyan],Blue, Lighter[Magenta],Lighter[Purple]};
Highlight1 = Table[(Bs[[1,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Highlight2 = Table[(Bs[[2,nn,ii,ii]]*(-1)+1)/2,{nn,1,Dimensions[Bs][[2]]},{jj,1},{ii,1,Dimensions[Bs][[3]]}];
Nodes = {{0,1,0,0},{0,1,0,1},{0,1,1,1},{0,1,1,0},{1,1,0,0},{1,1,0,1},{1,1,1,1},{1,1,1,0},{0,0,0,0},{0,0,0,1},{0,0,1,1},{0,0,1,0},{1,0,0,0},{1,0,0,1},{1,0,1,1},{1,0,1,0}};
NodesB = {"(4\!\(\*SubscriptBox[\()\), \(b\)]\)","(5\!\(\*SubscriptBox[\()\), \(b\)]\)","(7\!\(\*SubscriptBox[\()\), \(b\)]\)","(6\!\(\*SubscriptBox[\()\), \(b\)]\)","(12\!\(\*SubscriptBox[\()\), \(b\)]\)","(13\!\(\*SubscriptBox[\()\), \(b\)]\)","(15\!\(\*SubscriptBox[\()\), \(b\)]\)","(14\!\(\*SubscriptBox[\()\), \(b\)]\)","(0\!\(\*SubscriptBox[\()\), \(b\)]\)","(1\!\(\*SubscriptBox[\()\), \(b\)]\)","(3\!\(\*SubscriptBox[\()\), \(b\)]\)","(2\!\(\*SubscriptBox[\()\), \(b\)]\)","(8\!\(\*SubscriptBox[\()\), \(b\)]\)","(9\!\(\*SubscriptBox[\()\), \(b\)]\)","(11\!\(\*SubscriptBox[\()\), \(b\)]\)","(10\!\(\*SubscriptBox[\()\), \(b\)]\)"};
Points = {{1,1,-1},{-1,1,-1},{-1,-1,-1},{1,-1,-1},{1,1,1},{-1,1,1},{-1,-1,1},{1,-1,1},{3,3,-3},{-3,3,-3},{-3,-3,-3},{3,-3,-3},{3,3,3},{-3,3,3},{-3,-3,3},{3,-3,3}};
Colored1 = Table[{Points[[Position[Nodes,Highlight1[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Colored2 = Table[{Points[[Position[Nodes,Highlight2[[nn,1]]][[1,1]]]]},{nn,1,Dimensions[Bs][[2]]}];
Lines = 
{{{Points[[1]],Points[[2]]},{Points[[3]],Points[[4]]},{Points[[5]],Points[[6]]},{Points[[7]],Points[[8]]},{Points[[9]],Points[[10]]},{Points[[11]],Points[[12]]},{Points[[13]],Points[[14]]},{Points[[15]],Points[[16]]}},{{Points[[2]],Points[[3]]},{Points[[4]],Points[[1]]},{Points[[6]],Points[[7]]},{Points[[8]],Points[[5]]},{Points[[10]],Points[[11]]},{Points[[12]],Points[[9]]},{Points[[13]],Points[[16]]},{Points[[14]],Points[[15]]}},{{Points[[1]],Points[[5]]},{Points[[2]],Points[[6]]},{Points[[3]],Points[[7]]},{Points[[4]],Points[[8]]},{Points[[9]],Points[[13]]},{Points[[10]],Points[[14]]},{Points[[11]],Points[[15]]},{Points[[16]],Points[[12]]}},
{{Points[[1]],Points[[9]]},{Points[[2]],Points[[10]]},{Points[[3]],Points[[11]]},{Points[[4]],Points[[12]]},{Points[[5]],Points[[13]]},{Points[[6]],Points[[14]]},{Points[[7]],Points[[15]]},{Points[[8]],Points[[16]]}}};
Ss = Graphics3D[Table[{Sphere[Points[[i]],.4],Text[NodesB[[i]],Points[[i]],Background-> White]},{i,16}],Lighting->"Neutral",Boxed->False];
Ts1 = Graphics3D[Table[{Gray, Tube[Lines[[1,i]],.05]},{i,8}]];
Ts2 = Graphics3D[Table[{Gray, Tube[Lines[[2,i]],.05]},{i,8}]];
Ts3 = Graphics3D[Table[{Gray, Tube[Lines[[3,i]],.05]},{i,8}]];
Ts4 = Graphics3D[Table[{Gray, Tube[Lines[[4,i]],.05]},{i,8}]];
Cs1 = Graphics3D[Table[{Rainbow[[1]],Sphere[Colored1[[i]],.4]},{i,1,4}]];
Cs2 = Graphics3D[Table[{Rainbow[[5]],Sphere[Colored2[[i]],.4]},{i,1,4}]];
Show[{Ss,Ts1,Ts2,Ts3,Ts4,Cs1, Cs2},Axes->False, ImageSize->{700},BaseStyle->{FontWeight->"Bold",FontSize->13}]
)


(*Defining Transformation Rules and Deriving L Matrices for 16-Color Theories *)


(*4D N=4 Super-Maxwell On Shell*)


DaSM4D[aa_][Arg1_+Arg2_]:= DaSM4D[aa][Arg1] + DaSM4D[aa][Arg2]
DaSM4D[aa_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM4D[aa][Arg2]
DaSM4D[aa_][\!\(\*SuperscriptBox[\(Field_\), 
TagBox[
RowBox[{"(", 
RowBox[{"nt_", ",", "nx_", ",", "ny_", ",", "nz_"}], ")"}],
Derivative],
MultilineFunction->None]\)[t,x,y,z]]:=D[DaSM4D[aa][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM4D[aa_,II_][Arg1_+Arg2_]:= DaSM4D[aa,II][Arg1] + DaSM4D[aa,II][Arg2]
DaSM4D[aa_,II_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM4D[aa,II][Arg2]
DaSM4D[aa_,II_][D[Field_[t,x,y,z],{t,nt_},{x,nx_},{y,ny_},{z,nz_}]]:=D[DaSM4D[aa,II][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM4D[aa_,0]:= DaSM4D[aa];


DaSM4D0[aa_][Arg1_+Arg2_]:= DaSM4D0[aa][Arg1] + DaSM4D0[aa][Arg2]
DaSM4D0[aa_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM4D0[aa][Arg2]
DaSM4D0[aa_][\!\(\*SuperscriptBox[\(Field_\), 
TagBox[
RowBox[{"(", 
RowBox[{"nt_", ",", "nx_", ",", "ny_", ",", "nz_"}], ")"}],
Derivative],
MultilineFunction->None]\)[t,x,y,z]]:=D[DaSM4D0[aa][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM4D0[aa_,II_][Arg1_+Arg2_]:= DaSM4D0[aa,II][Arg1] + DaSM4D0[aa,II][Arg2]
DaSM4D0[aa_,II_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM4D0[aa,II][Arg2]
DaSM4D0[aa_,II_][D[Field_[t,x,y,z],{t,nt_},{x,nx_},{y,ny_},{z,nz_}]]:=D[DaSM4D0[aa,II][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM4D0[aa_,0]:= DaSM4D0[aa];
Cab = {{0, -1, 0, 0},{1, 0, 0, 0},{0, 0, 0, 1}, {0, 0, -1, 0}};
Fmn[mu_,nu_][t,x,y,z]:=D[Am[nu][t,x,y,z],\[CapitalStigma][mu]]-D[Am[mu][t,x,y,z],\[CapitalStigma][nu]];


(*4D N=4 Super-Maxwell On Shell Transformation Rules*)


DaSM4D[aa_][Am[mu_][t, x, y, z]] := Sum[\[Gamma]stdown[mu][[aa,bb]]*\[Lambda][bb][t, x, y, z], {bb, 1, 4}]; 
DaSM4D[aa_][\[Lambda][bb_][t, x, y, z]] := (-I/4)*Sum[CommuteGammadown[mu, nu][[aa,bb]]*Fmn[mu, nu][t, x, y, z], {mu, 0, 3}, 
     {nu, 0, 3}]; 
DaSM4D[aa_, II_][Am[mu_][t, x, y, z]] := -Sum[\[Gamma]stdown[mu][[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], {bb, 1, 4}]; 
DaSM4D[aa_, II_][\[Lambda][bb_][t, x, y, z]] := I*Sum[\[Gamma]down[mu][[aa,bb]]*D[A[II][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}] - 
    Sum[\[Gamma]5\[Gamma]down[mu][[aa,bb]]*D[B[II][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}]; 
DaSM4D[aa_][A[II_][t, x, y, z]] := \[CapitalPsi][aa][II][t, x, y, z]; 
DaSM4D[aa_][B[II_][t, x, y, z]] := I*Sum[\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], {bb, 1, 4}]; 
DaSM4D[aa_][\[CapitalPsi][bb_][II_][t, x, y, z]] := I*Sum[\[Gamma]down[\[Mu]][[aa,bb]]*D[A[II][t, x, y, z], \[CapitalStigma][\[Mu]]], {\[Mu], 0, 3}] - 
    Sum[\[Gamma]5\[Gamma]down[\[Mu]][[aa,bb]]*D[B[II][t, x, y, z], \[CapitalStigma][\[Mu]]], {\[Mu], 0, 3}]; 
DaSM4D[aa_, II_][A[JJ_][t, x, y, z]] := IdentityMatrix[3][[II,JJ]]*\[Lambda][aa][t, x, y, z] - 
    Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][aa][KK][t, x, y, z], {KK, 1, 3}]; 
DaSM4D[aa_, II_][B[JJ_][t, x, y, z]] := 
   I*Sum[\[Gamma]5[[aa,bb]]*(IdentityMatrix[3][[II,JJ]]*\[Lambda][bb][t, x, y, z] + 
       Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}]), {bb, 1, 4}]; 
DaSM4D[aa_, II_][\[CapitalPsi][bb_][JJ_][t, x, y, z]] := IdentityMatrix[3][[II,JJ]]*(I/4)*
     Sum[CommuteGammadown[mu, nu][[aa,bb]]*Fmn[mu, nu][t, x, y, z], {mu, 0, 3}, {nu, 0, 3}] + 
    Sum[LeviCivitaTensor[3][[II,JJ,KK]]*(I*Sum[\[Gamma]down[mu][[aa,bb]]*D[A[KK][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}] + 
       Sum[\[Gamma]5\[Gamma]down[mu][[aa,bb]]*D[B[KK][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}]), {KK, 1, 3}]; 


(*4D N=4 Super-Maxwell On Shell Zero Brane Boson Rules*)


DaSM4D0[aa_][Am[mu_][t, x, y, z]] := Sum[\[Gamma]stdown[mu][[aa,bb]]*\[Lambda][bb][t, x, y, z], {bb, 1, 4}]; 
DaSM4D0[aa_, II_][Am[mu_][t, x, y, z]] := -Sum[\[Gamma]stdown[mu][[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], {bb, 1, 4}]; 
DaSM4D0[aa_][A[II_][t, x, y, z]] := \[CapitalPsi][aa][II][t, x, y, z]; 
DaSM4D0[aa_][B[II_][t, x, y, z]] := I*Sum[\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], {bb, 1, 4}]; 
DaSM4D0[aa_, II_][A[JJ_][t, x, y, z]] := IdentityMatrix[3][[II,JJ]]*\[Lambda][aa][t, x, y, z] - 
    Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][aa][KK][t, x, y, z], {KK, 1, 3}]; 
DaSM4D0[aa_, II_][B[JJ_][t, x, y, z]] := 
   I*Sum[\[Gamma]5[[aa,bb]]*(IdentityMatrix[3][[II,JJ]]*\[Lambda][bb][t, x, y, z] + 
       Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}]), {bb, 1, 4}]; 


SM4Bosons = {A[1][t, x, y, z], B[1][t, x, y, z], A[2][t, x, y, z], B[2][t, x, y, z], A[3][t, x, y, z], B[3][t, x, y, z], 
    Am[1][t, x, y, z], Am[2][t, x, y, z], Am[3][t, x, y, z]}; 
SM4Fermions = {\[CapitalPsi][1][1][t, x, y, z], \[CapitalPsi][2][1][t, x, y, z], \[CapitalPsi][3][1][t, x, y, z], \[CapitalPsi][4][1][t, x, y, z], 
    \[CapitalPsi][1][2][t, x, y, z], \[CapitalPsi][2][2][t, x, y, z], \[CapitalPsi][3][2][t, x, y, z], \[CapitalPsi][4][2][t, x, y, z], \[CapitalPsi][1][3][t, x, y, z], 
    \[CapitalPsi][2][3][t, x, y, z], \[CapitalPsi][3][3][t, x, y, z], \[CapitalPsi][4][3][t, x, y, z], \[Lambda][1][t, x, y, z], \[Lambda][2][t, x, y, z], 
    \[Lambda][3][t, x, y, z], \[Lambda][4][t, x, y, z]}; 


(*Adinkra Matrices*)


LRMatricesSM4D[] := (LsSM4 = Table[0, {i, 1, 16}, {j, 1, 9}, {k, 1, 16}]; 
   RsSM4 = Table[0, {i, 1, 16}, {j, 1, 16}, {k, 1, 9}]; For[II = 0, II < 4, II++, 
    For[i = 1, i < 5, i++, For[j = 1, j < 10, j++, If[ContainsAny[SM4Fermions, {DaSM4D0[i, II][SM4Bosons[[j]]]}], 
        k = Position[SM4Fermions, DaSM4D0[i, II][SM4Bosons[[j]]]][[1,1]]; LsSM4[[4*II + i,j,k]] = 
          DaSM4D0[i, II][SM4Bosons[[j]]]/SM4Fermions[[k]], If[ContainsAny[SM4Fermions, {-DaSM4D0[i, II][SM4Bosons[[j]]]}], 
         k = Position[SM4Fermions, -DaSM4D0[i, II][SM4Bosons[[j]]]][[1,1]]; LsSM4[[4*II + i,j,k]] = 
           DaSM4D0[i, II][SM4Bosons[[j]]]/SM4Fermions[[k]]]]]; RsSM4[[4*II + i]] = Transpose[LsSM4[[4*II + i]]]]]; 
   Return[{LsSM4, RsSM4}])


(*10D N=1 Super-Maxwell On Shell*)


\[Sigma]0down = kp[I2, I2, I2, I2]; 
\[Sigma]1down = kp[\[Sigma]2, \[Sigma]2, \[Sigma]2, \[Sigma]2]; 
\[Sigma]2down = kp[\[Sigma]2, \[Sigma]2, I2, \[Sigma]1]; 
\[Sigma]3down = kp[\[Sigma]2, \[Sigma]2, I2, \[Sigma]3]; 
\[Sigma]4down = kp[\[Sigma]2, \[Sigma]1, \[Sigma]2, I2]; 
\[Sigma]5down = kp[\[Sigma]2, \[Sigma]3, \[Sigma]2, I2]; 
\[Sigma]6down = kp[\[Sigma]2, I2, \[Sigma]1, \[Sigma]2]; 
\[Sigma]7down = kp[\[Sigma]2, I2, \[Sigma]3, \[Sigma]2]; 
\[Sigma]8down = kp[\[Sigma]1, I2, I2, I2]; 
\[Sigma]9down = kp[\[Sigma]3, I2, I2, I2]; 
\[Sigma]0up = -kp[I2, I2, I2, I2]; 
\[Sigma]1up = kp[\[Sigma]2, \[Sigma]2, \[Sigma]2, \[Sigma]2]; 
\[Sigma]2up = kp[\[Sigma]2, \[Sigma]2, I2, \[Sigma]1]; 
\[Sigma]3up = kp[\[Sigma]2, \[Sigma]2, I2, \[Sigma]3]; 
\[Sigma]4up = kp[\[Sigma]2, \[Sigma]1, \[Sigma]2, I2]; 
\[Sigma]5up = kp[\[Sigma]2, \[Sigma]3, \[Sigma]2, I2]; 
\[Sigma]6up = kp[\[Sigma]2, I2, \[Sigma]1, \[Sigma]2]; 
\[Sigma]7up = kp[\[Sigma]2, I2, \[Sigma]3, \[Sigma]2]; 
\[Sigma]8up = kp[\[Sigma]1, I2, I2, I2]; 
\[Sigma]9up = kp[\[Sigma]3, I2, I2, I2]; 
\[Sigma]10down = Array[ToExpression[StringJoin["\[Sigma]", ToString[#1 - 1], "down"]] & , 10]; 
\[Sigma]10up = Array[ToExpression[StringJoin["\[Sigma]", ToString[#1 - 1], "up"]] & , 10]; 
\[Sigma]2index = Table[(1/2)*(\[Sigma]10down[[a]] . \[Sigma]10up[[b]] - \[Sigma]10down[[b]] . \[Sigma]10up[[a]]), {a, 1, 10}, {b, 1, 10}]; 
DaSM10[aa_][(Arg1_) + (Arg2_)] := DaSM10[aa][Arg1] + DaSM10[aa][Arg2]
DaSM10[aa_][(Arg1_)*(Arg2_)] /; NumberQ[Arg1] := Arg1*DaSM10[aa][Arg2]
DaSM10[aa_][Derivative[nt_, nx_, ny_, nz_][Field_][t, x, y, z]] := D[DaSM10[aa][Field[t, x, y, z]], {t, nt}, {x, nx}, 
   {y, ny}, {z, nz}]


(*Zero Brane Transformation Rules (This is the only theory where we have left out the general form of the transformation rules)*)


DaSM10[aa_][A[\[Mu]_][t, x, y, z]] := \[Sigma]10down[[\[Mu],aa]] . \[Lambda]b; 
DaSM10[aa_][\[Lambda][bb_][t, x, y, z]] := Sum[I*\[Sigma]2index[[1,\[Nu],aa,bb]]*A[\[Nu]], {\[Nu], 1, 9}]
A\[Mu] = {A[2][t, x, y, z], A[3][t, x, y, z], A[4][t, x, y, z], A[5][t, x, y, z], A[6][t, x, y, z], A[7][t, x, y, z], 
    A[8][t, x, y, z], A[9][t, x, y, z], A[1][t, x, y, z]}; 
\[Lambda]b = Array[\[Lambda][#1][t, x, y, z] & , 16]; 


(*L Matrices*)


LRMatricesSM10D[] := (LsSM10 = Table[0, {i, 1, 16}, {j, 1, 9}, {k, 1, 16}]; 
   RsSM10 = Table[0, {i, 1, 16}, {j, 1, 16}, {k, 1, 9}]; For[i = 1, i < 17, i++, 
    For[j = 1, j < 10, j++, If[ContainsAny[\[Lambda]b, {DaSM10[i][A\[Mu][[j]]]}], k = Position[\[Lambda]b, DaSM10[i][A\[Mu][[j]]]][[1,1]]; 
        LsSM10[[i,j,k]] = DaSM10[i][A\[Mu][[j]]]/\[Lambda]b[[k]]]; If[ContainsAny[\[Lambda]b, {-DaSM10[i][A\[Mu][[j]]]}], 
       k = Position[\[Lambda]b, -DaSM10[i][A\[Mu][[j]]]][[1,1]]; LsSM10[[i,j,k]] = DaSM10[i][A\[Mu][[j]]]/\[Lambda]b[[k]]]; 
      RsSM10[[i]] = Transpose[LsSM10[[i]]]]]; Return[{LsSM10, RsSM10}])


(*4D N=4 Super-Maxwell with N=1 Off-Shell Closure*)


DaSM[aa_][Arg1_+Arg2_]:= DaSM[aa][Arg1] + DaSM[aa][Arg2]
DaSM[aa_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM[aa][Arg2]
DaSM[aa_][\!\(\*SuperscriptBox[\(Field_\), 
TagBox[
RowBox[{"(", 
RowBox[{"nt_", ",", "nx_", ",", "ny_", ",", "nz_"}], ")"}],
Derivative],
MultilineFunction->None]\)[t,x,y,z]]:=D[DaSM[aa][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM[aa_,II_][Arg1_+Arg2_]:= DaSM[aa,II][Arg1] + DaSM[aa,II][Arg2]
DaSM[aa_,II_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM[aa,II][Arg2]
DaSM[aa_,II_][D[Field_[t,x,y,z],{t,nt_},{x,nx_},{y,ny_},{z,nz_}]]:=D[DaSM[aa,II][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM[aa_,0]:= DaSM[aa];


DaSM0[aa_][Arg1_+Arg2_]:= DaSM0[aa][Arg1] + DaSM0[aa][Arg2]
DaSM0[aa_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM0[aa][Arg2]
DaSM0[aa_][\!\(\*SuperscriptBox[\(Field_\), 
TagBox[
RowBox[{"(", 
RowBox[{"nt_", ",", "nx_", ",", "ny_", ",", "nz_"}], ")"}],
Derivative],
MultilineFunction->None]\)[t,x,y,z]]:=D[DaSM0[aa][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM0[aa_,II_][Arg1_+Arg2_]:= DaSM0[aa,II][Arg1] + DaSM0[aa,II][Arg2]
DaSM0[aa_,II_][Arg1_ Arg2_]/;NumberQ[Arg1]:=Arg1 DaSM0[aa,II][Arg2]
DaSM0[aa_,II_][D[Field_[t,x,y,z],{t,nt_},{x,nx_},{y,ny_},{z,nz_}]]:=D[DaSM0[aa,II][Field[t,x,y,z]],{t,nt},{x,nx},{y,ny},{z,nz}]
DaSM0[aa_,0]:= DaSM0[aa];


Cab = {{0, -1, 0, 0},{1, 0, 0, 0},{0, 0, 0, 1}, {0, 0, -1, 0}};


(* 4D N=4 Super-Maxwell with N=1 Off-Shell Closure: Transformation Rules *)


DaSM[aa_][Am[mu_][t, x, y, z]] := Sum[\[Gamma]stdown[mu][[aa,bb]]*\[Lambda][bb][t, x, y, z], {bb, 1, 4}]; 
DaSM[aa_][\[Lambda][bb_][t, x, y, z]] := 
   (-I/4)*Sum[CommuteGammadown[mu, nu][[aa,bb]]*Fmn[mu, nu][t, x, y, z], {mu, 0, 3}, 
      {nu, 0, 3}] + \[Gamma]5down[[aa,bb]]*d[t, x, y, z]; 
DaSM[aa_][d[t, x, y, z]] := I*Sum[\[Gamma]5\[Gamma][mu][[aa,bb]]*D[\[Lambda][bb][t, x, y, z], \[CapitalStigma][mu]], 
     {bb, 1, 4}, {mu, 0, 3}]; 
DaSM[aa_, II_][Am[mu_][t, x, y, z]] := -Sum[\[Gamma]stdown[mu][[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], 
     {bb, 1, 4}]; 
DaSM[aa_, II_][\[Lambda][bb_][t, x, y, z]] := 
   I*Sum[\[Gamma]down[mu][[aa,bb]]*D[A[II][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}] - 
    Sum[\[Gamma]5\[Gamma]down[mu][[aa,bb]]*D[B[II][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}] - 
    I*Cab[[aa,bb]]*F[II][t, x, y, z] - \[Gamma]5down[[aa,bb]]*G[II][t, x, y, z]; 
DaSM[aa_, II_][d[t, x, y, z]] := I*Sum[\[Gamma]5\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][II][t, x, y, z], \[CapitalStigma][mu]], 
     {bb, 1, 4}, {mu, 0, 3}]; 
DaSM[aa_][A[II_][t, x, y, z]] := \[CapitalPsi][aa][II][t, x, y, z]; 
DaSM[aa_][B[II_][t, x, y, z]] := I*Sum[\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], {bb, 1, 4}]; 
DaSM[aa_][\[CapitalPsi][bb_][II_][t, x, y, z]] := 
   I*Sum[\[Gamma]down[\[Mu]][[aa,bb]]*D[A[II][t, x, y, z], \[CapitalStigma][\[Mu]]], {\[Mu], 0, 3}] - 
    Sum[\[Gamma]5\[Gamma]down[\[Mu]][[aa,bb]]*D[B[II][t, x, y, z], \[CapitalStigma][\[Mu]]], {\[Mu], 0, 3}]; 
DaSM[aa_][F[II_][t, x, y, z]] := Sum[\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][II][t, x, y, z], \[CapitalStigma][mu]], 
    {bb, 1, 4}, {mu, 0, 3}]; 
DaSM[aa_][G[II_][t, x, y, z]] := Sum[I*\[Gamma]5\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][II][t, x, y, z], \[CapitalStigma][mu]], 
    {bb, 1, 4}, {mu, 0, 3}]; 
DaSM[aa_, II_][A[JJ_][t, x, y, z]] := IdentityMatrix[3][[II,JJ]]*\[Lambda][aa][t, x, y, z] - 
    Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][aa][KK][t, x, y, z], {KK, 1, 3}]; 
DaSM[aa_, II_][B[JJ_][t, x, y, z]] := 
   I*Sum[\[Gamma]5[[aa,bb]]*(IdentityMatrix[3][[II,JJ]]*\[Lambda][bb][t, x, y, z] + 
       Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}]), 
     {bb, 1, 4}]; 
DaSM[aa_, II_][\[CapitalPsi][bb_][JJ_][t, x, y, z]] := 
   IdentityMatrix[3][[II,JJ]]*(I/4)*Sum[CommuteGammadown[mu, nu][[aa,bb]]*
       Fmn[mu, nu][t, x, y, z], {mu, 0, 3}, {nu, 0, 3}] + 
    Sum[LeviCivitaTensor[3][[II,JJ,KK]]*
      (I*Sum[\[Gamma]down[mu][[aa,bb]]*D[A[KK][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}] + 
       Sum[\[Gamma]5\[Gamma]down[mu][[aa,bb]]*D[B[KK][t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}]), {KK, 1, 3}]; 
DaSM[aa_, II_][F[JJ_][t, x, y, z]] := 
   Sum[\[Gamma][mu][[aa,bb]]*D[IdentityMatrix[3][[II,JJ]]*\[Lambda][bb][t, x, y, z] - 
       Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}], \[CapitalStigma][mu]], 
    {bb, 1, 4}, {mu, 0, 3}]; 
DaSM[aa_, II_][G[JJ_][t, x, y, z]] := 
   I*Sum[\[Gamma]5\[Gamma][mu][[aa,bb]]*D[(-IdentityMatrix[3][[II,JJ]])*\[Lambda][bb][t, x, y, z] + 
        Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}], \[CapitalStigma][mu]], 
     {bb, 1, 4}, {mu, 0, 3}]; 


(* 4D N=4 Super-Maxwell with N=1 Off-Shell Closure: Zero-Brane Boson rules *)


DaSM0[aa_][Am[mu_][t, x, y, z]] := Sum[\[Gamma]stdown[mu][[aa,bb]]*\[Lambda][bb][t, x, y, z], 
    {bb, 1, 4}]; 
DaSM0[aa_][d[t, x, y, z]] := I*Sum[\[Gamma]5\[Gamma][0][[aa,bb]]*\[Lambda][bb][t, x, y, z], {bb, 1, 4}]; 
DaSM0[aa_, II_][Am[mu_][t, x, y, z]] := -Sum[\[Gamma]stdown[mu][[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], 
     {bb, 1, 4}]; 
DaSM0[aa_, II_][d[t, x, y, z]] := I*Sum[\[Gamma]5\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], 
     {bb, 1, 4}]; 
DaSM0[aa_][A[II_][t, x, y, z]] := \[CapitalPsi][aa][II][t, x, y, z]; 
DaSM0[aa_][B[II_][t, x, y, z]] := I*Sum[\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], {bb, 1, 4}]; 
DaSM0[aa_][F[II_][t, x, y, z]] := Sum[\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], {bb, 1, 4}]; 
DaSM0[aa_][G[II_][t, x, y, z]] := Sum[I*\[Gamma]5\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][II][t, x, y, z], 
    {bb, 1, 4}]; 
DaSM0[aa_, II_][A[JJ_][t, x, y, z]] := IdentityMatrix[3][[II,JJ]]*\[Lambda][aa][t, x, y, z] - 
    Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][aa][KK][t, x, y, z], {KK, 1, 3}]; 
DaSM0[aa_, II_][B[JJ_][t, x, y, z]] := 
   I*Sum[\[Gamma]5[[aa,bb]]*(IdentityMatrix[3][[II,JJ]]*\[Lambda][bb][t, x, y, z] + 
       Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}]), 
     {bb, 1, 4}]; 
DaSM0[aa_, II_][F[JJ_][t, x, y, z]] := 
   Sum[\[Gamma][0][[aa,bb]]*(IdentityMatrix[3][[II,JJ]]*\[Lambda][bb][t, x, y, z] - 
      Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}]), {bb, 1, 4}]; 
DaSM0[aa_, II_][G[JJ_][t, x, y, z]] := 
   I*Sum[\[Gamma]5\[Gamma][0][[aa,bb]]*((-IdentityMatrix[3][[II,JJ]])*\[Lambda][bb][t, x, y, z] + 
       Sum[LeviCivitaTensor[3][[II,JJ,KK]]*\[CapitalPsi][bb][KK][t, x, y, z], {KK, 1, 3}]), 
     {bb, 1, 4}]; 


BosonsSM = {A[1][t, x, y, z], B[1][t, x, y, z], F[1][t, x, y, z], G[1][t, x, y, z], 
    A[2][t, x, y, z], B[2][t, x, y, z], F[2][t, x, y, z], G[2][t, x, y, z], 
    A[3][t, x, y, z], B[3][t, x, y, z], F[3][t, x, y, z], G[3][t, x, y, z], 
    Am[1][t, x, y, z], Am[2][t, x, y, z], Am[3][t, x, y, z], d[t, x, y, z]}; 
FermionsSM = {\[CapitalPsi][1][1][t, x, y, z], \[CapitalPsi][2][1][t, x, y, z], \[CapitalPsi][3][1][t, x, y, z], 
    \[CapitalPsi][4][1][t, x, y, z], \[CapitalPsi][1][2][t, x, y, z], \[CapitalPsi][2][2][t, x, y, z], \[CapitalPsi][3][2][t, x, y, z], 
    \[CapitalPsi][4][2][t, x, y, z], \[CapitalPsi][1][3][t, x, y, z], \[CapitalPsi][2][3][t, x, y, z], \[CapitalPsi][3][3][t, x, y, z], 
    \[CapitalPsi][4][3][t, x, y, z], \[Lambda][1][t, x, y, z], \[Lambda][2][t, x, y, z], \[Lambda][3][t, x, y, z], 
    \[Lambda][4][t, x, y, z]}; 


(* Solving For Adinkra Matrices *)


LRMatricesSM[] := (LsSM = Table[0, {i, 1, 16}, {j, 1, 16}, {k, 1, 16}]; 
   RsSM = Table[0, {i, 1, 16}, {j, 1, 16}, {k, 1, 16}]; For[II = 0, II < 4, II++, 
    For[i = 1, i < 5, i++, For[j = 1, j < 17, j++, 
       If[ContainsAny[FermionsSM, {DaSM0[i, II][BosonsSM[[j]]]}], 
        k = Position[FermionsSM, DaSM0[i, II][BosonsSM[[j]]]][[1,1]]; 
         LsSM[[4*II + i,j,k]] = DaSM0[i, II][BosonsSM[[j]]]/FermionsSM[[k]], 
        If[ContainsAny[FermionsSM, {-DaSM0[i, II][BosonsSM[[j]]]}], 
         k = Position[FermionsSM, -DaSM0[i, II][BosonsSM[[j]]]][[1,1]]; 
          LsSM[[4*II + i,j,k]] = DaSM0[i, II][BosonsSM[[j]]]/FermionsSM[[k]]]]]; 
      RsSM[[4*II + i]] = Transpose[LsSM[[4*II + i]]]]]; Return[{LsSM, RsSM}])


(* 4D N=4 Vector Tensor with N=2 Off-Shell Closure *)


DaVT[aa_][(Arg1_) + (Arg2_)] := DaVT[aa][Arg1] + DaVT[aa][Arg2]
DaVT[aa_][(Arg1_)*(Arg2_)] /; NumberQ[Arg1] := Arg1*DaVT[aa][Arg2]
DaVT[aa_][Derivative[nt_, nx_, ny_, nz_][Field_][t, x, y, z]] := 
  D[DaVT[aa][Field[t, x, y, z]], {t, nt}, {x, nx}, {y, ny}, 
   {z, nz}]
DaVT[aa_, II_][(Arg1_) + (Arg2_)] := DaVT[aa, II][Arg1] + 
   DaVT[aa, II][Arg2]
DaVT[aa_, II_][(Arg1_)*(Arg2_)] /; NumberQ[Arg1] := 
  Arg1*DaVT[aa, II][Arg2]
DaVT[aa_, II_][D[(Field_)[t, x, y, z], {t, nt_}, {x, nx_}, 
    {y, ny_}, {z, nz_}]] := D[DaVT[aa, II][Field[t, x, y, z]], 
   {t, nt}, {x, nx}, {y, ny}, {z, nz}]
DaVT[aa_, 0] := DaVT[aa]; 


DaVT0[aa_][(Arg1_) + (Arg2_)] := DaVT0[aa][Arg1] + DaVT0[aa][Arg2]
DaVT0[aa_][(Arg1_)*(Arg2_)] /; NumberQ[Arg1] := 
  Arg1*DaVT0[aa][Arg2]
DaVT0[aa_][Derivative[nt_, nx_, ny_, nz_][Field_][t, x, y, z]] := 
  D[DaVT0[aa][Field[t, x, y, z]], {t, nt}, {x, nx}, {y, ny}, 
   {z, nz}]
DaVT0[aa_, II_][(Arg1_) + (Arg2_)] := DaVT0[aa, II][Arg1] + 
   DaVT0[aa, II][Arg2]
DaVT0[aa_, II_][(Arg1_)*(Arg2_)] /; NumberQ[Arg1] := 
  Arg1*DaVT0[aa, II][Arg2]
DaVT0[aa_, II_][D[(Field_)[t, x, y, z], {t, nt_}, {x, nx_}, 
    {y, ny_}, {z, nz_}]] := D[DaVT0[aa, II][Field[t, x, y, z]], 
   {t, nt}, {x, nx}, {y, ny}, {z, nz}]
DaVT0[aa_, 0] := DaVT0[aa]; 


Z = {{0,0},{0,0}};
MA = ArrayFlatten[{{\[Sigma]0, Z},{Z, I*\[Sigma]2}}];
MAt = ArrayFlatten[{{Z, \[Sigma]3},{-\[Sigma]1, Z}}];
MB = ArrayFlatten[{{I*\[Sigma]0, Z},{Z, -\[Sigma]2}}];
MBt = ArrayFlatten[{{Z, I*\[Sigma]0},{-\[Sigma]2, Z}}];
MF = ArrayFlatten[{{I*\[Sigma]0, Z},{Z, -\[Sigma]2}}];
MFt = ArrayFlatten[{{Z, \[Sigma]0},{I*\[Sigma]2,Z}}];
MG = ArrayFlatten[{{I*\[Sigma]3,Z },{Z,I*\[Sigma]1}}];
MGt = ArrayFlatten[{{Z, I*\[Sigma]0},{-\[Sigma]2, Z}}];
MA\[Mu] = ArrayFlatten[{{I*\[Sigma]2, Z},{Z, \[Sigma]0}}];
Md = ArrayFlatten[{{I*\[Sigma]1, Z},{Z, -I*\[Sigma]3}}];
M\[Phi]t = ArrayFlatten[{{Z, \[Sigma]1},{\[Sigma]3, Z}}];
MBt\[Mu]\[Nu] = ArrayFlatten[{{Z, -I*\[Sigma]2},{-\[Sigma]0, Z}}];

N\[Eta] = MA;
N\[Eta]t = MFt;
NG = -I*MG;
NAt = MAt;
Nd = -I*Md;
N\[Phi]t = M\[Phi]t;
NF\[Mu]\[Nu] = -I*MA\[Mu];
NBt\[Mu]\[Nu] = MBt\[Mu]\[Nu];
Fmn[mu_,nu_][t,x,y,z]:=D[A[nu][t,x,y,z],\[CapitalStigma][mu]]-D[A[mu][t,x,y,z],\[CapitalStigma][nu]];


(* 4D N=4 Vector Tensor with N=2 Off-Shell Closure: Transformation Rules *)


DaVT[aa_, II_][A[t, x, y, z]] := 
   Sum[MA[[II,JJ]]*\[CapitalPsi][aa][JJ][t, x, y, z], {JJ, 1, 4}]; 
DaVT[aa_, II_][B[t, x, y, z]] := 
   Sum[MB[[II,JJ]]*\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
    {bb, 1, 4}, {JJ, 1, 4}]; 
DaVT[aa_, II_][F[t, x, y, z]] := 
  Sum[MF[[II,JJ]]*\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][JJ][t, x, y, z], 
     \[CapitalStigma][mu]], {bb, 1, 4}, {mu, 0, 3}, {JJ, 1, 4}]
DaVT[aa_, II_][G[t, x, y, z]] := 
  Sum[MG[[II,JJ]]*\[Gamma]5\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][JJ][t, x, y, z], 
     \[CapitalStigma][mu]], {bb, 1, 4}, {mu, 0, 3}, {JJ, 1, 4}]
DaVT[aa_, II_][A[mu_][t, x, y, z]] := 
  Sum[MA\[Mu][[II,JJ]]*\[Gamma]stdown[mu][[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
   {bb, 1, 4}, {JJ, 1, 4}]
DaVT[aa_, II_][d[t, x, y, z]] := 
   Sum[Md[[II,JJ]]*\[Gamma]5\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][JJ][t, x, y, z], 
      \[CapitalStigma][mu]], {bb, 1, 4}, {mu, 0, 3}, {JJ, 1, 4}]; 
DaVT[aa_, II_][At[t, x, y, z]] := 
   Sum[MAt[[II,JJ]]*\[CapitalPsi][aa][JJ][t, x, y, z], {JJ, 1, 4}]; 
DaVT[aa_, II_][Bt[t, x, y, z]] := 
   Sum[MBt[[II,JJ]]*\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
    {bb, 1, 4}, {JJ, 1, 4}]; 
DaVT[aa_, II_][Ft[t, x, y, z]] := 
  Sum[MFt[[II,JJ]]*\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][JJ][t, x, y, z], 
     \[CapitalStigma][mu]], {bb, 1, 4}, {mu, 0, 3}, {JJ, 1, 4}]
DaVT[aa_, II_][Gt[t, x, y, z]] := 
  Sum[MGt[[II,JJ]]*\[Gamma]5\[Gamma][mu][[aa,bb]]*D[\[CapitalPsi][bb][JJ][t, x, y, z], 
     \[CapitalStigma][mu]], {bb, 1, 4}, {mu, 0, 3}, {JJ, 1, 4}]
DaVT[aa_, II_][Bt[mu_, nu_][t, x, y, z]] := 
  (1/4)*Sum[MBt\[Mu]\[Nu][[II,JJ]]*CommuteGammastdown[mu, nu][[aa,bb]]*
     \[CapitalPsi][bb][JJ][t, x, y, z], {bb, 1, 4}, {JJ, 1, 4}]
DaVT[aa_, II_][\[Phi]t[t, x, y, z]] := 
   Sum[M\[Phi]t[[II,JJ]]*\[CapitalPsi][aa][JJ][t, x, y, z], {JJ, 1, 4}]; 
\[CapitalGamma][aa_, bb_] := 
   Sum[I*\[Gamma]down[mu][[aa,bb]]*D[A[t, x, y, z], \[CapitalStigma][mu]] - 
      \[Gamma]5\[Gamma]down[mu][[aa,bb]]*D[B[t, x, y, z], \[CapitalStigma][mu]], 
     {mu, 0, 3}] - I*Cab[[aa,bb]]*F[t, x, y, z]; 
\[CapitalGamma]t[aa_, bb_] := Sum[(-\[Gamma]5\[Gamma]down[mu][[aa,bb]])*
      D[Bt[t, x, y, z], \[CapitalStigma][mu]], {mu, 0, 3}] - 
    I*Cab[[aa,bb]]*Ft[t, x, y, z] + \[Gamma]5down[[aa,bb]]*
     Gt[t, x, y, z]; 
DaVT[aa_, II_][\[CapitalPsi][bb_][JJ_][t, x, y, z]] := 
   N\[Eta][[II,JJ]]*\[CapitalGamma][aa, bb] + N\[Eta]t[[II,JJ]]*\[CapitalGamma]t[aa, bb] + 
    NG[[II,JJ]]*\[Gamma]5down[[aa,bb]]*G[t, x, y, z] + 
    NAt[[II,JJ]]*I*Sum[\[Gamma]down[mu][[aa,bb]]*D[At[t, x, y, z], 
        \[CapitalStigma][mu]], {mu, 0, 3}] + Nd[[II,JJ]]*\[Gamma]5down[[aa,bb]]*
     d[t, x, y, z] + N\[Phi]t[[II,JJ]]*I*
     Sum[\[Gamma]down[mu][[aa,bb]]*D[\[Phi]t[t, x, y, z], \[CapitalStigma][mu]], 
      {mu, 0, 3}] + (1/4)*Sum[NF\[Mu]\[Nu][[II,JJ]]*
       CommuteGammadown[mu, nu][[aa,bb]]*Fmn[mu, nu][t, x, y, z], 
      {mu, 0, 3}, {nu, 0, 3}] + NBt\[Mu]\[Nu][[II,JJ]]*
     Sum[LeviCivitaTensor[4][[mu + 1,nu + 1,m + 1,n + 1]]*
       \[Gamma]5\[Gamma]stdown[mu][[aa,bb]]*D[Bt\[Mu]\[Nu][m, n][t, x, y, z], 
        \[CapitalStigma][mu]], {mu, 0, 3}, {nu, 0, 3}, {m, 0, 3}, {n, 0, 3}]; 


(* 4D N=4 Vector Tensor with N=2 Off-Shell Closure: Zero Brane Boson Rules *)


DaVT0[aa_, II_][A[t, x, y, z]] := 
   Sum[MA[[II,JJ]]*\[CapitalPsi][aa][JJ][t, x, y, z], {JJ, 1, 4}]; 
DaVT0[aa_, II_][B[t, x, y, z]] := 
   Sum[MB[[II,JJ]]*\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
    {bb, 1, 4}, {JJ, 1, 4}]; 
DaVT0[aa_, II_][F[t, x, y, z]] := 
  Sum[MF[[II,JJ]]*\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
   {bb, 1, 4}, {JJ, 1, 4}]
DaVT0[aa_, II_][G[t, x, y, z]] := 
  Sum[MG[[II,JJ]]*\[Gamma]5\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
   {bb, 1, 4}, {JJ, 1, 4}]
DaVT0[aa_, II_][A[mu_][t, x, y, z]] := 
  Sum[MA\[Mu][[II,JJ]]*\[Gamma]stdown[mu][[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
   {bb, 1, 4}, {JJ, 1, 4}]
DaVT0[aa_, II_][d[t, x, y, z]] := 
   Sum[Md[[II,JJ]]*\[Gamma]5\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
    {bb, 1, 4}, {JJ, 1, 4}]; 
DaVT0[aa_, II_][At[t, x, y, z]] := 
   Sum[MAt[[II,JJ]]*\[CapitalPsi][aa][JJ][t, x, y, z], {JJ, 1, 4}]; 
DaVT0[aa_, II_][Bt[t, x, y, z]] := 
   Sum[MBt[[II,JJ]]*\[Gamma]5[[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
    {bb, 1, 4}, {JJ, 1, 4}]; 
DaVT0[aa_, II_][Ft[t, x, y, z]] := 
  Sum[MFt[[II,JJ]]*\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
   {bb, 1, 4}, {JJ, 1, 4}]
DaVT0[aa_, II_][Gt[t, x, y, z]] := 
  Sum[MGt[[II,JJ]]*\[Gamma]5\[Gamma][0][[aa,bb]]*\[CapitalPsi][bb][JJ][t, x, y, z], 
   {bb, 1, 4}, {JJ, 1, 4}]
DaVT0[aa_, II_][Bt[mu_, nu_][t, x, y, z]] := 
  (1/4)*Sum[MBt\[Mu]\[Nu][[II,JJ]]*CommuteGammastdown[mu, nu][[aa,bb]]*
     \[CapitalPsi][bb][JJ][t, x, y, z], {bb, 1, 4}, {JJ, 1, 4}]
DaVT0[aa_, II_][\[Phi]t[t, x, y, z]] := 
   Sum[M\[Phi]t[[II,JJ]]*\[CapitalPsi][aa][JJ][t, x, y, z], {JJ, 1, 4}]; 


BosonsVT = {A[t, x, y, z], B[t, x, y, z], F[t, x, y, z], G[t, x, y, z], At[t, x, y, z], 
    Bt[t, x, y, z], Ft[t, x, y, z], Gt[t, x, y, z], A[1][t, x, y, z], A[2][t, x, y, z], 
    A[3][t, x, y, z], d[t, x, y, z], Bt[1, 2][t, x, y, z], Bt[2, 3][t, x, y, z], 
    Bt[3, 1][t, x, y, z], \[Phi]t[t, x, y, z]}; 
FermionsVT = {\[CapitalPsi][1][1][t, x, y, z], \[CapitalPsi][2][1][t, x, y, z], \[CapitalPsi][3][1][t, x, y, z], 
    \[CapitalPsi][4][1][t, x, y, z], \[CapitalPsi][1][2][t, x, y, z], \[CapitalPsi][2][2][t, x, y, z], \[CapitalPsi][3][2][t, x, y, z], 
    \[CapitalPsi][4][2][t, x, y, z], \[CapitalPsi][1][3][t, x, y, z], \[CapitalPsi][2][3][t, x, y, z], \[CapitalPsi][3][3][t, x, y, z], 
    \[CapitalPsi][4][3][t, x, y, z], \[CapitalPsi][1][4][t, x, y, z], \[CapitalPsi][2][4][t, x, y, z], \[CapitalPsi][3][4][t, x, y, z], 
    \[CapitalPsi][4][4][t, x, y, z]}; 
BosonsVTnew = {A[t, x, y, z], B[t, x, y, z], (-I)*F[t, x, y, z], G[t, x, y, z], 
    A[1][t, x, y, z], A[2][t, x, y, z], A[3][t, x, y, z], d[t, x, y, z], At[t, x, y, z], 
    Bt[t, x, y, z], Ft[t, x, y, z], Gt[t, x, y, z], 2*Bt[1, 2][t, x, y, z], 
    2*Bt[2, 3][t, x, y, z], 2*Bt[3, 1][t, x, y, z], \[Phi]t[t, x, y, z]}; 
FermionsVTnew = {\[CapitalPsi][1][1][t, x, y, z], \[CapitalPsi][2][1][t, x, y, z], \[CapitalPsi][3][1][t, x, y, z], 
    \[CapitalPsi][4][1][t, x, y, z], \[CapitalPsi][1][2][t, x, y, z], \[CapitalPsi][2][2][t, x, y, z], \[CapitalPsi][3][2][t, x, y, z], 
    \[CapitalPsi][4][2][t, x, y, z], \[CapitalPsi][1][3][t, x, y, z], \[CapitalPsi][2][3][t, x, y, z], \[CapitalPsi][3][3][t, x, y, z], 
    \[CapitalPsi][4][3][t, x, y, z], \[CapitalPsi][1][4][t, x, y, z], \[CapitalPsi][2][4][t, x, y, z], \[CapitalPsi][3][4][t, x, y, z], 
    \[CapitalPsi][4][4][t, x, y, z]}; 


LRMatricesVT[] := (LsVT = Table[0, {i, 1, 16}, {j, 1, 16}, {k, 1, 16}]; 
   RsVT = Table[0, {i, 1, 16}, {j, 1, 16}, {k, 1, 16}]; For[II = 1, II < 5, II++, 
    For[i = 1, i < 5, i++, For[j = 1, j < 17, j++, 
       If[ContainsAny[FermionsVTnew, {DaVT0[i, II][BosonsVTnew[[j]]]}], 
         k = Position[FermionsVTnew, DaVT0[i, II][BosonsVTnew[[j]]]][[1,1]]; 
          LsVT[[4*(II - 1) + i,j,k]] = DaVT0[i, II][BosonsVTnew[[j]]]/FermionsVTnew[[k]]]; 
        If[ContainsAny[FermionsVTnew, {-DaVT0[i, II][BosonsVTnew[[j]]]}], 
         k = Position[FermionsVTnew, -DaVT0[i, II][BosonsVTnew[[j]]]][[1,1]]; 
          LsVT[[4*(II - 1) + i,j,k]] = DaVT0[i, II][BosonsVTnew[[j]]]/FermionsVTnew[[k]]]]; 
      RsVT[[4*(II - 1) + i]] = Transpose[LsVT[[4*(II - 1) + i]]]]]; Return[{LsVT, RsVT}])


(* 1D Spinning Particle Theory Off-Shell: N=2, 4, 8, 16 *)


LRMatricesSP[nn_] := (L2SP = {I*\[Sigma]2, -\[Sigma]0}; R2SP = Table[Transpose[L2SP[[k]]], {k, 1, 2}]; 
   L4SP = {KroneckerProduct[I*\[Sigma]1, \[Sigma]2], KroneckerProduct[I*\[Sigma]2, \[Sigma]0], KroneckerProduct[(-I)*\[Sigma]3, \[Sigma]2], 
     KroneckerProduct[\[Sigma]0, \[Sigma]0]}; R4SP = Table[Transpose[L4SP[[k]]], {k, 1, 4}]; 
   L8SP = {KroneckerProduct[I*\[Sigma]0, \[Sigma]3, \[Sigma]2], KroneckerProduct[I*\[Sigma]3, \[Sigma]2, \[Sigma]0], KroneckerProduct[I*\[Sigma]0, \[Sigma]1, \[Sigma]2], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0], KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]1], KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]3], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]2, \[Sigma]2], KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0]}; R8SP = Table[Transpose[L8SP[[k]]], {k, 1, 8}]; 
   L9SP = {KroneckerProduct[I*\[Sigma]0, \[Sigma]3, \[Sigma]2, \[Sigma]1], KroneckerProduct[I*\[Sigma]3, \[Sigma]2, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]0, \[Sigma]1, \[Sigma]2, \[Sigma]1], KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]1, \[Sigma]1], KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]3, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]2, \[Sigma]2, \[Sigma]1], KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]3], 
     KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0]}; R9SP = Table[Transpose[L9SP[[k]]], {k, 1, 9}]; 
   L10SP = {KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0], KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0], KroneckerProduct[I*\[Sigma]1, \[Sigma]3, \[Sigma]2, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]1, \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]3, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]1], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]2, \[Sigma]2], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]3], KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0]}; 
   R10SP = Table[Transpose[L10SP[[k]]], {k, 1, 10}]; L12SP = {KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0], KroneckerProduct[I*\[Sigma]1, \[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]3, \[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]1, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]3, \[Sigma]0, \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]1, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]3, \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]2, \[Sigma]2], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]2, \[Sigma]2, \[Sigma]1], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]2, \[Sigma]2, \[Sigma]3], 
     KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0]}; R12SP = Table[Transpose[L12SP[[k]]], {k, 1, 12}]; 
   L16SP = Table[0, {i, 1, 16}, {j, 1, 128}, {k, 1, 128}]; L16SP[[1]] = I*KroneckerProduct[\[Sigma]2, IdentityMatrix[64]]; 
   For[i = 1, i < 8, i++, L16SP[[i + 1]] = KroneckerProduct[\[Sigma]3, L8SP[[i]], IdentityMatrix[8]]]; 
   For[i = 1, i < 8, i++, L16SP[[i + 8]] = KroneckerProduct[\[Sigma]1, IdentityMatrix[8], L8SP[[i]]]]; 
   L16SP[[16]] = IdentityMatrix[128]; R16SP = Table[Transpose[L16SP[[k]]], {k, 1, 16}]; If[nn == 2, Return[{L2SP, R2SP}]]; 
   If[nn == 4, Return[{L4SP, R4SP}]]; If[nn == 8, Return[{L8SP, R8SP}]]; If[nn == 16, Return[{L16SP, R16SP}]]; 
   If[nn == 9, Return[{L9SP, R9SP}]]; If[nn == 10, Return[{L10SP, R10SP}]]; If[nn == 12, Return[{L12SP, R12SP}]])


GRdN[Ls_, Rs_] := (bool = Table[Ls[[i]] . Rs[[j]] + Ls[[j]] . Rs[[i]], {i, 1, Dimensions[Ls][[1]]}, 
      {j, 1, Dimensions[Ls][[1]]}] == Table[2*IdentityMatrix[Dimensions[Ls][[2]]]*IdentityMatrix[Dimensions[Ls][[1]]][[i,
        j]], {i, 1, Dimensions[Ls][[1]]}, {j, 1, Dimensions[Ls][[1]]}]; Return[bool])


End[];


EndPackage[];
