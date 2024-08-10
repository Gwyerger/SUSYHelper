(* ::Package:: *)

BeginPackage["SUSYHelper`"]


(* ::Section::Closed:: *)
(*Definitions*)


\[Sigma]::usage = "Returns Pauli Matrices by index"
S4::usage = "Returns Matrix Representation of \!\(\*SubscriptBox[\(\[DoubleStruckCapitalS]\), \(4\)]\) by index"; 
S8::usage = "Returns Matrix Representation of \!\(\*SubscriptBox[\(\[DoubleStruckCapitalS]\), \(8\)]\) by index"; 
PopFirst::usage = "Returns a list with first instance of specified element removed"
PopN::usage = "Returns a list with first N instances of specified element removed"
BitXorList::usage = "Performs Xor consecutively on every element in a list and returns the result"
BitXorMapToList::usage = "Performs Xor on every element of a list by a specified binary string"
OneLineToMatrix::usage = "Creates a matrix out of a signed one-line notation"
MatrixToOneLine::usage = "Converts a signed permutation to a signed one-line notation"
BinaryToMatrix::usage = "Converts a binary string \[Element](\!\(\*SubscriptBox[\(\[DoubleStruckCapitalZ]\), \(2\)]\)\!\(\*SuperscriptBox[\()\), \(n\)]\) to its matrix representation \[Element](\!\(\*SubscriptBox[\(\[DoubleStruckCapitalS]\), \(2\)]\)\!\(\*SuperscriptBox[\()\), \(n\)]\)"
Z2Group::usage = "Returns the list of all binary strings for a specified length n"
S2Group::usage = "Returns the group (\!\(\*SubscriptBox[\(\[DoubleStruckCapitalS]\), \(2\)]\)\!\(\*SuperscriptBox[\()\), \(n\)]\) for a specified n"
GLGroup32::usage = "Returns the group \[DoubleStruckCapitalG]\[DoubleStruckCapitalL](3,\!\(\*SubscriptBox[\(\[DoubleStruckCapitalF]\), \(2\)]\)) as a list of matrices"
ReWeight::usage = "Reorders matrices which are represented in one line notation by their weak bruhat ordering"
MatricesToTeX::usage = "Prints a TeX formatting of a list of matrices placed in a specified number of columns"
DelimitWith::usage = "Places a specified delimiter after every character in a string except for the last"
PauliSearchList::usage = "Returns a list of signed permutation matrices in the most concise form possible: decomposition of \!\(\*SubscriptBox[\(\[DoubleStruckCapitalS]\), \(2\)]\) elements, or bracket notation"
PauliSearch::usage = "Returns a signed permutation matrix in the most concise form possible"
PauliSearchListToBinary::usage = "Returns a list of lists with the binary representation of the permutation matrices, given that they are \[Element](\!\(\*SubscriptBox[\(\[DoubleStruckCapitalS]\), \(2\)]\)\!\(\*SuperscriptBox[\()\), \(n\)]\)"
B2NProdSearch::usage = "Returns the most concise notation of a sign factor matrix"
S2NProdSearch::usage = "Returns the most concise form of a permutation matrix"
AddOneInBinary::usage = "Adds 1 to a list of binary digits, but maintains the length of the list"
BracketRep::usage = "Finds the one-line notation and prints an equality in a specified format"
BooleanRep::usage = "Finds the optimal representation of a sign factor matrix in boolean factor or oneline notation"
FindHRs::usage = "Finds the right-handed hopping operators of a set of L-matrices"
FindHLs::usage = "Finds the left-handed hopping operators of a set of L-matrices"
FindBs::usage = "Finds the sign factor matrices of a set of L-matrices defined by left-multiplication"
WeightOrder::usage = "Returns a list of bruhat weak-ordered signed or unsigned permutation matrices"
WeightTotal::usage = "Returns the total weight of a list of signed or unsigned permutation matrices"
GraphVAdinkra::usage = "Returns the graph of a valise Adinkra given a set of L and R-matrices"
GraphBigVAdinkra::usage = "Returns the graph of a valise Adinkra given a set of L and R-matrices of large dimensions"
LRMatricesSP::usage = "Returns the L and R-matrices for the spinning particle multiplet for specified N=2,4,8,9,10,12,16"
\[CapitalGamma]s::usage = "Converts LR-matrices to block Gamma matrices"
GRdN::usage = "Returns the bool for the entire Garden algebra given L and R-matrices"
GRdN\[CapitalGamma]::usage = "Returns the bool for the entire Garden algebra given the clifford algebra reprsentation"
LMatricesSM4D::usage = "Returns the L-matrices corresponding to the 0-brane reduced 4D N=4 vector multiplet with 0 off-shell charges"
LMatricesSM10D::usage = "Returns the L-matrices corresponding to the 0-brane reduced 10D N=1 vector multiplet with 0 off-shell charges"
LMatricesSM4DValise::usage = "Returns the L-matrices corresponding to the 0-brane reduced 4D N=4 vector multiplet with 1 off-shell charge"
LMatricesVT4DValise::usage = "Returns the L-matrices corresponding to the 0-brane reduced 4D N=4 vector-tensor multiplet with 2 off-shell charges"
Involutions::usage = "Returns the involutions on the group \!\(\*SubscriptBox[\(\[DoubleStruckCapitalS]\), \(n\)]\) given n"
PlotTesseractAdinkra::usage = "Creates the graph of a hypercube with proper coloring and dashing as in the maximal 1D N=4 Adinkra"
PlotProjectedTesseractAdinkra::usage = "Creates the graph of a projected hypercube, as in the minimal 1D N=4 Adinkra"
DecodeNCube::usage = "Quotients an N-cube by a given code of N-length and k-dimensions, returning the Adjacency and distance elements of the resulting graph"
GenerateCode::usage = "Returns the code given a generator matrix"
AllSubGraphs::usage = "Returns the connected and disconnected graphs resulting from the n-color decomposition of a given set of hopping operators for a given n"
AllSubGraphsEfficient::usage = "Returns the connected and unconnected graphs similar to AllSubGraphs, but first checks crudly for repeat graphs based on the elements of the distance matrix"
AllSubGraphsRandom::usage = "Randomly performs n-color decompositions for a set number of combinations in attempt to obtain all possible graphs through sampling"
AllSubAlgebras::usage = "Returns the number of n-color decompositions which satisfy the Garden algebra for N=n, connected and disconnected"
AllSubAlgebrasNoPrint::usage = "Returns the combinations of \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(I\)]\) which form valid Garden algebras for a specified n, does not print results"
AllNonSubAlgebrasNoPrint::usage = "Returns the combinations of \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(I\)]\) which do not form valid Garden algebras for a specified n, does not print results"


\[Sigma]0=IdentityMatrix[2];
\[Sigma]1=PauliMatrix[1];
\[Sigma]2=PauliMatrix[2];
\[Sigma]3=PauliMatrix[3];
\[Sigma]s={\[Sigma]0,\[Sigma]1,\[Sigma]2,\[Sigma]3};
\[Sigma]ts={\[Sigma]0,\[Sigma]3};
\[Sigma]bs={\[Sigma]0,\[Sigma]1};


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Initialization*)


\[Sigma][0]:=\[Sigma]s[[1]]
\[Sigma][1]:=\[Sigma]s[[2]]
\[Sigma][2]:=\[Sigma]s[[3]]
\[Sigma][3]:=\[Sigma]s[[4]]
S4[i_]:=Permute[IdentityMatrix[4],GroupElements[SymmetricGroup[4]][[i]]]
S8[i_]:=Permute[IdentityMatrix[8],GroupElements[SymmetricGroup[8]][[i]]]


CommuteGamma[0, 0] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGamma[0, 1] = {{2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, 2}}; 
CommuteGamma[0, 2] = {{0, 0, 2, 0}, {0, 0, 0, 2}, {2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGamma[0, 3] = {{0, -2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, 2, 0}}; 
CommuteGamma[1, 0] = {{-2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGamma[1, 1] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGamma[1, 2] = {{0, 0, 2, 0}, {0, 0, 0, -2}, {-2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGamma[1, 3] = {{0, -2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, 2, 0}}; 
CommuteGamma[2, 0] = {{0, 0, -2, 0}, {0, 0, 0, -2}, {-2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGamma[2, 1] = {{0, 0, -2, 0}, {0, 0, 0, 2}, {2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGamma[2, 2] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGamma[2, 3] = {{0, 0, 0, 2}, {0, 0, 2, 0}, {0, -2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGamma[3, 0] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, -2, 0}}; 
CommuteGamma[3, 1] = {{0, 2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, -2, 0}}; 
CommuteGamma[3, 2] = {{0, 0, 0, -2}, {0, 0, -2, 0}, {0, 2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGamma[3, 3] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammadown[0, 0] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammadown[0, 1] = {{0, -2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, -2, 0}}; 
CommuteGammadown[0, 2] = {{0, 0, 0, 2}, {0, 0, -2, 0}, {0, -2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammadown[0, 3] = {{-2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, 2}}; 
CommuteGammadown[1, 0] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, 2, 0}}; 
CommuteGammadown[1, 1] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammadown[1, 2] = {{0, 0, 0, 2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammadown[1, 3] = {{-2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, 2}}; 
CommuteGammadown[2, 0] = {{0, 0, 0, -2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammadown[2, 1] = {{0, 0, 0, -2}, {0, 0, -2, 0}, {0, -2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammadown[2, 2] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammadown[2, 3] = {{0, 0, -2, 0}, {0, 0, 0, 2}, {-2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGammadown[3, 0] = {{2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammadown[3, 1] = {{2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, -2}}; 
CommuteGammadown[3, 2] = {{0, 0, 2, 0}, {0, 0, 0, -2}, {2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammadown[3, 3] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdown[0, 0] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdown[0, 1] = {{-2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdown[0, 2] = {{0, 0, -2, 0}, {0, 0, 0, -2}, {-2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammastdown[0, 3] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, -2, 0}}; 
CommuteGammastdown[1, 0] = {{2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, 2}}; 
CommuteGammastdown[1, 1] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdown[1, 2] = {{0, 0, 2, 0}, {0, 0, 0, -2}, {-2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGammastdown[1, 3] = {{0, -2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, 2, 0}}; 
CommuteGammastdown[2, 0] = {{0, 0, 2, 0}, {0, 0, 0, 2}, {2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGammastdown[2, 1] = {{0, 0, -2, 0}, {0, 0, 0, 2}, {2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammastdown[2, 2] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdown[2, 3] = {{0, 0, 0, 2}, {0, 0, 2, 0}, {0, -2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammastdown[3, 0] = {{0, -2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, 2, 0}}; 
CommuteGammastdown[3, 1] = {{0, 2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, -2, 0}}; 
CommuteGammastdown[3, 2] = {{0, 0, 0, -2}, {0, 0, -2, 0}, {0, 2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammastdown[3, 3] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdowndown[0, 0] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdowndown[0, 1] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, 2, 0}}; 
CommuteGammastdowndown[0, 2] = {{0, 0, 0, -2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammastdowndown[0, 3] = {{2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdowndown[1, 0] = {{0, -2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, -2, 0}}; 
CommuteGammastdowndown[1, 1] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdowndown[1, 2] = {{0, 0, 0, 2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammastdowndown[1, 3] = {{-2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, 2}}; 
CommuteGammastdowndown[2, 0] = {{0, 0, 0, 2}, {0, 0, -2, 0}, {0, -2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammastdowndown[2, 1] = {{0, 0, 0, -2}, {0, 0, -2, 0}, {0, -2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammastdowndown[2, 2] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdowndown[2, 3] = {{0, 0, -2, 0}, {0, 0, 0, 2}, {-2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGammastdowndown[3, 0] = {{-2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, 2}}; 
CommuteGammastdowndown[3, 1] = {{2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdowndown[3, 2] = {{0, 0, 2, 0}, {0, 0, 0, -2}, {2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammastdowndown[3, 3] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstup[0, 0] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstup[0, 1] = {{-2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdownstup[0, 2] = {{0, 0, -2, 0}, {0, 0, 0, -2}, {-2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammastdownstup[0, 3] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, -2, 0}}; 
CommuteGammastdownstup[1, 0] = {{-2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdownstup[1, 1] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstup[1, 2] = {{0, 0, 2, 0}, {0, 0, 0, -2}, {-2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGammastdownstup[1, 3] = {{0, -2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, 2, 0}}; 
CommuteGammastdownstup[2, 0] = {{0, 0, -2, 0}, {0, 0, 0, -2}, {-2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammastdownstup[2, 1] = {{0, 0, -2, 0}, {0, 0, 0, 2}, {2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammastdownstup[2, 2] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstup[2, 3] = {{0, 0, 0, 2}, {0, 0, 2, 0}, {0, -2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammastdownstup[3, 0] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, -2, 0}}; 
CommuteGammastdownstup[3, 1] = {{0, 2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, -2, 0}}; 
CommuteGammastdownstup[3, 2] = {{0, 0, 0, -2}, {0, 0, -2, 0}, {0, 2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammastdownstup[3, 3] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstupdown[0, 0] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstupdown[0, 1] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, 2, 0}}; 
CommuteGammastdownstupdown[0, 2] = {{0, 0, 0, -2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammastdownstupdown[0, 3] = {{2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdownstupdown[1, 0] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, 2, 0}}; 
CommuteGammastdownstupdown[1, 1] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstupdown[1, 2] = {{0, 0, 0, 2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammastdownstupdown[1, 3] = {{-2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, 2}}; 
CommuteGammastdownstupdown[2, 0] = {{0, 0, 0, -2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammastdownstupdown[2, 1] = {{0, 0, 0, -2}, {0, 0, -2, 0}, {0, -2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammastdownstupdown[2, 2] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammastdownstupdown[2, 3] = {{0, 0, -2, 0}, {0, 0, 0, 2}, {-2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGammastdownstupdown[3, 0] = {{2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdownstupdown[3, 1] = {{2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, -2}}; 
CommuteGammastdownstupdown[3, 2] = {{0, 0, 2, 0}, {0, 0, 0, -2}, {2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammastdownstupdown[3, 3] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammaup[0, 0] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammaup[0, 1] = {{0, 2, 0, 0}, {2, 0, 0, 0}, {0, 0, 0, 2}, {0, 0, 2, 0}}; 
CommuteGammaup[0, 2] = {{0, 0, 0, -2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammaup[0, 3] = {{2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, -2}}; 
CommuteGammaup[1, 0] = {{0, -2, 0, 0}, {-2, 0, 0, 0}, {0, 0, 0, -2}, {0, 0, -2, 0}}; 
CommuteGammaup[1, 1] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammaup[1, 2] = {{0, 0, 0, 2}, {0, 0, 2, 0}, {0, 2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammaup[1, 3] = {{-2, 0, 0, 0}, {0, -2, 0, 0}, {0, 0, 2, 0}, {0, 0, 0, 2}}; 
CommuteGammaup[2, 0] = {{0, 0, 0, 2}, {0, 0, -2, 0}, {0, -2, 0, 0}, {2, 0, 0, 0}}; 
CommuteGammaup[2, 1] = {{0, 0, 0, -2}, {0, 0, -2, 0}, {0, -2, 0, 0}, {-2, 0, 0, 0}}; 
CommuteGammaup[2, 2] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
CommuteGammaup[2, 3] = {{0, 0, -2, 0}, {0, 0, 0, 2}, {-2, 0, 0, 0}, {0, 2, 0, 0}}; 
CommuteGammaup[3, 0] = {{-2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, 2}}; 
CommuteGammaup[3, 1] = {{2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, -2, 0}, {0, 0, 0, -2}}; 
CommuteGammaup[3, 2] = {{0, 0, 2, 0}, {0, 0, 0, -2}, {2, 0, 0, 0}, {0, -2, 0, 0}}; 
CommuteGammaup[3, 3] = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
\[Gamma][0] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma][1] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma][2] = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma][3] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]5 = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5down = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5test = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
\[Gamma]5testdownupupup = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
\[Gamma]5\[Gamma][0] = {{0, 0, I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma][1] = {{0, 0, I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma][2] = {{-I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma][3] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]down[0] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]down[1] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]down[2] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]down[3] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]stdown[0] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]stdown[1] = {{0, 0, I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]stdown[2] = {{-I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]stdown[3] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]stdowndown[0] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]stdowndown[1] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]stdowndown[2] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]stdowndown[3] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]stdownup[0] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]stdownup[1] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]stdownup[2] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]stdownup[3] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]up[0] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]up[1] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]up[2] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]up[3] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][0, 0] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][0, 1] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][0, 2] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][0, 3] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][1, 0] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][1, 1] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][1, 2] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][1, 3] = {{0, 0, I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][2, 0] = {{0, -I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][2, 1] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][2, 2] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][2, 3] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma][3, 0] = {{0, 0, -I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][3, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma][3, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma][3, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[0, 0] = {{0, 0, I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[0, 1] = {{0, 0, -I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[0, 2] = {{I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]down[0, 3] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[1, 0] = {{0, 0, I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[1, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[1, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]down[1, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[2, 0] = {{-I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]down[2, 1] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]down[2, 2] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[2, 3] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[3, 0] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[3, 1] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[3, 2] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]down[3, 3] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[0, 0] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[0, 1] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[0, 2] = {{0, -I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[0, 3] = {{0, 0, -I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[1, 0] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[1, 1] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[1, 2] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[1, 3] = {{0, 0, I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[2, 0] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[2, 1] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[2, 2] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[2, 3] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[3, 0] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[3, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[3, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdown[3, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[0, 0] = {{0, 0, I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[0, 1] = {{0, 0, I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[0, 2] = {{-I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[0, 3] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[1, 0] = {{0, 0, -I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[1, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[1, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[1, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[2, 0] = {{I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[2, 1] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[2, 2] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[2, 3] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[3, 0] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[3, 1] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[3, 2] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstdowndown[3, 3] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[0, 0] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[0, 1] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[0, 2] = {{0, -I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[0, 3] = {{0, 0, -I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[1, 0] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[1, 1] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[1, 2] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[1, 3] = {{0, 0, I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[2, 0] = {{0, -I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[2, 1] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[2, 2] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[2, 3] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[3, 0] = {{0, 0, -I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[3, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[3, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstup[3, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[0, 0] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[0, 1] = {{0, 0, I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[0, 2] = {{-I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[0, 3] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[1, 0] = {{0, 0, I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[1, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[1, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[1, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[2, 0] = {{-I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[2, 1] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[2, 2] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[2, 3] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[3, 0] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[3, 1] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[3, 2] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stdownstupdown[3, 3] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[0, 0] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[0, 1] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[0, 2] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[0, 3] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[1, 0] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[1, 1] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[1, 2] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[1, 3] = {{0, 0, I, 0}, {0, 0, 0, I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[2, 0] = {{0, I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[2, 1] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[2, 2] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[2, 3] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[3, 0] = {{0, 0, I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[3, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[3, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdown[3, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[0, 0] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[0, 1] = {{0, 0, -I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[0, 2] = {{I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[0, 3] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[1, 0] = {{0, 0, -I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[1, 1] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[1, 2] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[1, 3] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[2, 0] = {{I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[2, 1] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[2, 2] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[2, 3] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[3, 0] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[3, 1] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[3, 2] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]stupstdowndown[3, 3] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[0, 0] = {{0, 0, -I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[0, 1] = {{0, 0, -I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[0, 2] = {{I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]up[0, 3] = {{0, 0, 0, I}, {0, 0, I, 0}, {0, I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[1, 0] = {{0, 0, I, 0}, {0, 0, 0, -I}, {I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[1, 1] = {{0, 0, I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[1, 2] = {{-I, 0, 0, 0}, {0, I, 0, 0}, {0, 0, -I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]up[1, 3] = {{0, 0, 0, -I}, {0, 0, I, 0}, {0, I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[2, 0] = {{-I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, I}}; 
\[Gamma]5\[Gamma]\[Gamma]up[2, 1] = {{I, 0, 0, 0}, {0, -I, 0, 0}, {0, 0, I, 0}, {0, 0, 0, -I}}; 
\[Gamma]5\[Gamma]\[Gamma]up[2, 2] = {{0, 0, I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[2, 3] = {{0, -I, 0, 0}, {-I, 0, 0, 0}, {0, 0, 0, -I}, {0, 0, -I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[3, 0] = {{0, 0, 0, -I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {-I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[3, 1] = {{0, 0, 0, I}, {0, 0, -I, 0}, {0, -I, 0, 0}, {I, 0, 0, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[3, 2] = {{0, I, 0, 0}, {I, 0, 0, 0}, {0, 0, 0, I}, {0, 0, I, 0}}; 
\[Gamma]5\[Gamma]\[Gamma]up[3, 3] = {{0, 0, I, 0}, {0, 0, 0, I}, {-I, 0, 0, 0}, {0, -I, 0, 0}}; 
\[Gamma]down[0] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]down[1] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]down[2] = {{0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]down[3] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]stdown[0] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]stdown[1] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]stdown[2] = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]stdown[3] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]stdowndown[0] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]stdowndown[1] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]stdowndown[2] = {{0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]stdowndown[3] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]up[0] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]up[1] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]up[2] = {{0, 0, -1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]up[3] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma][0, 0] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma][0, 1] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma][0, 2] = {{0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma][0, 3] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma][1, 0] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma][1, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma][1, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma][1, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma][2, 0] = {{0, 0, -1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma][2, 1] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma][2, 2] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma][2, 3] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma][3, 0] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma][3, 1] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma][3, 2] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma][3, 3] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]down[0, 0] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]down[0, 1] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]down[0, 2] = {{0, 0, 0, 1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]down[0, 3] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]down[1, 0] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]down[1, 1] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]down[1, 2] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]down[1, 3] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]down[2, 0] = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]down[2, 1] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]down[2, 2] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]down[2, 3] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]down[3, 0] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]down[3, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]down[3, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]down[3, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstdown[0, 0] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstdown[0, 1] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstdown[0, 2] = {{0, 0, -1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdown[0, 3] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstdown[1, 0] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstdown[1, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstdown[1, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdown[1, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stdownstdown[2, 0] = {{0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdown[2, 1] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdown[2, 2] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstdown[2, 3] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdown[3, 0] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stdownstdown[3, 1] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstdown[3, 2] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdown[3, 3] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstdowndown[0, 0] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[0, 1] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[0, 2] = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[0, 3] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstdowndown[1, 0] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[1, 1] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[1, 2] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[1, 3] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstdowndown[2, 0] = {{0, 0, 0, 1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[2, 1] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[2, 2] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[2, 3] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[3, 0] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstdowndown[3, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstdowndown[3, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstdowndown[3, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstup[0, 0] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstup[0, 1] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstup[0, 2] = {{0, 0, -1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstup[0, 3] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstup[1, 0] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstup[1, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstup[1, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstup[1, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stdownstup[2, 0] = {{0, 0, -1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstup[2, 1] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstup[2, 2] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstup[2, 3] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstup[3, 0] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstup[3, 1] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstup[3, 2] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstup[3, 3] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstupdown[0, 0] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[0, 1] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[0, 2] = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[0, 3] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstupdown[1, 0] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[1, 1] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[1, 2] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[1, 3] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stdownstupdown[2, 0] = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[2, 1] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[2, 2] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[2, 3] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[3, 0] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstupdown[3, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stdownstupdown[3, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stdownstupdown[3, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stupstdown[0, 0] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdown[0, 1] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdown[0, 2] = {{0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stupstdown[0, 3] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stupstdown[1, 0] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdown[1, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdown[1, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stupstdown[1, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stupstdown[2, 0] = {{0, 0, 1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stupstdown[2, 1] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stupstdown[2, 2] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdown[2, 3] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stupstdown[3, 0] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]stupstdown[3, 1] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stupstdown[3, 2] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stupstdown[3, 3] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdowndown[0, 0] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[0, 1] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[0, 2] = {{0, 0, 0, 1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[0, 3] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdowndown[1, 0] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[1, 1] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[1, 2] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[1, 3] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdowndown[2, 0] = {{0, 0, 0, 1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[2, 1] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[2, 2] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[2, 3] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[3, 0] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]stupstdowndown[3, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]stupstdowndown[3, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]stupstdowndown[3, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]up[0, 0] = {{0, 1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]up[0, 1] = {{0, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}; 
\[Gamma]\[Gamma]up[0, 2] = {{0, 0, 0, -1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]up[0, 3] = {{1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]up[1, 0] = {{0, -1, 0, 0}, {-1, 0, 0, 0}, {0, 0, 0, -1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]up[1, 1] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]up[1, 2] = {{0, 0, 0, 1}, {0, 0, 1, 0}, {0, 1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]up[1, 3] = {{-1, 0, 0, 0}, {0, -1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]up[2, 0] = {{0, 0, 0, 1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {1, 0, 0, 0}}; 
\[Gamma]\[Gamma]up[2, 1] = {{0, 0, 0, -1}, {0, 0, -1, 0}, {0, -1, 0, 0}, {-1, 0, 0, 0}}; 
\[Gamma]\[Gamma]up[2, 2] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 
\[Gamma]\[Gamma]up[2, 3] = {{0, 0, -1, 0}, {0, 0, 0, 1}, {-1, 0, 0, 0}, {0, 1, 0, 0}}; 
\[Gamma]\[Gamma]up[3, 0] = {{-1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, 1}}; 
\[Gamma]\[Gamma]up[3, 1] = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, -1, 0}, {0, 0, 0, -1}}; 
\[Gamma]\[Gamma]up[3, 2] = {{0, 0, 1, 0}, {0, 0, 0, -1}, {1, 0, 0, 0}, {0, -1, 0, 0}}; 
\[Gamma]\[Gamma]up[3, 3] = {{0, -1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 1}, {0, 0, -1, 0}}; 


PopFirst[list_, elem_] := (For[hh = 1, hh < Length[list] + 1, hh++, If[list[[hh]] == elem, l = Delete[list, hh]; Return[l]]]; 
   Return[list])
   
PopN[list_, elem_, n_] := (l = list; For[hhh = 1, hhh < n + 1, hhh++, l = PopFirst[l, elem]]; Return[l])

BitXorList[List_] := (list = List; len = Length[list]; For[hh = 1, hh < len, hh++, 
    list[[1]] = BitXor[list[[1]], list[[2]]]; list = Delete[list, 2]]; Return[list])
    
BitXorMapToList[List_, str_] := (list = List; For[hh = 1, hh < Length[list] + 1, hh++, list[[hh]] = BitXor[list[[hh]], str]]; 
   Return[list])
   
OneLineToMatrix[OneLine_]:= (
L = Length[OneLine];
Return[Normal[SparseArray[Table[{i,Abs[OneLine[[i]]]}-> Sign[OneLine[[i]]]*1, {i, 1, L}]]]])
   
MatrixToOneLine[Matrix_] := (L = Dimensions[Matrix][[1]]; v = Table[i, {i, 1, L}]; Return[Matrix . v])

BinaryToMatrix[Binary_] := Return[Catch[H\[Sigma] = {1}; h = 1; For[ii = 1, ii < Length[Binary] + 1, ii++, 
      H\[Sigma] = KroneckerProduct[H\[Sigma], \[Sigma]bs[[Binary[[ii]] + 1]]]]; Throw[H\[Sigma]]]]
      
Z2Group[n_] := (binary = Table[IntegerDigits[hh, 2, n], {hh, 0, 2^n - 1}]; Return[binary])

S2Group[n_] := (Z2 = Z2Group[n]; Return[Table[BinaryToMatrix[Z2[[hh]]], {hh, 1, 2^n}]])

GLGroup32[] := Return[{{{0, 0, 1}, {0, 1, 0}, {1, 0, 0}}, {{0, 0, 1}, {0, 1, 0}, {1, 0, 1}}, 
    {{0, 0, 1}, {0, 1, 0}, {1, 1, 0}}, {{0, 0, 1}, {0, 1, 0}, {1, 1, 1}}, {{0, 0, 1}, {0, 1, 1}, {1, 0, 0}}, 
    {{0, 0, 1}, {0, 1, 1}, {1, 0, 1}}, {{0, 0, 1}, {0, 1, 1}, {1, 1, 0}}, {{0, 0, 1}, {0, 1, 1}, {1, 1, 1}}, 
    {{0, 0, 1}, {1, 0, 0}, {0, 1, 0}}, {{0, 0, 1}, {1, 0, 0}, {0, 1, 1}}, {{0, 0, 1}, {1, 0, 0}, {1, 1, 0}}, 
    {{0, 0, 1}, {1, 0, 0}, {1, 1, 1}}, {{0, 0, 1}, {1, 0, 1}, {0, 1, 0}}, {{0, 0, 1}, {1, 0, 1}, {0, 1, 1}}, 
    {{0, 0, 1}, {1, 0, 1}, {1, 1, 0}}, {{0, 0, 1}, {1, 0, 1}, {1, 1, 1}}, {{0, 0, 1}, {1, 1, 0}, {0, 1, 0}}, 
    {{0, 0, 1}, {1, 1, 0}, {0, 1, 1}}, {{0, 0, 1}, {1, 1, 0}, {1, 0, 0}}, {{0, 0, 1}, {1, 1, 0}, {1, 0, 1}}, 
    {{0, 0, 1}, {1, 1, 1}, {0, 1, 0}}, {{0, 0, 1}, {1, 1, 1}, {0, 1, 1}}, {{0, 0, 1}, {1, 1, 1}, {1, 0, 0}}, 
    {{0, 0, 1}, {1, 1, 1}, {1, 0, 1}}, {{0, 1, 0}, {0, 0, 1}, {1, 0, 0}}, {{0, 1, 0}, {0, 0, 1}, {1, 0, 1}}, 
    {{0, 1, 0}, {0, 0, 1}, {1, 1, 0}}, {{0, 1, 0}, {0, 0, 1}, {1, 1, 1}}, {{0, 1, 0}, {0, 1, 1}, {1, 0, 0}}, 
    {{0, 1, 0}, {0, 1, 1}, {1, 0, 1}}, {{0, 1, 0}, {0, 1, 1}, {1, 1, 0}}, {{0, 1, 0}, {0, 1, 1}, {1, 1, 1}}, 
    {{0, 1, 0}, {1, 0, 0}, {0, 0, 1}}, {{0, 1, 0}, {1, 0, 0}, {0, 1, 1}}, {{0, 1, 0}, {1, 0, 0}, {1, 0, 1}}, 
    {{0, 1, 0}, {1, 0, 0}, {1, 1, 1}}, {{0, 1, 0}, {1, 0, 1}, {0, 0, 1}}, {{0, 1, 0}, {1, 0, 1}, {0, 1, 1}}, 
    {{0, 1, 0}, {1, 0, 1}, {1, 0, 0}}, {{0, 1, 0}, {1, 0, 1}, {1, 1, 0}}, {{0, 1, 0}, {1, 1, 0}, {0, 0, 1}}, 
    {{0, 1, 0}, {1, 1, 0}, {0, 1, 1}}, {{0, 1, 0}, {1, 1, 0}, {1, 0, 1}}, {{0, 1, 0}, {1, 1, 0}, {1, 1, 1}}, 
    {{0, 1, 0}, {1, 1, 1}, {0, 0, 1}}, {{0, 1, 0}, {1, 1, 1}, {0, 1, 1}}, {{0, 1, 0}, {1, 1, 1}, {1, 0, 0}}, 
    {{0, 1, 0}, {1, 1, 1}, {1, 1, 0}}, {{0, 1, 1}, {0, 0, 1}, {1, 0, 0}}, {{0, 1, 1}, {0, 0, 1}, {1, 0, 1}}, 
    {{0, 1, 1}, {0, 0, 1}, {1, 1, 0}}, {{0, 1, 1}, {0, 0, 1}, {1, 1, 1}}, {{0, 1, 1}, {0, 1, 0}, {1, 0, 0}}, 
    {{0, 1, 1}, {0, 1, 0}, {1, 0, 1}}, {{0, 1, 1}, {0, 1, 0}, {1, 1, 0}}, {{0, 1, 1}, {0, 1, 0}, {1, 1, 1}}, 
    {{0, 1, 1}, {1, 0, 0}, {0, 0, 1}}, {{0, 1, 1}, {1, 0, 0}, {0, 1, 0}}, {{0, 1, 1}, {1, 0, 0}, {1, 0, 1}}, 
    {{0, 1, 1}, {1, 0, 0}, {1, 1, 0}}, {{0, 1, 1}, {1, 0, 1}, {0, 0, 1}}, {{0, 1, 1}, {1, 0, 1}, {0, 1, 0}}, 
    {{0, 1, 1}, {1, 0, 1}, {1, 0, 0}}, {{0, 1, 1}, {1, 0, 1}, {1, 1, 1}}, {{0, 1, 1}, {1, 1, 0}, {0, 0, 1}}, 
    {{0, 1, 1}, {1, 1, 0}, {0, 1, 0}}, {{0, 1, 1}, {1, 1, 0}, {1, 0, 0}}, {{0, 1, 1}, {1, 1, 0}, {1, 1, 1}}, 
    {{0, 1, 1}, {1, 1, 1}, {0, 0, 1}}, {{0, 1, 1}, {1, 1, 1}, {0, 1, 0}}, {{0, 1, 1}, {1, 1, 1}, {1, 0, 1}}, 
    {{0, 1, 1}, {1, 1, 1}, {1, 1, 0}}, {{1, 0, 0}, {0, 0, 1}, {0, 1, 0}}, {{1, 0, 0}, {0, 0, 1}, {0, 1, 1}}, 
    {{1, 0, 0}, {0, 0, 1}, {1, 1, 0}}, {{1, 0, 0}, {0, 0, 1}, {1, 1, 1}}, {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, 
    {{1, 0, 0}, {0, 1, 0}, {0, 1, 1}}, {{1, 0, 0}, {0, 1, 0}, {1, 0, 1}}, {{1, 0, 0}, {0, 1, 0}, {1, 1, 1}}, 
    {{1, 0, 0}, {0, 1, 1}, {0, 0, 1}}, {{1, 0, 0}, {0, 1, 1}, {0, 1, 0}}, {{1, 0, 0}, {0, 1, 1}, {1, 0, 1}}, 
    {{1, 0, 0}, {0, 1, 1}, {1, 1, 0}}, {{1, 0, 0}, {1, 0, 1}, {0, 1, 0}}, {{1, 0, 0}, {1, 0, 1}, {0, 1, 1}}, 
    {{1, 0, 0}, {1, 0, 1}, {1, 1, 0}}, {{1, 0, 0}, {1, 0, 1}, {1, 1, 1}}, {{1, 0, 0}, {1, 1, 0}, {0, 0, 1}}, 
    {{1, 0, 0}, {1, 1, 0}, {0, 1, 1}}, {{1, 0, 0}, {1, 1, 0}, {1, 0, 1}}, {{1, 0, 0}, {1, 1, 0}, {1, 1, 1}}, 
    {{1, 0, 0}, {1, 1, 1}, {0, 0, 1}}, {{1, 0, 0}, {1, 1, 1}, {0, 1, 0}}, {{1, 0, 0}, {1, 1, 1}, {1, 0, 1}}, 
    {{1, 0, 0}, {1, 1, 1}, {1, 1, 0}}, {{1, 0, 1}, {0, 0, 1}, {0, 1, 0}}, {{1, 0, 1}, {0, 0, 1}, {0, 1, 1}}, 
    {{1, 0, 1}, {0, 0, 1}, {1, 1, 0}}, {{1, 0, 1}, {0, 0, 1}, {1, 1, 1}}, {{1, 0, 1}, {0, 1, 0}, {0, 0, 1}}, 
    {{1, 0, 1}, {0, 1, 0}, {0, 1, 1}}, {{1, 0, 1}, {0, 1, 0}, {1, 0, 0}}, {{1, 0, 1}, {0, 1, 0}, {1, 1, 0}}, 
    {{1, 0, 1}, {0, 1, 1}, {0, 0, 1}}, {{1, 0, 1}, {0, 1, 1}, {0, 1, 0}}, {{1, 0, 1}, {0, 1, 1}, {1, 0, 0}}, 
    {{1, 0, 1}, {0, 1, 1}, {1, 1, 1}}, {{1, 0, 1}, {1, 0, 0}, {0, 1, 0}}, {{1, 0, 1}, {1, 0, 0}, {0, 1, 1}}, 
    {{1, 0, 1}, {1, 0, 0}, {1, 1, 0}}, {{1, 0, 1}, {1, 0, 0}, {1, 1, 1}}, {{1, 0, 1}, {1, 1, 0}, {0, 0, 1}}, 
    {{1, 0, 1}, {1, 1, 0}, {0, 1, 0}}, {{1, 0, 1}, {1, 1, 0}, {1, 0, 0}}, {{1, 0, 1}, {1, 1, 0}, {1, 1, 1}}, 
    {{1, 0, 1}, {1, 1, 1}, {0, 0, 1}}, {{1, 0, 1}, {1, 1, 1}, {0, 1, 1}}, {{1, 0, 1}, {1, 1, 1}, {1, 0, 0}}, 
    {{1, 0, 1}, {1, 1, 1}, {1, 1, 0}}, {{1, 1, 0}, {0, 0, 1}, {0, 1, 0}}, {{1, 1, 0}, {0, 0, 1}, {0, 1, 1}}, 
    {{1, 1, 0}, {0, 0, 1}, {1, 0, 0}}, {{1, 1, 0}, {0, 0, 1}, {1, 0, 1}}, {{1, 1, 0}, {0, 1, 0}, {0, 0, 1}}, 
    {{1, 1, 0}, {0, 1, 0}, {0, 1, 1}}, {{1, 1, 0}, {0, 1, 0}, {1, 0, 1}}, {{1, 1, 0}, {0, 1, 0}, {1, 1, 1}}, 
    {{1, 1, 0}, {0, 1, 1}, {0, 0, 1}}, {{1, 1, 0}, {0, 1, 1}, {0, 1, 0}}, {{1, 1, 0}, {0, 1, 1}, {1, 0, 0}}, 
    {{1, 1, 0}, {0, 1, 1}, {1, 1, 1}}, {{1, 1, 0}, {1, 0, 0}, {0, 0, 1}}, {{1, 1, 0}, {1, 0, 0}, {0, 1, 1}}, 
    {{1, 1, 0}, {1, 0, 0}, {1, 0, 1}}, {{1, 1, 0}, {1, 0, 0}, {1, 1, 1}}, {{1, 1, 0}, {1, 0, 1}, {0, 0, 1}}, 
    {{1, 1, 0}, {1, 0, 1}, {0, 1, 0}}, {{1, 1, 0}, {1, 0, 1}, {1, 0, 0}}, {{1, 1, 0}, {1, 0, 1}, {1, 1, 1}}, 
    {{1, 1, 0}, {1, 1, 1}, {0, 1, 0}}, {{1, 1, 0}, {1, 1, 1}, {0, 1, 1}}, {{1, 1, 0}, {1, 1, 1}, {1, 0, 0}}, 
    {{1, 1, 0}, {1, 1, 1}, {1, 0, 1}}, {{1, 1, 1}, {0, 0, 1}, {0, 1, 0}}, {{1, 1, 1}, {0, 0, 1}, {0, 1, 1}}, 
    {{1, 1, 1}, {0, 0, 1}, {1, 0, 0}}, {{1, 1, 1}, {0, 0, 1}, {1, 0, 1}}, {{1, 1, 1}, {0, 1, 0}, {0, 0, 1}}, 
    {{1, 1, 1}, {0, 1, 0}, {0, 1, 1}}, {{1, 1, 1}, {0, 1, 0}, {1, 0, 0}}, {{1, 1, 1}, {0, 1, 0}, {1, 1, 0}}, 
    {{1, 1, 1}, {0, 1, 1}, {0, 0, 1}}, {{1, 1, 1}, {0, 1, 1}, {0, 1, 0}}, {{1, 1, 1}, {0, 1, 1}, {1, 0, 1}}, 
    {{1, 1, 1}, {0, 1, 1}, {1, 1, 0}}, {{1, 1, 1}, {1, 0, 0}, {0, 0, 1}}, {{1, 1, 1}, {1, 0, 0}, {0, 1, 0}}, 
    {{1, 1, 1}, {1, 0, 0}, {1, 0, 1}}, {{1, 1, 1}, {1, 0, 0}, {1, 1, 0}}, {{1, 1, 1}, {1, 0, 1}, {0, 0, 1}}, 
    {{1, 1, 1}, {1, 0, 1}, {0, 1, 1}}, {{1, 1, 1}, {1, 0, 1}, {1, 0, 0}}, {{1, 1, 1}, {1, 0, 1}, {1, 1, 0}}, 
    {{1, 1, 1}, {1, 1, 0}, {0, 1, 0}}, {{1, 1, 1}, {1, 1, 0}, {0, 1, 1}}, {{1, 1, 1}, {1, 1, 0}, {1, 0, 0}}, 
    {{1, 1, 1}, {1, 1, 0}, {1, 0, 1}}}]


(* ::Section::Closed:: *)
(*Useful Functions*)


ReWeight[OneLine_] := (Num = Length[OneLine]; 
   Base = Length[OneLine[[1]]]; ListWeight = {}; 
   For[i = 1, i <= Num, i++, Weight = 0; 
     For[j = 1, j <= Base, j++, Weight = 
       (Weight + Abs[OneLine[[i]][[j]]])*Base]; 
     AppendTo[ListWeight, Weight]]; 
   Return[OneLine[[Ordering[ListWeight, Num]]]])
   
MatricesToTeX[Ms_, ncols_, str_] := 
  {Tex = ""; For[i = 1, i < Length[Ms] + 1, i++, 
     Tex = StringJoin[Tex, str, "_{", ToString[i], 
        "} &= ", ToString[TeXForm[Ms[[i]]]], "&"]; 
      If[Mod[i, ncols] == 0 &&  !i == 1, 
       Tex = StringJoin[Tex, "\\\\"]]]; Print[Tex]}
       
DelimitWith[Str_, Del_] := (char = Characters[Str]; 
   s = StringJoin[{Table[StringJoin[char[[hi]], Del], 
       {hi, 1, Length[char] - 1}], 
      char[[Length[char]]]}]; Return[s])


PauliSearchList[Ls_, Str_] := (lstr = ""; 
   For[ii = 1, ii < Length[Ls] + 1, ii++, 
    lstr = StringJoin[lstr, PauliSearch[Ls[[ii]], 
       StringJoin[Str, ToString[ii]]], "\n"]]; 
   Return[lstr])
   
PauliSearchListToBinary[Hs_] := (chars = {}; 
   For[ii = 1, ii < Length[Hs] + 1, ii++, 
    chars = Append[chars, ToExpression[
       Characters[S2NProdSearch[Hs[[ii]], "", 
         "Bare"]]]]]; Return[chars])
         
PauliSearch[L_, Str_] := (P = Abs[L]; 
   B = L . Transpose[P]; If[L == P, 
    Return[S2NProdSearch[P, Str]]]; 
   If[L == B, Return[B2NProdSearch[B, Str]]]; 
   str = StringJoin[B2NProdSearch[B, Str], 
     S2NProdSearch[P, ""]]; Return[str])
     
B2NProdSearch[B_, Str_] := 
  Return[B2NProdSearch[B, Str, "Display"]]
  
B2NProdSearch[B_, Str_, Lang_] := 
  (If[Lang == "Python", Delim = ", "; LBrace = "["; 
     RBrace = "]"]; If[Lang == "Mathematica", 
    Delim = ", "; LBrace = "{"; RBrace = "}"]; 
   If[Lang == "LaTeX", Delim = "\\;"; LBrace = "("; 
     RBrace = ")"]; If[Lang == "Display", 
    Delim = ""; LBrace = "("; RBrace = ")"]; 
   If[Lang == "Bare", Delim = ""; LBrace = ""; 
     RBrace = ""]; str = Str; 
   bosons = Dimensions[B][[1]]; 
   fermions = Dimensions[B][[2]]; 
   If[bosons != fermions ||  !IntegerQ[Log2[fermions]], 
    If[str != "", str = StringJoin[str, " = "]]; 
     str = StringJoin[str, BooleanRep[B, "", Lang]]; 
     Return[str]]; len = Log2[fermions]; 
   bs = Table[0, {hh, 1, len}]; 
   For[i = 1, i < fermions + 1, i++, 
    try = {1}; For[j = 1, j < len + 1, j++, 
      try = KroneckerProduct[try, \[Sigma]ts[[bs[[j]] + 1]]]]; 
     If[B == try, If[str != "", str = StringJoin[str, 
          " = "]]; str = StringJoin[str, LBrace, 
         DelimitWith[IntegerString[FromDigits[bs, 2], 2, 
           len], Delim], RBrace]; Return[str], 
      bs = AddOneInBinary[bs]]]; If[str != "", 
    str = StringJoin[str, " = "]]; 
   str = StringJoin[str, BooleanRep[B, "", Lang]]; 
   Return[str])
   
S2NProdSearch[P_, Str_] := 
  Return[S2NProdSearch[P, Str, "Display"]]
  
S2NProdSearch[P_, Str_, Lang_] := 
  (If[Lang == "Python", Delim = ", "; LBrace = "["; 
     RBrace = "]"]; If[Lang == "Mathematica", 
    Delim = ", "; LBrace = "{"; RBrace = "}"]; 
   If[Lang == "LaTeX", Delim = "\\;"; LBrace = "("; 
     RBrace = ")"]; If[Lang == "Display", 
    Delim = ""; LBrace = "("; RBrace = ")"]; 
   If[Lang == "Bare", Delim = ""; LBrace = ""; 
     RBrace = ""]; str = Str; If[str != "", 
    str = StringJoin[str, " = "]]; 
   bosons = Dimensions[P][[1]]; 
   fermions = Dimensions[P][[2]]; 
   If[bosons != fermions ||  !IntegerQ[Log2[fermions]], 
    If[str != "", str = StringJoin[str, " = "]]; 
     str = StringJoin[str, BracketRep[P, "", Lang]]; 
     Return[str]]; len = Log2[fermions]; 
   hs = Table[0, {hh, 1, len}]; 
   For[i = 1, i < fermions + 1, i++, 
    try = {1}; For[j = 1, j < len + 1, j++, 
      try = KroneckerProduct[try, \[Sigma]bs[[hs[[j]] + 1]]]]; 
     If[Abs[P] == try, str = StringJoin[str, LBrace, 
         DelimitWith[IntegerString[FromDigits[hs, 2], 2, 
           len], Delim], RBrace]; Return[str], 
      hs = AddOneInBinary[hs]]]; 
   str = StringJoin[str, BracketRep[P, "", Lang]]; 
   Return[str])
   
AddOneInBinary[dig2_] := (lendig2 = Length[dig2]; 
   If[dig2 == Table[1, {hhh, 1, lendig2}], 
    Return[Table[0, {hhh, 1, lendig2}]]]; 
   b10 = IntegerPart[dig2 . 
      2^(Reverse[Range[Length[dig2]]] - 1)]; b10 += 1; 
   dig2new = IntegerDigits[b10, 2, lendig2]; 
   Return[dig2new])


BracketRep[L_, Str_] := Return[BracketRep[L, Str, 
    "Display"]]
    
BracketRep[L_, Str_, Lang_] := 
  (Print["unsigned"];
  If[Lang == "Python", Delim = ", "; LBrace = "["; 
     RBrace = "]"]; If[Lang == "Mathematica", 
    Delim = ", "; LBrace = "{"; RBrace = "}"]; 
   If[Lang == "LaTeX", Delim = "\\;"; 
     LBrace = "\\langle"; RBrace = "\\rangle"]; 
   If[Lang == "Display", Delim = " "; LBrace = "("; 
     RBrace = ")"]; If[Lang == "Bare", 
    Delim = ""; LBrace = ""; RBrace = ""]; str = Str; 
   If[str != "", str = StringJoin[str, " = "]]; 
   P = Abs[L]; vec = Table[hh, 
     {hh, 1, Dimensions[L][[2]]}]; Pvec = P . vec; 
   oneline = LBrace; For[hh = 1, hh < Length[Pvec], hh++, 
    oneline = StringJoin[oneline, ToString[Pvec[[hh]]], 
      Delim]]; oneline = StringJoin[oneline, 
     ToString[Pvec[[Length[Pvec]]]], RBrace]; 
   str = StringJoin[str, oneline]; Return[str])
   


BooleanRep[B_, Str_] := Return[BooleanRep[B, Str, "Best", 
    "Display"]]
    
BooleanRep[B_, Str_, Lang_] := 
  Return[BooleanRep[B, Str, "Best", Lang]]
  
BooleanRep[B_, Str_, Form_, Lang_] := 
  (If[Lang == "Python", Delim = ", "; LBrace = "["; 
     RBrace = "]"]; If[Lang == "Mathematica", 
    Delim = ", "; LBrace = "{"; RBrace = "}"]; 
   If[Lang == "LaTeX", Delim = "\\;"; 
     LBrace = "\\langle"; RBrace = "\\rangle"]; 
   If[Lang == "Display", Delim = " "; LBrace = "("; 
     RBrace = ")"]; If[Form == "Best", 
    str = Str; If[str != "", str = StringJoin[str, 
        " = "]]; If[Abs[B] == IdentityMatrix[Length[B]], 
      vec = Table[1, {hh, 1, Length[B]}]; Bvec = vec . B; 
       Bvec = (-Bvec + vec)/2; Bfactor = StringJoin["B", 
         ToString[IntegerPart[Bvec . 
            2^(Reverse[Range[Length[Bvec]]] - 1)]]]; 
       Return[StringJoin[str, Bfactor]]]; 
     vec = Table[1, {hh, 1, Length[B]}]; Bvec = vec . B; 
     Boneline = LBrace; For[hh = 1, hh < Length[Bvec], 
      hh++, Boneline = StringJoin[Boneline, 
        ToString[Bvec[[hh]]], Delim]]; 
     Boneline = StringJoin[Boneline, 
       ToString[Bvec[[Length[Bvec]]]], RBrace]; 
     Return[StringJoin[str, Boneline]]])


FindHRs[Ls_] := 
  (HRs = Table[Abs[Transpose[Ls[[j]]] . Ls[[i]]], 
     {j, Dimensions[Ls][[1]]}, {i, Dimensions[Ls][[1]]}]; 
   Return[HRs])
   
FindHLs[Ls_] := 
  (HLs = Table[Abs[Ls[[i]] . Transpose[Ls[[j]]]], 
     {j, Dimensions[Ls][[1]]}, {i, Dimensions[Ls][[1]]}]; 
   Return[HLs])
   
FindBs[Ls_] := 
  (BBs = Table[Ls[[i]] . Abs[Transpose[Ls[[i]]]], 
     {i, Dimensions[Ls][[1]]}]; Return[BBs])


WeightOrder[Ls_] := (dim = Dimensions[Ls]; NL = dim[[1]]; 
   d1 = dim[[2]]; d2 = dim[[3]]; 
   LsWtd = Table[0, {i, 1, NL}]; AA = Table[0, {i, NL}]; 
   For[ii = 1, ii < NL + 1, ii++, 
    Vect = Table[a, {a, 1, d2}]; 
     Vect = Abs[Ls[[ii]]] . Vect; AA[[ii]] = 
      FromDigits[Flatten[Vect]]]; BB = Sort[AA]; 
   For[ii = 1, ii < NL + 1, ii++, LsWtd[[ii]] = 
     Ls[[Position[AA, BB[[ii]]][[1,1]]]]]; Return[LsWtd])
     
WeightTotal[Ls_] := (Return[Sum[FromDigits[MatrixToOneLine[Abs[Ls[[ii]]]]],{ii,1,Length[Ls]}]])


(* ::Section::Closed:: *)
(*Adinkra Maker*)


GraphVAdinkra[L\[LetterSpace]I_, R\[LetterSpace]I_]:= (Return[GraphVAdinkra[L\[LetterSpace]I, R\[LetterSpace]I,600]])

GraphVAdinkra[L\[LetterSpace]I_, R\[LetterSpace]I_, size_] := 
  (numFermions = Length[L\[LetterSpace]I[[1,1,1 ;; All]]]; 
   numBosons = Length[L\[LetterSpace]I[[1,1 ;; All,1]]]; 
   numColors = Length[L\[LetterSpace]I[[1 ;; All,1,1]]]; 
   If[numFermions > numBosons, numConnections = 
     numBosons, numConnections = numFermions]; 
   If[numFermions > numBosons, AdinkraWidth = 
     numFermions, AdinkraWidth = numBosons]; 
   xStartingPosFermions = (AdinkraWidth - numFermions)/2; 
   xStartingPosBosons = (AdinkraWidth - numBosons)/2; 
   AdinkraCoordinates = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}, {i, 1, 2}, {j, 1, 2}]; 
   Dashes = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}]; For[n = 1, 
    n < numColors + 1, n++, 
    h = 1; For[j = 1, j < numFermions + 1, j++, 
      For[i = 1, i < numBosons + 1, i++, 
       If[L\[LetterSpace]I[[n,i,j]] =!= 0 && h < numConnections + 1, 
        Dashes[[n,h]] = L\[LetterSpace]I[[n,i,j]]; 
         AdinkraCoordinates[[n,h,1,1]] = 
          i + xStartingPosBosons; AdinkraCoordinates[[n,h,
           1,2]] = 1; AdinkraCoordinates[[n,h,2,1]] = 
          j + xStartingPosFermions; 
         AdinkraCoordinates[[n,h,2,2]] = 2; h++]]]]; 
   AdinkraCoordinatesSolid = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}, {i, 1, 2}, {j, 1, 2}]; 
   AdinkraCoordinatesDashed = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}, {i, 1, 2}, {j, 1, 2}]; 
   For[n = 1, n < numColors + 1, n++, 
    For[h = 1, h < numConnections + 1, h++, 
     If[Dashes[[n,h]] == 1, AdinkraCoordinatesSolid[[n,
        h]] = AdinkraCoordinates[[n,h]], 
      AdinkraCoordinatesDashed[[n,h]] = 
       AdinkraCoordinates[[n,h]]]]]; 
   GColors = {Hue[0], Hue[0.1], Hue[0.15], Hue[0.3], 
     Hue[0.5], Hue[0.55], Hue[0.6], Hue[0.75], 
     Hue[0.85]}; GabesColors = {Hue[0], Hue[0.15], 
     Hue[0.3], Hue[0.55]}; g = Table[0, {d, 1, 2}, 
     {n, 1, numColors}]; For[n = 1, n < numColors + 1, 
    n++, g[[1,n]] = ListLinePlot[AdinkraCoordinatesSolid[[
        n,1 ;; All,1 ;; All,1 ;; All]], 
       PlotStyle -> Directive[Hue[n/numColors]], ImageSize -> size]; 
     g[[2,n]] = ListLinePlot[AdinkraCoordinatesDashed[[n,
        1 ;; All,1 ;; All,1 ;; All]], PlotStyle -> 
        Directive[Hue[n/numColors], Dashed]]]; 
   BVertices = ListPlot[Table[{i + xStartingPosBosons, 
       1}, {i, 1, numBosons}], PlotMarkers -> 
      {Graphics[{EdgeForm[Directive[Thick, Black]], 
         White, Disk[]}], 0.05}]; 
   FVertices = ListPlot[Table[{i + xStartingPosFermions, 
       2}, {i, 1, numFermions}], LabelStyle -> White, 
     PlotMarkers -> {Graphics[{Black, Disk[]}], 0.05}]; 
   Return[Show[g[[1,1 ;; All]], g[[2,1 ;; All]], 
     BVertices, FVertices, PlotRange -> 
      {{0.1, AdinkraWidth + 1}, {0.9, 2.1}}, 
     Axes -> False]])

GraphBigVAdinkra[L\[LetterSpace]I_, R\[LetterSpace]I_]:= (Return[GraphBigVAdinkra[L\[LetterSpace]I, R\[LetterSpace]I,600]])

GraphBigVAdinkra[L\[LetterSpace]I_, R\[LetterSpace]I_, size_] := 
  (numFermions = Length[L\[LetterSpace]I[[1,1,1 ;; All]]]; 
   numBosons = Length[L\[LetterSpace]I[[1,1 ;; All,1]]]; 
   numColors = Length[L\[LetterSpace]I[[1 ;; All,1,1]]]; 
   If[numFermions > numBosons, numConnections = 
     numBosons, numConnections = numFermions]; 
   If[numFermions > numBosons, AdinkraWidth = 
     numFermions, AdinkraWidth = numBosons]; 
   xStartingPosFermions = (AdinkraWidth - numFermions)/2; 
   xStartingPosBosons = (AdinkraWidth - numBosons)/2; 
   AdinkraCoordinates = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}, {i, 1, 2}, {j, 1, 2}]; 
   Dashes = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}]; For[n = 1, 
    n < numColors + 1, n++, 
    h = 1; For[j = 1, j < numFermions + 1, j++, 
      For[i = 1, i < numBosons + 1, i++, 
       If[L\[LetterSpace]I[[n,i,j]] =!= 0 && h < numConnections + 1, 
        Dashes[[n,h]] = L\[LetterSpace]I[[n,i,j]]; 
         AdinkraCoordinates[[n,h,1,1]] = 
          i + xStartingPosBosons; AdinkraCoordinates[[n,h,
           1,2]] = 1; AdinkraCoordinates[[n,h,2,1]] = 
          j + xStartingPosFermions; 
         AdinkraCoordinates[[n,h,2,2]] = 2; h++]]]]; 
   AdinkraCoordinatesSolid = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}, {i, 1, 2}, {j, 1, 2}]; 
   AdinkraCoordinatesDashed = Table[0, {n, 1, numColors}, 
     {h, 1, numConnections}, {i, 1, 2}, {j, 1, 2}]; 
   For[n = 1, n < numColors + 1, n++, 
    For[h = 1, h < numConnections + 1, h++, 
     If[Dashes[[n,h]] == 1, AdinkraCoordinatesSolid[[n,
        h]] = AdinkraCoordinates[[n,h]], 
      AdinkraCoordinatesDashed[[n,h]] = 
       AdinkraCoordinates[[n,h]]]]]; 
   g = Table[0, {d, 1, 2}, {n, 1, numColors}]; 
   For[n = 1, n < numColors + 1, n++, 
    g[[1,n]] = ListLinePlot[AdinkraCoordinatesSolid[[n,
        1 ;; All,1 ;; All,1 ;; All]], PlotStyle -> 
        Hue[n/numColors], ImageSize -> size]; 
     g[[2,n]] = ListLinePlot[AdinkraCoordinatesDashed[[n,
        1 ;; All,1 ;; All,1 ;; All]], PlotStyle -> 
        Directive[Hue[n/numColors], Dashed]]]; 
   BVertices = BVertices = ListPlot[
      Table[{i + xStartingPosBosons, 1}, 
       {i, 1, numBosons}], PlotMarkers -> 
       {Graphics[{EdgeForm[Directive[Thin, Black]], 
          White, Disk[]}], 0.01}]; 
   FVertices = ListPlot[Table[{i + xStartingPosFermions, 
       2}, {i, 1, numFermions}], PlotMarkers -> 
      {Graphics[{Black, Disk[]}], 0.01}]; 
   Return[Show[g[[1,1 ;; All]], g[[2,1 ;; All]], 
     BVertices, FVertices, PlotRange -> 
      {{0.1, AdinkraWidth + 1}, {0.9, 2.1}}, 
     Axes -> False]])


(* ::Section::Closed:: *)
(*Callable Adinkra Matrices*)


(*Spinning particle multiplet with N=2,4,8,9,10,12,16*)
LRMatricesSP[nn_] := (L2SP = {I*\[Sigma]2, -\[Sigma]0}; 
   R2SP = Table[Transpose[L2SP[[k]]], {k, 1, 2}]; 
   L4SP = {KroneckerProduct[I*\[Sigma]1, \[Sigma]2], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]0], KroneckerProduct[
      (-I)*\[Sigma]3, \[Sigma]2], KroneckerProduct[\[Sigma]0, \[Sigma]0]}; 
   R4SP = Table[Transpose[L4SP[[k]]], {k, 1, 4}]; 
   L8SP = {KroneckerProduct[I*\[Sigma]0, \[Sigma]3, \[Sigma]2], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]2, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]0, \[Sigma]1, \[Sigma]2], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]3], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]2, \[Sigma]2], 
     KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0]}; 
   R8SP = Table[Transpose[L8SP[[k]]], {k, 1, 8}]; 
   L9SP = {KroneckerProduct[I*\[Sigma]0, \[Sigma]3, \[Sigma]2, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]2, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]0, \[Sigma]1, \[Sigma]2, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]1, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]3, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]2, \[Sigma]2, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]3], 
     KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0]}; 
   R9SP = Table[Transpose[L9SP[[k]]], {k, 1, 9}]; 
   L10SP = {KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]1, \[Sigma]3, \[Sigma]2, \[Sigma]0, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]1, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]3, \[Sigma]0], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]1], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]2, \[Sigma]2], 
     KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]3], 
     KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0]}; 
   R10SP = Table[Transpose[L10SP[[k]]], {k, 1, 10}]; 
   L12SP = {KroneckerProduct[I*\[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0, \[Sigma]0, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]1, \[Sigma]1, \[Sigma]2, \[Sigma]0, \[Sigma]0, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]1, \[Sigma]3, \[Sigma]2, \[Sigma]0, \[Sigma]0, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]1, \[Sigma]0, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]3, \[Sigma]0, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]1, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]3, 
      \[Sigma]0], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]0, \[Sigma]2, \[Sigma]2, 
      \[Sigma]2], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]2, \[Sigma]2, 
      \[Sigma]1], KroneckerProduct[I*\[Sigma]3, \[Sigma]0, \[Sigma]2, \[Sigma]2, \[Sigma]2, 
      \[Sigma]3], KroneckerProduct[\[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, \[Sigma]0, 
      \[Sigma]0]}; R12SP = Table[Transpose[L12SP[[k]]], 
     {k, 1, 12}]; L16SP = Table[0, {i, 1, 16}, 
     {j, 1, 128}, {k, 1, 128}]; L16SP[[1]] = 
    I*KroneckerProduct[\[Sigma]2, IdentityMatrix[64]]; 
   For[i = 1, i < 8, i++, L16SP[[i + 1]] = 
     KroneckerProduct[\[Sigma]3, L8SP[[i]], IdentityMatrix[
       8]]]; For[i = 1, i < 8, i++, L16SP[[i + 8]] = 
     KroneckerProduct[\[Sigma]1, IdentityMatrix[8], 
      L8SP[[i]]]]; L16SP[[16]] = IdentityMatrix[128]; 
   R16SP = Table[Transpose[L16SP[[k]]], {k, 1, 16}]; 
   If[nn == 2, Return[{L2SP, R2SP}]]; 
   If[nn == 4, Return[{L4SP, R4SP}]]; 
   If[nn == 8, Return[{L8SP, R8SP}]]; 
   If[nn == 16, Return[{L16SP, R16SP}]]; 
   If[nn == 9, Return[{L9SP, R9SP}]]; 
   If[nn == 10, Return[{L10SP, R10SP}]]; 
   If[nn == 12, Return[{L12SP, R12SP}]])


LMatricesSM4D[] := 
  (OneLine = {{1, -4, 5, -8, 9, -12, 14, -16, 13}, 
     {2, 3, 6, 7, 10, 11, 13, 15, -14}, 
     {3, -2, 7, -6, 11, -10, 16, 14, 15}, 
     {4, 1, 8, 5, 12, 9, 15, -13, -16}, 
     {-5, -8, 1, 4, 13, -16, -10, 12, -9}, 
     {-6, 7, 2, -3, 14, 15, -9, -11, 10}, 
     {-7, -6, 3, 2, 15, -14, -12, -10, -11}, 
     {-8, 5, 4, -1, 16, 13, -11, 9, 12}, 
     {9, 12, 13, -16, -1, -4, -6, 8, -5}, 
     {10, -11, 14, 15, -2, 3, -5, -7, 6}, 
     {11, 10, 15, -14, -3, -2, -8, -6, -7}, 
     {12, -9, 16, 13, -4, 1, -7, 5, 8}, 
     {13, -16, -9, -12, 5, 8, -2, 4, -1}, 
     {14, 15, -10, 11, 6, -7, -1, -3, 2}, 
     {15, -14, -11, -10, 7, 6, -4, -2, -3}, 
     {16, 13, -12, 9, 8, -5, -3, 1, 4}}; 
   ListMatrices = {}; Num = Length[OneLine]; 
   L = Length[OneLine[[1]]]; For[i = 1, i <= Num, i++, 
    AppendTo[ListMatrices, Normal[SparseArray[
        Table[{j, Abs[OneLine[[i]][[j]]]} -> 
          Sign[OneLine[[i]][[j]]]*1, {j, 1, L}]]]]; ]; 
   Return[ListMatrices])
   
LMatricesSM10D[] := 
  (OneLine = {{1, -3, 4, -2, 6, -5, 7, 8, 16}, 
     {-2, -4, -3, -1, 5, 6, -8, 7, 15}, 
     {-3, -1, 2, 4, -8, -7, -5, 6, 14}, 
     {4, -2, -1, 3, -7, 8, 6, 5, 13}, {-5, 7, -8, -6, -2, 
      -1, 3, 4, 12}, {6, 8, 7, -5, -1, 2, -4, 3, 11}, 
     {7, 5, -6, 8, 4, -3, -1, 2, 10}, {-8, 6, 5, 7, 3, 4, 
      2, 1, 9}, {-9, 11, -12, 10, -14, 13, -15, 16, 8}, 
     {10, 12, 11, 9, -13, -14, 16, 15, 7}, 
     {11, 9, -10, -12, 16, 15, 13, 14, 6}, 
     {-12, 10, 9, -11, 15, -16, -14, 13, 5}, 
     {13, -15, 16, 14, 10, 9, -11, 12, 4}, 
     {-14, -16, -15, 13, 9, -10, 12, 11, 3}, 
     {-15, -13, 14, -16, -12, 11, 9, 10, 2}, 
     {16, -14, -13, -15, -11, -12, -10, 9, 1}}; 
   ListMatrices = {}; Num = Length[OneLine]; 
   L = Length[OneLine[[1]]]; For[i = 1, i <= Num, i++, 
    AppendTo[ListMatrices, Normal[SparseArray[
        Table[{j, Abs[OneLine[[i]][[j]]]} -> 
          Sign[OneLine[[i]][[j]]]*1, {j, 1, L}]]]]; ]; 
   Return[ListMatrices])
   
LMatricesSM4DValise[] := 
  (OneLine = {{1, -4, 2, -3, 5, -8, 6, -7, 9, -12, 10, 
      -11, 14, -16, 13, -15}, {2, 3, -1, -4, 6, 7, -5, 
      -8, 10, 11, -9, -12, 13, 15, -14, -16}, 
     {3, -2, -4, 1, 7, -6, -8, 5, 11, -10, -12, 9, 16, 
      14, 15, 13}, {4, 1, 3, 2, 8, 5, 7, 6, 12, 9, 11, 
      10, 15, -13, -16, 14}, {-5, -8, -6, -7, 1, 4, 2, 3, 
      13, -16, 14, 15, -10, 12, -9, -11}, 
     {-6, 7, 5, -8, 2, -3, -1, 4, 14, 15, -13, 16, -9, 
      -11, 10, -12}, {-7, -6, 8, 5, 3, 2, -4, -1, 15, 
      -14, -16, -13, -12, -10, -11, 9}, 
     {-8, 5, -7, 6, 4, -1, 3, -2, 16, 13, 15, -14, -11, 
      9, 12, 10}, {9, 12, 10, 11, 13, -16, 14, 15, -1, 
      -4, -2, -3, -6, 8, -5, -7}, {10, -11, -9, 12, 14, 
      15, -13, 16, -2, 3, 1, -4, -5, -7, 6, -8}, 
     {11, 10, -12, -9, 15, -14, -16, -13, -3, -2, 4, 1, 
      -8, -6, -7, 5}, {12, -9, 11, -10, 16, 13, 15, -14, 
      -4, 1, -3, 2, -7, 5, 8, 6}, {13, -16, 14, 15, -9, 
      -12, -10, -11, 5, 8, 6, 7, -2, 4, -1, -3}, 
     {14, 15, -13, 16, -10, 11, 9, -12, 6, -7, -5, 8, -1, 
      -3, 2, -4}, {15, -14, -16, -13, -11, -10, 12, 9, 7, 
      6, -8, -5, -4, -2, -3, 1}, {16, 13, 15, -14, -12, 
      9, -11, 10, 8, -5, 7, -6, -3, 1, 4, 2}}; 
   ListMatrices = {}; Num = Length[OneLine]; 
   L = Length[OneLine[[1]]]; For[i = 1, i <= Num, i++, 
    AppendTo[ListMatrices, Normal[SparseArray[
        Table[{j, Abs[OneLine[[i]][[j]]]} -> 
          Sign[OneLine[[i]][[j]]]*1, {j, 1, L}]]]]; ]; 
   Return[ListMatrices])
   
LMatricesVT4DValise[] := 
  (OneLine = {{1, -4, 2, -3, 6, -8, 5, -7, 9, -12, 10, 
      -11, -15, -16, -14, 13}, {2, 3, -1, -4, 5, 7, -6, 
      -8, 10, 11, -9, -12, 16, -15, 13, 14}, 
     {3, -2, -4, 1, 8, 6, 7, 5, 11, -10, -12, 9, 13, 14, 
      -16, 15}, {4, 1, 3, 2, 7, -5, -8, 6, 12, 9, 11, 10, 
      -14, 13, 15, 16}, {5, -8, 6, 7, -2, 4, -1, -3, -13, 
      -16, 14, -15, 11, 12, 10, 9}, {6, 7, -5, 8, -1, -3, 
      2, -4, -14, 15, -13, -16, -12, 11, -9, 10}, 
     {7, -6, -8, -5, -4, -2, -3, 1, -15, -14, -16, 13, 
      -9, -10, 12, 11}, {8, 5, 7, -6, -3, 1, 4, 2, -16, 
      13, 15, 14, 10, -9, -11, 12}, {-9, 12, -10, -11, 
      14, -16, 13, -15, -1, 4, -2, 3, -7, -8, -6, -5}, 
     {-10, -11, 9, -12, 13, 15, -14, -16, -2, -3, 1, 4, 
      8, -7, 5, -6}, {-11, 10, 12, 9, 16, 14, 15, 13, -3, 
      2, 4, -1, 5, 6, -8, -7}, {-12, -9, -11, 10, 15, 
      -13, -16, 14, -4, -1, -3, -2, -6, 5, 7, -8}, 
     {13, -16, 14, -15, 10, -12, 9, 11, -5, -8, 6, -7, 
      -3, -4, -2, 1}, {14, 15, -13, -16, 9, 11, -10, 12, 
      -6, 7, -5, -8, 4, -3, 1, 2}, {15, -14, -16, 13, 12, 
      10, 11, -9, -7, -6, -8, 5, 1, 2, -4, 3}, 
     {16, 13, 15, 14, 11, -9, -12, -10, -8, 5, 7, 6, -2, 
      1, 3, 4}}; ListMatrices = {}; 
   Num = Length[OneLine]; L = Length[OneLine[[1]]]; 
   For[i = 1, i <= Num, i++, AppendTo[ListMatrices, 
      Normal[SparseArray[Table[
         {j, Abs[OneLine[[i]][[j]]]} -> 
          Sign[OneLine[[i]][[j]]]*1, {j, 1, L}]]]]; ]; 
   Return[ListMatrices])


\[CapitalGamma]s[Ls_, Rs_]:= Table[KroneckerProduct[{{0,1},{0,0}}, Ls[[i]]] +  KroneckerProduct[{{0,0},{1,0}}, Rs[[i]]],{i,1,Length[Ls]}];

GRdN[Ls_, Rs_] := 
  (\[CapitalGamma] = \[CapitalGamma]s[Ls,Rs]; 
   RHS = Table[2*IdentityMatrix[Dimensions[\[CapitalGamma]][[2]]]*
      IdentityMatrix[Dimensions[\[CapitalGamma]][[1]]][[i,j]], 
     {i, 1, Length[Ls]}, {j, 1, Length[Ls]}]; 
   Return[GRdN\[CapitalGamma][\[CapitalGamma], RHS]])
   
GRdN\[CapitalGamma][\[CapitalGamma]_]:= GRdN\[CapitalGamma][\[CapitalGamma], Table[2*IdentityMatrix[Dimensions[\[CapitalGamma]][[2]]]*
      IdentityMatrix[Dimensions[\[CapitalGamma]][[1]]][[i,j]], 
     {i, 1, Length[\[CapitalGamma]]}, {j, 1, Length[\[CapitalGamma]]}]]
     
GRdN\[CapitalGamma][\[CapitalGamma]_, RHS_] := (Ncolors = Length[\[CapitalGamma]]; 
   d = Dimensions[\[CapitalGamma]][[2]]; 
   bool = Table[\[CapitalGamma][[i]] . \[CapitalGamma][[j]] + \[CapitalGamma][[j]] . \[CapitalGamma][[i]], 
      {i, 1, Dimensions[\[CapitalGamma]][[1]]}, 
      {j, 1, Dimensions[\[CapitalGamma]][[1]]}] == RHS[[1 ;; Ncolors,
      1 ;; Ncolors,1 ;; d,1 ;; d]]; Return[bool])


(* ::Section::Closed:: *)
(*Cube Quotienting Graphics*)


PlotTesseractAdinkra[] := 
  (Nodes = Table[IntegerDigits[hh, 2, 4], {hh, 0, 15}]; 
   NodeLabels = Table[StringJoin["(", IntegerString[hh, 
       2, 4], ")"], {hh, 0, 15}]; 
   Points = Table[Nodes[[hh,2 ;; All]], {hh, 1, 16}]; 
   Points -= Table[{0.5, 0.5, 0.5}, {hh, 1, 16}]; 
   Points[[1 ;; 8]] *= Table[{3, 3, 3}, {hh, 1, 8}]; 
   Edges = {UndirectedEdge[1, 2], UndirectedEdge[1, 3], 
     UndirectedEdge[1, 5], UndirectedEdge[1, 9], 
     UndirectedEdge[2, 4], UndirectedEdge[2, 6], 
     UndirectedEdge[2, 10], UndirectedEdge[3, 4], 
     UndirectedEdge[3, 7], UndirectedEdge[3, 11], 
     UndirectedEdge[4, 8], UndirectedEdge[4, 12], 
     UndirectedEdge[5, 6], UndirectedEdge[5, 7], 
     UndirectedEdge[5, 13], UndirectedEdge[6, 8], 
     UndirectedEdge[6, 14], UndirectedEdge[7, 8], 
     UndirectedEdge[7, 15], UndirectedEdge[8, 16], 
     UndirectedEdge[9, 10], UndirectedEdge[9, 11], 
     UndirectedEdge[9, 13], UndirectedEdge[10, 12], 
     UndirectedEdge[10, 14], UndirectedEdge[11, 12], 
     UndirectedEdge[11, 15], UndirectedEdge[12, 16], 
     UndirectedEdge[13, 14], UndirectedEdge[13, 15], 
     UndirectedEdge[14, 16], UndirectedEdge[15, 16]}; 
   Bosons = {1, 4, 6, 7, 10, 11, 13, 16}; 
   Fermions = Complement[Table[hhi, {hhi, 16}], Bosons]; 
   DashedEdges = {UndirectedEdge[1, 9], 
     UndirectedEdge[4, 12], UndirectedEdge[5, 13], 
     UndirectedEdge[8, 16], UndirectedEdge[2, 4], 
     UndirectedEdge[5, 7], UndirectedEdge[10, 12], 
     UndirectedEdge[9, 13], UndirectedEdge[13, 15], 
     UndirectedEdge[2, 6], UndirectedEdge[4, 8], 
     UndirectedEdge[10, 12], UndirectedEdge[11, 15]}; 
   SolidEdges = Complement[Edges, DashedEdges]; 
   ColoredSolidLines = Table[{}, {i, 1, 4}]; 
   ColoredDashedLines = Table[{}, {i, 1, 4}]; 
   For[hi = 1, hi < Length[SolidEdges] + 1, hi++, 
    ColoredSolidLines[[1 + Log2[SolidEdges[[hi,2]] - 
         SolidEdges[[hi,1]]]]] = 
     Append[ColoredSolidLines[[
       1 + Log2[SolidEdges[[hi,2]] - SolidEdges[[hi,
           1]]]]], {Points[[SolidEdges[[hi,1]]]], 
       Points[[SolidEdges[[hi,2]]]]}]]; 
   For[hi = 1, hi < Length[DashedEdges] + 1, hi++, 
    ColoredDashedLines[[1 + Log2[DashedEdges[[hi,2]] - 
         DashedEdges[[hi,1]]]]] = 
     Append[ColoredDashedLines[[
       1 + Log2[DashedEdges[[hi,2]] - DashedEdges[[hi,
           1]]]]], {Points[[DashedEdges[[hi,1]]]], 
       Points[[DashedEdges[[hi,2]]]]}]]; 
   WhiteNodes = Graphics3D[
     Table[{Sphere[Points[[Bosons[[i]]]], 0.1]}, {i, 8}], 
     Lighting -> "Neutral", Boxed -> False]; 
   BlackNodes = Graphics3D[
     Table[{Black, Sphere[Points[[Fermions[[i]]]], 0.1]}, 
      {i, 8}], Lighting -> "Neutral", Boxed -> False]; 
   Dashed1 = Table[Graphics3D[{Black, DashedTube[
        ColoredDashedLines[[1,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[1]]]}]; 
   Dashed2 = Table[Graphics3D[{Red, DashedTube[
        ColoredDashedLines[[2,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[2]]]}]; 
   Dashed3 = Table[Graphics3D[{Green, DashedTube[
        ColoredDashedLines[[3,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[3]]]}]; 
   Dashed4 = Table[Graphics3D[{Blue, DashedTube[
        ColoredDashedLines[[4,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[4]]]}]; 
   Solid1 = Table[Graphics3D[{Black, 
       Tube[ColoredSolidLines[[1,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[1]]]}]; 
   Solid2 = Table[Graphics3D[
      {Red, Tube[ColoredSolidLines[[2,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[2]]]}]; 
   Solid3 = Table[Graphics3D[{Green, 
       Tube[ColoredSolidLines[[3,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[3]]]}]; 
   Solid4 = Table[Graphics3D[{Blue, 
       Tube[ColoredSolidLines[[4,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[4]]]}]; 
   Show[{WhiteNodes, BlackNodes, Dashed1, Dashed2, 
     Dashed3, Dashed4, Solid1, Solid2, Solid3, Solid4}, 
    Axes -> False, ImageSize -> {500}, 
    BaseStyle -> {FontWeight -> "Bold", FontSize -> 11}])
    
PlotProjectedTesseractAdinkra[] := 
  (Nodes = Table[IntegerDigits[hh, 2, 3], {hh, 0, 7}]; 
   NodeLabels = Table[StringJoin["(", IntegerString[hh, 
       2, 3], ")"], {hh, 0, 7}]; 
   Points = 2*Table[Nodes[[hh,1 ;; All]], {hh, 1, 8}]; 
   Edges = {UndirectedEdge[1, 2], UndirectedEdge[1, 3], 
     UndirectedEdge[1, 5], UndirectedEdge[1, 8], 
     UndirectedEdge[2, 4], UndirectedEdge[2, 6], 
     UndirectedEdge[2, 7], UndirectedEdge[3, 4], 
     UndirectedEdge[3, 6], UndirectedEdge[3, 7], 
     UndirectedEdge[4, 8], UndirectedEdge[4, 5], 
     UndirectedEdge[5, 6], UndirectedEdge[5, 7], 
     UndirectedEdge[6, 8], UndirectedEdge[7, 8]}; 
   Bosons = {1, 4, 6, 7}; Fermions = 
    Complement[Table[hhi, {hhi, 8}], Bosons]; 
   DashedEdges = {UndirectedEdge[1, 8], 
     UndirectedEdge[2, 4], UndirectedEdge[4, 5], 
     UndirectedEdge[5, 7], UndirectedEdge[2, 6], 
     UndirectedEdge[4, 8]}; SolidEdges = 
    Complement[Edges, DashedEdges]; 
   NewEdges = {UndirectedEdge[1, 8], UndirectedEdge[2, 
      7], UndirectedEdge[3, 6], UndirectedEdge[4, 5]}; 
   ColoredSolidLines = Table[{}, {i, 1, 4}]; 
   ColoredDashedLines = Table[{}, {i, 1, 4}]; 
   For[hi = 1, hi < Length[SolidEdges] + 1, hi++, 
    If[ContainsAll[NewEdges, {SolidEdges[[hi]]}], 
     ColoredSolidLines[[4]] = Append[ColoredSolidLines[[
        4]], {Points[[SolidEdges[[hi,1]]]], 
        Points[[SolidEdges[[hi,2]]]]}], 
     ColoredSolidLines[[1 + Log2[SolidEdges[[hi,2]] - 
          SolidEdges[[hi,1]]]]] = 
      Append[ColoredSolidLines[[
        1 + Log2[SolidEdges[[hi,2]] - SolidEdges[[hi,
            1]]]]], {Points[[SolidEdges[[hi,1]]]], 
        Points[[SolidEdges[[hi,2]]]]}]]]; 
   For[hi = 1, hi < Length[DashedEdges] + 1, hi++, 
    If[ContainsAll[NewEdges, {DashedEdges[[hi]]}], 
     ColoredDashedLines[[4]] = Append[ColoredDashedLines[[
        4]], {Points[[DashedEdges[[hi,1]]]], 
        Points[[DashedEdges[[hi,2]]]]}], 
     ColoredDashedLines[[1 + Log2[DashedEdges[[hi,2]] - 
          DashedEdges[[hi,1]]]]] = 
      Append[ColoredDashedLines[[
        1 + Log2[DashedEdges[[hi,2]] - DashedEdges[[hi,
            1]]]]], {Points[[DashedEdges[[hi,1]]]], 
        Points[[DashedEdges[[hi,2]]]]}]]]; 
   WhiteNodes = Graphics3D[
     Table[{Sphere[Points[[Bosons[[i]]]], 0.1]}, 
      {i, Length[Bosons]}], Lighting -> "Neutral", 
     Boxed -> False]; BlackNodes = 
    Graphics3D[Table[{Black, Sphere[Points[[
         Fermions[[i]]]], 0.1]}, {i, Length[Fermions]}], 
     Lighting -> "Neutral", Boxed -> False]; 
   Dashed1 = Table[Graphics3D[{Black, DashedTube[
        ColoredDashedLines[[1,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[1]]]}]; 
   Dashed2 = Table[Graphics3D[{Red, DashedTube[
        ColoredDashedLines[[2,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[2]]]}]; 
   Dashed3 = Table[Graphics3D[{Green, DashedTube[
        ColoredDashedLines[[3,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[3]]]}]; 
   Dashed4 = Table[Graphics3D[{Blue, DashedTube[
        ColoredDashedLines[[4,i]], 0.03, 0.1]}], 
     {i, Length[ColoredDashedLines[[4]]]}]; 
   Solid1 = Table[Graphics3D[{Black, 
       Tube[ColoredSolidLines[[1,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[1]]]}]; 
   Solid2 = Table[Graphics3D[
      {Red, Tube[ColoredSolidLines[[2,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[2]]]}]; 
   Solid3 = Table[Graphics3D[{Green, 
       Tube[ColoredSolidLines[[3,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[3]]]}]; 
   Solid4 = Table[Graphics3D[{Blue, 
       Tube[ColoredSolidLines[[4,i]], 0.03]}], 
     {i, Length[ColoredSolidLines[[4]]]}]; 
   Show[{WhiteNodes, BlackNodes, Dashed1, Dashed2, 
     Dashed3, Dashed4, Solid1, Solid2, Solid3, Solid4}, 
    Axes -> False, ImageSize -> {500}, 
    BaseStyle -> {FontWeight -> "Bold", FontSize -> 11}])


(* ::Section::Closed:: *)
(*NCube Quotienting:*)


DecodeNCube[CodeGenerator_] := 
  ({Adjacenecy, AdjacencyM, AdjEigenVals, DistanceM, DistEigenVals, 
     LMatrices} = DecodeNCube[CodeGenerator, False]; 
   Return[{Adjacenecy, AdjacencyM, AdjEigenVals, DistanceM, 
     DistEigenVals, LMatrices}])
     
GenerateCode[CodeGenerator_] := 
  (Colors = Dimensions[CodeGenerator][[2]]; 
   Len = Length[CodeGenerator]; If[2^Colors > 10000, 
    If[Sure == True, Continue, Return["Are You Sure?"]]]; 
   Combinations = Subsets[CodeGenerator]; 
   Combinations[[1]] = {Table[0, {i, Colors}]}; 
   Codes = Table[BitXorList[Combinations[[i]]][[1]], 
     {i, 1, Length[Combinations]}]; Return[Codes])
     
DecodeNCube[CodeGenerator_, Sure_] := 
  (Colors = Dimensions[CodeGenerator][[2]]; 
   Len = Length[CodeGenerator]; If[2^Colors > 10000, 
    If[Sure == True, Continue, Return["Are You Sure?"]]]; 
   Combinations = Subsets[CodeGenerator]; 
   Combinations[[1]] = {Table[0, {i, Colors}]}; 
   Codes = Table[BitXorList[Combinations[[i]]][[1]], 
     {i, 1, Length[Combinations]}]; 
   Codes = Reverse[Sort[Codes]]; 
   NCube = Table[IntegerDigits[i, 2, Colors], 
     {i, 0, 2^Colors - 1}]; CodePartitions = 
    Table[{}, {i, 1, 2^(Colors - Len)}]; 
   NCubeHolder = NCube; h = 1; 
   While[h < 2^(Colors - Len) + 1, 
    For[j = 1, j < Length[Codes] + 1, j++, 
      AdjointNode = BitXor[NCubeHolder[[1]], Codes[[j]]]; 
       CodePartitions[[h]] = Append[CodePartitions[[h]], 
         AdjointNode]; NCubeHolder = PopFirst[
         NCubeHolder, AdjointNode]]; h++]; 
   Print["Partitioned Nodes: ", Dimensions[
     CodePartitions]]; Print["Missed Nodes: ", 
    Dimensions[NCubeHolder]]; Connections = 
    Table[IntegerDigits[2^i, 2, Colors], 
     {i, 0, Colors - 1}]; Adjacency = 
    Table[0, {ii, 1, Colors}, {i, 1, 2^(Colors - Len)}, 
     {j, 1, 2^(Colors - Len)}]; 
   For[i = 1, i < Length[CodePartitions] + 1, i++, 
    CodePartitions[[i]] = Sort[CodePartitions[[i]]]]; 
   For[i = 1, i < Length[CodePartitions] + 1, i++, 
    For[j = 1, j < Length[Connections] + 1, j++, 
     Adjacency[[j,i,Position[CodePartitions, 
         Sort[BitXorMapToList[CodePartitions[[i]], 
           Connections[[j]]]]][[1,1]]]] = 1]]; 
   AdjacencyM = Sum[Adjacency[[i]], {i, 1, Colors}]; 
   AdjEigenVals = Eigenvalues[AdjacencyM]; 
   G = AdjacencyGraph[AdjacencyM]; 
   DistanceM = GraphDistanceMatrix[G]; 
   DistEigenVals = Eigenvalues[DistanceM]; 
   Bosons = FindIndependentVertexSet[G, 
      {2^(Colors - Len - 1)}][[1]]; 
   Fermions = Complement[Table[i, 
      {i, 1, 2^(Colors - Len)}], Bosons]; 
   Reassignment = OneLineToMatrix[
     Flatten[{Bosons, Fermions}]]; Adjacencynew = 
    Table[Reassignment . Adjacency[[i]] . 
      Transpose[Reassignment], {i, 1, Colors}]; 
   AdjacencyMnew = Reassignment . AdjacencyM . 
     Transpose[Reassignment]; DistanceMnew = 
    GraphDistanceMatrix[AdjacencyGraph[AdjacencyMnew]]; 
   LMatrices = Table[Adjacencynew[[i,
      1 ;; 2^(Colors - Len - 1),2^(Colors - Len - 1) + 
        1 ;; 2^(Colors - Len)]], {i, 1, Colors}]; 
   Return[{Adjacency, AdjacencyM, AdjEigenVals, 
     DistanceM, DistEigenVals, LMatrices}])


(* ::Section::Closed:: *)
(*N-Color Decomposition*)


AllSubGraphs[Hs_, n_] := (ConnectedA = {}; ConnectedD = {}; 
   NotConnectedA = {}; Combinations = 
    Subsets[Table[i, {i, 1, Length[Hs]}], {n}]; 
   For[i = 1, i < Length[Combinations] + 1, i++, 
    HsN = Table[Hs[[Combinations[[i,j]]]], {j, 1, n}]; 
     AdjacencyM = Sum[KroneckerProduct[(\[Sigma]1 + I*\[Sigma]2)/2, HsN[[j]]] + 
        KroneckerProduct[(\[Sigma]1 - I*\[Sigma]2)/2, Transpose[HsN[[j]]]], 
       {j, 1, n}]; graph = AdjacencyGraph[AdjacencyM]; 
     If[ConnectedGraphQ[graph] == True, 
      ConnectedA = Append[ConnectedA, AdjacencyM]; 
       ConnectedD = Append[ConnectedD, GraphDistanceMatrix[graph]], 
      NotConnectedA = Append[NotConnectedA, AdjacencyM]]]; 
   CeigsA = Table[Eigenvalues[ConnectedA[[i]]], 
     {i, 1, Length[ConnectedA]}]; CeigsA = DeleteDuplicates[CeigsA]; 
   NCeigsA = Table[Eigenvalues[NotConnectedA[[i]]], 
     {i, 1, Length[NotConnectedA]}]; 
   NCeigsA = DeleteDuplicates[NCeigsA]; 
   CeigsD = Table[Eigenvalues[ConnectedD[[i]]], 
     {i, 1, Length[ConnectedD]}]; CeigsD = DeleteDuplicates[CeigsD]; 
   Print["Connected Graphs: ", Dimensions[ConnectedA], 
    "\nAdjacency Spectra: ", CeigsA, "\nDistance Spectra: ", CeigsD, 
    "\nDisconnected Graphs: ", Dimensions[NotConnectedA], 
    "\nAdjacency Spectra: ", NCeigsA]; 
   Return[{ConnectedA, CeigsA, CeigsD, NotConnectedA, NCeigsA}])
   
AllSubGraphsEfficient[Hs_, n_] := (Connected = {}; NotConnected = {}; 
   Combinations = Subsets[Table[i, {i, 1, Length[Hs]}], {n}]; 
   Topologies = {}; For[i = 1, i < Length[Combinations] + 1, i++, 
    HsN = Table[Hs[[Combinations[[i,j]]]], {j, 1, n}]; 
     AdjacencyM = Sum[KroneckerProduct[(\[Sigma]1 + I*\[Sigma]2)/2, HsN[[j]]] + 
        KroneckerProduct[(\[Sigma]1 - I*\[Sigma]2)/2, Transpose[HsN[[j]]]], 
       {j, 1, n}]; graph = AdjacencyGraph[AdjacencyM]; 
     Distances = Sort[DeleteDuplicates[Flatten[GraphDistanceMatrix[
          graph]]]]; Enumerated = Table[{}, 
       {iii, 1, Length[Distances]}]; For[jj = 1, 
      jj < Length[Distances] + 1, jj++, Enumerated[[jj]] = 
       Count[GraphDistanceMatrix[graph], Distances[[jj]], 2]]; 
     If[ !ContainsAny[Topologies, {{Distances, Enumerated}}], 
      Topologies = Append[Topologies, {Distances, Enumerated}]; 
       If[ConnectedGraphQ[graph] == True, Connected = 
         Append[Connected, AdjacencyM], NotConnected = 
         Append[NotConnected, AdjacencyM]]]]; 
   Ceigs = Table[Eigenvalues[Connected[[i]]], 
     {i, 1, Length[Connected]}]; 
   NCeigs = Table[Eigenvalues[NotConnected[[i]]], 
     {i, 1, Length[NotConnected]}]; Print["Connected Graphs: ", 
    Dimensions[Connected], "\nAdjacency Spectra: ", Ceigs, 
    "\nDisconnected Graphs: ", Dimensions[NotConnected], 
    "\nAdjacency Spectra: ", NCeigs]; 
   Return[{Connected, Ceigs, NotConnected, NCeigs}])
   
AllSubGraphsRandom[Hs_, n_] := (Connected = {}; NotConnected = {}; 
   Combinations = Subsets[Table[i, {i, 1, Length[Hs]}], {n}]; 
   For[i = 1, i < 500, i++, 
    ri = RandomInteger[{1, Length[Combinations]}]; 
     HsN = Table[Hs[[Combinations[[ri,j]]]], {j, 1, n}]; 
     AdjacencyM = Sum[KroneckerProduct[(\[Sigma]1 + I*\[Sigma]2)/2, HsN[[j]]] + 
        KroneckerProduct[(\[Sigma]1 - I*\[Sigma]2)/2, Transpose[HsN[[j]]]], 
       {j, 1, n}]; graph = AdjacencyGraph[AdjacencyM]; 
     If[ConnectedGraphQ[graph] == True, Connected = 
       Append[Connected, AdjacencyM], NotConnected = 
       Append[NotConnected, AdjacencyM]]]; 
   Ceigs = Table[Eigenvalues[Connected[[i]]], 
     {i, 1, Length[Connected]}]; Ceigs = DeleteDuplicates[Ceigs]; 
   NCeigs = Table[Eigenvalues[NotConnected[[i]]], 
     {i, 1, Length[NotConnected]}]; 
   NCeigs = DeleteDuplicates[NCeigs]; Print["Connected Graphs: ", 
    Dimensions[Connected], "\nAdjacency Spectra: ", Ceigs, 
    "\nDisconnected Graphs: ", Dimensions[NotConnected], 
    "\nAdjacency Spectra: ", NCeigs]; 
   Return[{Connected, Ceigs, NotConnected, NCeigs}])


AllSubAlgebras[\[CapitalGamma]s_, n_] := (Valid = {}; InValid = {}; 
   Connected = {}; RHS = Table[2*IdentityMatrix[Dimensions[\[CapitalGamma]s][[2]]]*
      IdentityMatrix[Dimensions[\[CapitalGamma]s][[1]]][[i,j]], 
     {i, 1, Length[\[CapitalGamma]s]}, {j, 1, Length[\[CapitalGamma]s]}]; 
   Combinations = Subsets[Table[h, {h, 1, Length[\[CapitalGamma]s]}], {n}]; 
   For[hh = 1, hh < Length[Combinations] + 1, hh++, 
    \[CapitalGamma]sN = Table[\[CapitalGamma]s[[Combinations[[hh,hhh]]]], {hhh, 1, n}]; 
     If[GRdN\[CapitalGamma][\[CapitalGamma]sN, RHS], Valid = Append[Valid, \[CapitalGamma]sN]; 
       If[ConnectedGraphQ[AdjacencyGraph[Sum[Abs[\[CapitalGamma]sN[[hhh]]], 
           {hhh, 1, n}]]], Connected = Append[Connected, \[CapitalGamma]sN]], 
      InValid = Append[InValid, \[CapitalGamma]sN]]]; 
   Print["n=", n, "-color decomposition: Valid Supermultiplets: ", 
    Dimensions[Valid], "\n Fully Connected Supermultiplets: ", 
    Dimensions[Connected]]; Return[{Valid, InValid}])
    
AllNonSubAlgebrasNoPrint[\[CapitalGamma]s_, n_] := (Valid = {}; InValid = {}; 
   Connected = {}; RHS = Table[2*IdentityMatrix[Dimensions[\[CapitalGamma]s][[2]]]*
      IdentityMatrix[Dimensions[\[CapitalGamma]s][[1]]][[i,j]], 
     {i, 1, Length[\[CapitalGamma]s]}, {j, 1, Length[\[CapitalGamma]s]}]; 
   Combinations = Subsets[Table[h, {h, 1, Length[\[CapitalGamma]s]}], {n}]; 
   For[hh = 1, hh < Length[Combinations] + 1, hh++, 
    \[CapitalGamma]sN = Table[\[CapitalGamma]s[[Combinations[[hh,hhh]]]], {hhh, 1, n}]; 
     If[ !GRdN\[CapitalGamma][\[CapitalGamma]sN, RHS], InValid = Append[InValid, \[CapitalGamma]sN]]]; 
   Return[{InValid}])
   
AllSubAlgebrasNoPrint[\[CapitalGamma]s_, n_] := (Valid = {}; InValid = {}; 
   Connected = {}; RHS = Table[2*IdentityMatrix[Dimensions[\[CapitalGamma]s][[2]]]*
      IdentityMatrix[Dimensions[\[CapitalGamma]s][[1]]][[i,j]], 
     {i, 1, Length[\[CapitalGamma]s]}, {j, 1, Length[\[CapitalGamma]s]}]; 
   Combinations = Subsets[Table[h, {h, 1, Length[\[CapitalGamma]s]}], {n}]; 
   For[hh = 1, hh < Length[Combinations] + 1, hh++, 
    \[CapitalGamma]sN = Table[\[CapitalGamma]s[[Combinations[[hh,hhh]]]], {hhh, 1, n}]; 
     If[GRdN\[CapitalGamma][\[CapitalGamma]sN, RHS], Valid = Append[Valid, \[CapitalGamma]sN]; 
       If[ConnectedGraphQ[AdjacencyGraph[Sum[Abs[\[CapitalGamma]sN[[hhh]]], 
           {hhh, 1, n}]]], Connected = Append[Connected, \[CapitalGamma]sN]]]]; 
   Return[{Valid}])


(* ::Section::Closed:: *)
(*Involutions Resource Definition*)


ClearAll[Involutions, FromCycles]
FromCycles[cyc_List] := 
  Last /@ Sort[Transpose[Flatten /@ {RotateRight /@ cyc, 
       cyc}]]
Involutions[l_List, "Cycles"] := {{l}} /; Length[l] === 1
Involutions[l_List, "Cycles"] := {{}} /; Length[l] === 0
Involutions[l_List, "Cycles"] := 
  Block[{$RecursionLimit = Infinity, i}, 
   Join[Flatten[Table[
      (Append[#1, {l[[1]], l[[i]]}] & ) /@ 
       Involutions[Drop[Rest[l], {i - 1}], "Cycles"], 
      {i, 2, Length[l]}], 1], 
    (Append[#1, {l[[1]]}] & ) /@ Involutions[Rest[l], 
      "Cycles"]]]
Involutions[l_List] := FromCycles /@ 
   Involutions[l, "Cycles"]
Involutions[n_Integer] := Involutions[Range[n]]
Involutions[n_Integer, "Cycles"] := 
  Involutions[Range[n], "Cycles"]


(* ::Section::Closed:: *)
(*Dashed Tube Resource Definition*)


(* ::Subsection:: *)
(*Preliminaries*)


(* ::Input::Initialization:: *)
(*the high-level function is DashedTube*)
(*to take care of external dashing specifications an internal function inDashedTube is used *)
(*to take care of external specifications of arrows, an additional intermediary function arrDashedTube is used, that evaluates in the end into inDashedTube *)
(*function inDashedTube is just a wrapper around createTubes*)
(*it is createTubes that does the actual job*)

Options[DashedTube] = {PlotPoints -> 30, 
    VertexColors -> Automatic, "Each" -> False, 
    MaxIterations -> 20}; 
Options[arrDashedTube] := Options[DashedTube]; 
Options[inDashedTube] := Options[DashedTube]; 
SyntaxInformation[DashedTube] = 
   {"ArgumentsPattern" -> {_, ___, OptionsPattern[]}};

(*error messages*)
DashedTube::invpts="Invalid point specification: ``";
DashedTube::invdash="Invalid dashing specification: ``";
DashedTube::invcols="Invalid color specification: ``";
DashedTube::invrads="Invalid radius specification: ``";
DashedTube::invnum="Invalid PlotPoints specification: ``";
DashedTube::invmaxiters="Invalid MaxIterations specification: ``";
DashedTube::inveach="Invalid 'Each' specification: ``";

(*default values*)
Default[arrDashedTube,2]=Default[DashedTube,2]=Default[inDashedTube,2]=0.05;
Default[arrDashedTube,3]=Default[DashedTube,3]=Default[inDashedTube,3]={0.2,0.2};
Default[inDashedTube,4]=False;

(*upvalues*)
DashedTube/:Arrow[DashedTube[args___]]:=arrDashedTube[args];

DashedTube/:Graphics3D[DashedTube[args___],rest___]:=Graphics3D[{DashedTube[args]},rest];
DashedTube/:{pre___,dash:_Dashing|_AbsoluteDashing,Shortest[inter___],DashedTube[pts_,rad_,dd:(_?NumericQ|{___?NumericQ}|None),opts:OptionsPattern[]],post___}:={pre,dash,inter,inDashedTube[pts,rad,dd,False,opts],post};
DashedTube/:{pre___,dash:_Dashing|_AbsoluteDashing,Shortest[inter___],DashedTube[pts_,Shortest[rad_.],opts:OptionsPattern[]],post___}:={pre,dash,inter,inDashedTube[pts,rad,dash,False,opts],post};
DashedTube/:{pre___,DashedTube[pts_,Shortest[rad_.,1],Shortest[dash_.,2],opts:OptionsPattern[]],post___}:={pre,inDashedTube[pts,rad,dash,False,opts],post};

arrDashedTube/:Graphics3D[arrDashedTube[args___],rest___]:=Graphics3D[{arrDashedTube[args]},rest];
arrDashedTube/:{pre___,dash:_Dashing|_AbsoluteDashing,Shortest[inter___],arrDashedTube[pts_,rad_,dd:(_?NumericQ|{___?NumericQ}|None),opts:OptionsPattern[]],post___}:={pre,dash,inter,inDashedTube[pts,rad,dd,True,opts],post};
arrDashedTube/:{pre___,dash:_Dashing|_AbsoluteDashing,Shortest[inter___],arrDashedTube[pts_,Shortest[rad_.],opts:OptionsPattern[]],post___}:={pre,dash,inter,inDashedTube[pts,rad,dash,True,opts],post};
arrDashedTube/:{pre___,arrDashedTube[pts_,Shortest[rad_.,1],Shortest[dash_.,2],opts:OptionsPattern[]],post___}:={pre,inDashedTube[pts,rad,dash,True,opts],post};


(* ::Input::Initialization:: *)
(*line as the input*)
inDashedTube[pts_List,r___]:=inDashedTube[Line[pts],r]
inDashedTube[Line[pts_],r___]:=inDashedTube[BSplineCurve[pts,SplineDegree->1],r]


(* ::Input::Initialization:: *)
(*B-spline as the input*)
inDashedTube[BSplineCurve[pts_,splopts:OptionsPattern[BSplineCurve]],rad_.,dashing_.,arrowed_.,OptionsPattern[]]:=Module[{num=OptionValue[PlotPoints],maxiters=OptionValue[MaxIterations],eachsep=OptionValue["Each"],vertcol=OptionValue[VertexColors],
sploptsloc=FilterRules[{splopts},Options[BSplineCurve]]},

createTubes[BSplineCurve,BSplineFunction,sploptsloc,pts,rad,dashing,arrowed,vertcol,num,maxiters,eachsep]
];


(* ::Input::Initialization:: *)
(*Bezier curve as the input*)
inDashedTube[BezierCurve[pts_,splopts:OptionsPattern[BezierCurve]],rad_.,dashing_.,arrowed_.,OptionsPattern[]]:=Module[{num=OptionValue[PlotPoints],maxiters=OptionValue[MaxIterations],eachsep=OptionValue["Each"],vertcol=OptionValue[VertexColors],
sploptsloc=FilterRules[{splopts},Options[BezierCurve]]},

createTubes[BezierCurve,BezierFunction,sploptsloc,pts,rad,dashing,arrowed,vertcol,num,maxiters,eachsep]
];


(* ::Subsection::Closed:: *)
(*Preprocessing*)


(* ::Input::Initialization:: *)
preprocessDashing[dashing_]:=Module[{dashloc=dashing,tiny=0.01,small=0.05,medium=0.1,large=0.2},

dashloc=dashloc/.{Tiny->tiny,Small->small,Medium->medium,Large->large}/.Dashing|AbsoluteDashing->Identity;

Switch[dashloc,
{__?(NumericQ[#]&&NonNegative[#]&)},
dashloc/.{0|0.->0.01}
,
_?Positive,
{dashloc,dashloc}
,
{}|None|0|0.,
None
,
_,
Message[DashedTube::invdash,dashloc];
$Failed
]
]


(* ::Input::Initialization:: *)
preprocessColors[cols_,pdims_]:=Module[{dims,colsloc,cfun},

{dims,colsloc}=Switch[cols,

Automatic|{}|None,
{pdims,Automatic}
,
_?ColorQ,
{pdims,Table[cols,#]&/@pdims}
,
{__?ColorQ},
{{Length[cols]},{cols}}
,
{{__?ColorQ}..},
{Length/@cols,cols}
,
_,
Message[DashedTube::invcols,cols];
Return[$Failed];
{{},{}}
];

If[dims=!=pdims,Message[DashedTube::invcols,cols];Return[$Failed]];

colsloc
]


(* ::Input::Initialization:: *)
preprocessPoints[pts_]:=Module[{dims},

Switch[pts,

{{Repeated[{_?NumericQ,_?NumericQ,_?NumericQ},{2,\[Infinity]}]}..},
dims=Length/@pts;
{dims,pts,False}
,
{{Repeated[Scaled[{_?NumericQ,_?NumericQ,_?NumericQ}],{2,\[Infinity]}]}..},
dims=Length/@pts;
{dims,pts[[;;,;;,1]],True}
,
{Repeated[{_?NumericQ,_?NumericQ,_?NumericQ},{2,\[Infinity]}]},
dims={Length[pts]};
{dims,{pts},False}
,
{Repeated[Scaled[{_?NumericQ,_?NumericQ,_?NumericQ}],{2,\[Infinity]}]},
dims={Length[pts]};
{dims,{pts[[;;,1]]},True}
,
_,
Message[DashedTube::invpts,pts];{{},$Failed,False}
]
]


(* ::Input::Initialization:: *)
preprocessRadii[rads_,pdims_]:=Module[{dims,radsloc,cfun,scaled},

{dims,radsloc,scaled}=Switch[rads,

_?Positive,
{pdims,ConstantArray[rads,#]&/@pdims,False}
,
Scaled[_?Positive],
{pdims,ConstantArray[rads[[1]],#]&/@pdims,True}
,

{__?Positive},
{{Length[rads]},{rads},False}
,
{__Scaled?(Positive@*First)},
{{Length[rads]},{rads[[;;,1]]},True}
,

{{__?Positive}..},
{Length/@rads,rads,False}
,
{{__Scaled?(Positive@*First)}..},
{Length/@rads,rads[[;;,;;,1]],True}
,

_,
{0,{},False}
];

If[dims=!=pdims,Message[DashedTube::invrads,rads];Return[{$Failed,{}}]];

{radsloc,scaled}
]


(* ::Subsection::Closed:: *)
(*Auxiliary*)


(* ::Input::Initialization:: *)
createMesh[fun_,pts_,spopts_,num_,dashing_]:=Module[{mesh,lengths,acc,bins,binlengths,len},
(*sample the curve*)

(*sample the input function/curve uniformly*)
mesh=fun[pts,spopts]/@Subdivide[num];

(*calculate lengths of each segment*)
lengths=EuclideanDistance@@@Partition[mesh,2,1];

(*calculate accumulated lenghts*)
acc=Accumulate[lengths];
len=Last[acc];

(*prepared equidistant bins*)
bins=Accumulate@Flatten@Table[dashing,Ceiling[len/Total[dashing]]];
If[First[bins]>0,bins=Join[{0},bins]];
If[Last[bins]<len,bins=Join[bins,{len}]];

(*find out how many segments fall into individual bins*)
binlengths=Length/@BinLists[acc,{bins}];

(*mesh contains equidistant points of the original curve,
whereas binlenghts says to which segment these points should belong*)
{mesh,binlengths}
]


(* ::Input::Initialization:: *)
ClearAll[singleTube]
singleTube[pts_,cols_,rads_,eachsep_,fun_,spopts_,num_,dashing_,maxiters_,scaledrads_,scaledpts_]:=Module[{mesh,binlengths,numloc=num,incr=10,colmesh,radmesh,radfun,colfun},
(*prepare the data for each chopped curve*)

(*interatively generate mesh until all the segments have at least 2 points*)
{mesh,binlengths}=createMesh[fun,pts,spopts,numloc,dashing];
While[AnyTrue[binlengths,LessEqualThan[1]]&&numloc/incr<maxiters,
numloc+=incr;
{mesh,binlengths}=createMesh[fun,pts,spopts,numloc,dashing];
];
If[scaledpts,mesh=Scaled/@mesh];

(*from the input radius specification generate a list of (many) radii, each of which corresponds to one point of the chopped curve*)
radmesh=Transpose[{Subdivide[Length[rads]-1],rads}];
radfun=Quiet@Interpolation[radmesh,InterpolationOrder->2];
radmesh=radfun/@Subdivide[numloc];
If[scaledrads,radmesh=Scaled/@radmesh];

(*because of the dashing, only every other segment is retained*)
mesh=TakeList[mesh,binlengths][[;;;;2]];
radmesh=TakeList[radmesh,binlengths][[;;;;2]];

(*determine colors*)
If[cols===Automatic,

(*if no explicit setting, set colors to Automatic*)
colmesh=Table[Automatic,Length[radmesh]];
,
(*otherwise, from the input color specification generate a list of (many) colors, each of which corresonds to one point of the chopped curve*)
If[eachsep,
(*if each segment is to have the same color styling...*)
colfun[c_,t_]:=(Blend[c,#]&/@Subdivide[t-1]);
colmesh=colfun[cols,#]&/@binlengths[[;;;;2]];
,
(*if all the segments are treated as a single curve...*)
colmesh=Blend[cols,#]&/@Subdivide[Total[binlengths]];
colmesh=TakeList[colmesh,binlengths][[;;;;2]];
];
];

(*return specs for a single tube*)
Transpose[{mesh,radmesh,colmesh}]
]


(* ::Input::Initialization:: *)
segmentFun[pts_,rads_,cols_,arrowed_]:=
(*if no points, don't show anything; if one, point, show a ball; otherwise use Tube*)
Switch[Length[pts],
0,{},
1,{First[cols,Nothing],Ball[First@pts,First@rads]},
_,If[arrowed,Arrow,Identity]@Tube[pts,rads,VertexColors->cols]
]


(* ::Input::Initialization:: *)
createTubesRaw[ptsloc_,vertcolloc_,radsloc_,arrowed_,rad_,curvefun_,eachsep_,funfun_,sploptsloc_,num_,dashloc_,maxiters_,scaledrads_,scaledpts_]:=Module[{aux,tube,cols},
(*take tube specs from singleTube, turned them into Tubes and take care of Arrows, when necessary *)

(*if no explicit color specification, use Automatic everywhere*)
cols=If[vertcolloc===Automatic,Table[Automatic,Length[radsloc]],vertcolloc];

(*get specs*)
aux=Transpose[{ptsloc,cols,radsloc}];
aux=singleTube[#1,#2,#3,eachsep,funfun,sploptsloc,num,dashloc,maxiters,scaledrads,scaledpts]&@@@aux;

(*generate Tubes*)
tube=If[eachsep,
(*if styling is to be applied to each segment independently...*)
Apply[{Last[#3,Nothing],segmentFun[##,arrowed]}&,aux,{2}]
,
(*otherwise generate a collection of segments...*)
{
Apply[{Last[#3,Nothing],segmentFun[##,False]}&,aux,{2}]
,
(*if the curve has an arrow, the arrow is attached as a transparent line of smaller diameter with colored arrow tip*)
If[arrowed,
{Last[#1,Nothing],Arrow@Tube[
curvefun[#2,Sequence@@sploptsloc],
If[scaledrads,rad/.Scaled[x_]:>Scaled[0.3x],0.3rad],
VertexColors->Table[Transparent,Length[#2]]]}&@@@Transpose[{cols,ptsloc}]
,Nothing]
}
];

(*return segments for each curve*)
tube
]


(* ::Subsection:: *)
(*Main*)


(* ::Input::Initialization:: *)
createTubes[curvefun_,funfun_,sploptsloc_,pts_,rad_,dashing_,arrowed_,vertcol_,num_,maxiters_,eachsep_]:=Module[{dims,ptsloc,dashloc,arrloc,vertcolloc,radsloc,tube,binlengths,cols,aux,rads,colfun,colmesh,scaledrads,scaledpts},

(* --- This is the main function --- *)

(*preprocessing of option values*)
If[!TrueQ[num\[Element]PositiveIntegers],
Message[DashedTube::invnum,num];Return[{}]];

If[!TrueQ[maxiters\[Element]PositiveIntegers],
Message[DashedTube::invmaxiters,maxiters];Return[{}]];

If[!TrueQ[eachsep\[Element]Booleans],
Message[DashedTube::inveach,eachsep];Return[{}]];

(*preprocessing of inputs*)
{dims,ptsloc,scaledpts}=preprocessPoints[pts];
If[ptsloc===$Failed,Return[{}]];

{radsloc,scaledrads}=preprocessRadii[rad,dims];
If[radsloc===$Failed,Return[{}]];

vertcolloc=preprocessColors[vertcol,dims];
If[vertcolloc===$Failed,Return[{}]];

dashloc=preprocessDashing[dashing];
If[dashloc===$Failed,Return[{}]];

(*generate tubes*)
tube=If[dashloc===None,

(*if no dashing, use the built-in function*)
If[arrowed,Arrow,Identity]@Tube[curvefun[ptsloc,Sequence@@sploptsloc],rad,VertexColors->vertcolloc],

(*otherwise chop the curve into small segments, each of which will be a linear tube*)
createTubesRaw[ptsloc,vertcolloc,radsloc,arrowed,rad,curvefun,eachsep,funfun,sploptsloc,num,dashloc,maxiters,scaledrads,scaledpts]
];

(*return result*)
tube
]


(* ::Section::Closed:: *)
(*(End of Private Context)*)


End[]


(* ::Section::Closed:: *)
(*(End of Package)*)


EndPackage[]
