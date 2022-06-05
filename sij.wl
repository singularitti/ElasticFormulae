(* ::Package:: *)

BeginPackage["sij`"]

Symmetrise::usage = "Construct a symmetric matrix from an upper triangular matrix."

ElasticEnergy::usage = "Calculate the free energy formula."

\[Epsilon]::usage = "Strains"

Sij::usage = "Calculate S[i, j]."

S::usage = "Calculate matrix S."

Begin["Private`"]

Symmetrise[upperTriMatrix_] :=
    UpperTriangularize[upperTriMatrix, 1] + Transpose[upperTriMatrix]

ElasticEnergy[c_, \[Epsilon]_] :=
    \[Epsilon] . c . \[Epsilon] / 2

\[Epsilon] = {\[Epsilon]1, \[Epsilon]2, \[Epsilon]3, \[Epsilon]4, \[Epsilon]5,
     \[Epsilon]6};

Sij[f_, \[Epsilon]i_, cj_] :=
    D[f, \[Epsilon]i, cj]

S[f_, c_] :=
    Table[Sij[f, \[Epsilon][[i]], c[[j]]], {i, 1, 6}, {j, 1, Length[c
        ]}] // Simplify

End[]

EndPackage[]
