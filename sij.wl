(* ::Package:: *)

BeginPackage["sij`"]

Symmetrise::usage = "Construct a symmetric matrix from an upper triangular matrix."

ElasticEnergy::usage = "Calculate the free energy formula."

Sij::usage = "Calculate S[i, j]."

S::usage = "Calculate matrix S."

Begin["`Private`"]

\[Epsilon] = {Global`\[Epsilon]1, Global`\[Epsilon]2, Global`\[Epsilon]3,
     Global`\[Epsilon]4, Global`\[Epsilon]5, Global`\[Epsilon]6};

Symmetrise[upperTriMatrix_] :=
    UpperTriangularize[upperTriMatrix, 1] + Transpose[upperTriMatrix]

ElasticEnergy[c_] :=
    \[Epsilon] . c . \[Epsilon] / 2

Sij[f_, \[Epsilon]i_, cj_] :=
    D[f, \[Epsilon]i, cj]

S[f_, c_] :=
    Table[Sij[f, \[Epsilon][[i]], c[[j]]], {i, 1, 6}, {j, 1, Length[c
        ]}] // Simplify

End[]

EndPackage[]
