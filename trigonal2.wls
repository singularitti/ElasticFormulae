#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<sij`

tri = {{c11, c12, c13, c14, c15, 0}, {0, c11, c13, -c14, -c15, 0}, {0,
         0, c33, 0, 0, 0}, {0, 0, 0, c44, 0, -c15}, {0, 0, 0, 0, c44, c14}, {
        0, 0, 0, 0, 0, (c11 - c12) / 2}} // Symmetrise

tri // SymmetricMatrixQ

cj = {c11, c33, c12, c13, c44, c14, c15};

f = ElasticEnergy[tri]

Length[cj] == 7

S[f, cj] // MatrixForm
