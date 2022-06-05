#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<sij`

tri = {{c11, c12, c13, c14, 0, 0}, {0, c11, c13, -c14, 0, 0}, {0, 0, 
        c33, 0, 0, 0}, {0, 0, 0, c44, 0, 0}, {0, 0, 0, 0, c44, c14}, {0, 0, 0,
         0, 0, (c11 - c12) / 2}} // Symmetrise

cj = {c11, c33, c12, c13, c44, c14};

f = ElasticEnergy[tri]

S[f, cj] // MatrixForm
