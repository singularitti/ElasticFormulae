#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]

SetDirectory[NotebookDirectory[]];

<<sij`

hex = {{c11, c12, c13, 0, 0, 0}, {0, c11, c13, 0, 0, 0}, {0, 0, c33, 
      0, 0, 0}, {0, 0, 0, c44, 0, 0}, {0, 0, 0, 0, c55, 0}, {0, 0, 0, 0, 0,
       (c11 - c12) / 2}} // Symmetrise

f = ElasticEnergy[hex]

cj = {c11, c33, c12, c13, c44};

S[f, cj] // MatrixForm
