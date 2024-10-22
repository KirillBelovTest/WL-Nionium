#!/usr/bin/env wolframscript
(* ::Package:: *)

If[Head[$FrontEnd] === FrontEndObject, 
	SetDirectory[NotebookDirectory[]], 
	SetDirectory[DirectoryName[$InputFileName]]
];


$HistoryLength = 0; 


llm := If[Head[#] === Function, #[], Echo["[NOT READY]"]]& @ Check[Get["llm.wl"], $Failed]; 


Do[llm; Pause[5], {12 * 60 * 24 * 365}]; 
