#!/usr/bin/env wolframscript

Needs["PacletTools`"]
Needs["CodeFormatter`"]

Print["Using $InstallationDirectory: ", $InstallationDirectory];

(*----------------------------------------------------------*)
(* Verify that Directory[] is the repository root directory *)
(*----------------------------------------------------------*)

$repositoryDir = Directory[];

If[FileNameTake[$repositoryDir] =!= "diagram-maker",
	Throw[StringForm["Unexpected repository root directory path: ``", $repositoryDir]]
];

(*-------------------------------------------------------------*)
(* Build the DiagramMaker paclet into $repositoryDir/build/ *)
(*-------------------------------------------------------------*)

result = PacletBuild[
	FileNameJoin[{$repositoryDir, "paclets", "DiagramMaker"}],
	FileNameJoin[{$repositoryDir, "build"}]
];

If[FailureQ[result],
	Echo @ CodeFormat @ ToString[result, InputForm];
	Exit[-1];
];

(*-----------------------------------------------------------------------------------*)
(* Copy the examples and tests dynamic libraries into the LibraryResources directory *)
(*-----------------------------------------------------------------------------------*)

$dir = FileNameJoin[{
    $repositoryDir, "build", "DiagramMaker", "LibraryResources", $SystemID
}];

CreateDirectory[$dir]

(* FIXME: Make this switch between release and debug builds somehow. *)
CopyFile[
    FileNameJoin[{$repositoryDir, "target", "debug", "libdiagram_maker.dylib"}],
    FileNameJoin[{$dir, "libdiagram_maker_wll.dylib"}]
]
