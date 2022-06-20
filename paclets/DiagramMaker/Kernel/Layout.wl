BeginPackage["DiagramMaker`Layout`"]

PlacedBox::usage = "PlacedBox[box, textRect, borderRect]"
PlacedArrow::usage = "PlacedBox[arrow, startPoint, endPoint]"

$textWidth = 300.0;
$BoxPadding = 8.0;
$margin = 32.0;

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Utils`"]
Needs["DiagramMaker`Layouts`"]


LayoutDiagram[
	diagram_Diagram,
	algo0 : _?StringQ : Automatic
] := Module[{
	diagramOpts = Options[diagram],
	algo,
	result
},
	algo = Replace[algo0, Automatic :> OptionValue[Diagram, diagramOpts, DiagramLayout]];

	(* Default to "RowsLayout". *)
	algo = Replace[algo, Automatic :> "Rows"];

	result = Replace[algo, {
		"Row" :> DoRowLayout[diagram],
		"Rows" :> DoRowsLayout[diagram],
		"EqualWidthRows" :> DoEqualWidthRowsLayout[diagram],
		"Graph" :> DoGraphLayout[diagram],
		_ :> RaiseError["Unknown diagram layout algorithm: ``", algo]
	}];

	RaiseAssert[
		MatchQ[result, PlacedDiagram[_?AssociationQ, _?ListQ]],
		"diagram layout implementation for `` did not return expected result: ``",
		InputForm[algo],
		InputForm[result]
	];

	result
]

(*------------------------------------*)

LayoutDiagram[args___] :=
	RaiseError["unexpected arguments to LayoutDiagram: ``", InputForm[{args}]]


End[]
EndPackage[]