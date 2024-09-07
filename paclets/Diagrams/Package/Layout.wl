Package["Diagrams`Layout`"]

PackageModule["Utils"]

PackageExport[{
	PlacedBox, PlacedArrow,

	$TextWidth, $BoxPadding, $Margin,

	$DebugDiagramLayout
}]

PlacedBox::usage = "PlacedBox[box, placedContent, contentRect, borderRect]"
PlacedArrow::usage = "PlacedBox[arrow, startPoint, endPoint]"

$TextWidth = 300.0;
$BoxPadding = 8.0;
$Margin = 32.0;

$DebugDiagramLayout = False


PackageUse[Diagrams -> {
	Diagram, DiagramLayout, DiagramTitle, LayoutDiagram, PlacedDiagram,
	BoxPadding,
	Errors -> {RaiseError, RaiseAssert, SetFallthroughError},
	Layouts -> {
		DoRowLayout, DoRowsLayout, DoEqualWidthRowsLayout, DoGraphLayout
	}
}]

(*========================================================*)

SetFallthroughError[LayoutDiagram]

LayoutDiagram[
	diagram_Diagram,
	algoSpec0 : _?StringQ : Automatic
] := Module[{
	title = DiagramTitle[diagram],
	diagramOpts = Options[diagram],
	algo,
	algoOpts,
	result
},
	algo = Replace[algoSpec0, Automatic :> OptionValue[Diagram, diagramOpts, DiagramLayout]];

	{algo, algoOpts} = Replace[algo, {
		name : (Automatic | _?StringQ) :> {name, {}},
		{name : (Automatic | _?StringQ), opts___?OptionQ} :> {name, {opts}},
		_ :> RaiseError["Invalid diagram layout specification: ``", algo]
	}];

	(* Default to "RowsLayout". *)
	algo = Replace[algo, Automatic :> "Rows"];

	Block[{
		$BoxPadding = Replace[
			Lookup[algoOpts, BoxPadding, Automatic],
			Automatic :> $BoxPadding
		]
	},
		result = Replace[algo, {
			"Row" :> DoRowLayout[diagram],
			"Rows" :> DoRowsLayout[diagram],
			"EqualWidthRows" :> DoEqualWidthRowsLayout[diagram],
			"Graph" :> DoGraphLayout[diagram],
			_ :> RaiseError["Unknown diagram layout algorithm: ``", algo]
		}];
	];

	If[
		And[
			StringQ[title],
			MatchQ[result, PlacedDiagram[boxes_?ListQ, arrows_?ListQ]]
		],
		result = PlacedDiagram[title, boxes, arrows]
	];

	RaiseAssert[
		MatchQ[result, PlacedDiagram[_?AssociationQ, _?ListQ]],
		"diagram layout implementation for `` did not return expected result: ``",
		InputForm[algo],
		InputForm[result]
	];

	result
]
