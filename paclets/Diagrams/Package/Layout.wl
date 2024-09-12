Package["Diagrams`Layout`"]

PackageModule["LayoutUtils"]

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
	Diagram, DiagramLayout, LayoutDiagram, PlacedDiagram,
	DiagramError,
	BoxPadding,
	Errors -> {
		RaiseError, RaiseAssert, SetFallthroughError, ConfirmReplace
	},
	Layouts -> {
		DoManualLayout,
		DoRowLayout, DoRowsLayout, DoEqualWidthRowsLayout, DoGraphLayout
	}
}]

(*========================================================*)

SetFallthroughError[LayoutDiagram]

LayoutDiagram[
	boxes: _?ListQ,
	arrows: _?ListQ,
	algo0 : _ : Automatic
] := Module[{
	algo = algo0,
	algoOpts,
	result
},
	{algo, algoOpts} = ConfirmReplace[algo, {
		name : (Automatic | _?StringQ) :> {name, {}},
		{name : (Automatic | _?StringQ), opts___?OptionQ} :> {name, {opts}},
		_ :> RaiseError["Invalid diagram layout specification: ``", algo]
	}];

	(* Default to "RowsLayout". *)
	(* If there is only one box, then this is likely a hierarchical diagram,
		so the top-level layout algorithm does not matter. *)
	algo = Replace[
		algo,
		Automatic :> Which[
			Length[boxes] === 1,
				"Manual",
			True,
				"Rows"
		]
	];

	Block[{
		$BoxPadding = Replace[
			Lookup[algoOpts, BoxPadding, Automatic],
			Automatic :> $BoxPadding
		]
	},
		result = Replace[algo, {
			"Row" :> DoRowLayout[boxes, arrows],
			"Rows" :> DoRowsLayout[boxes, arrows],
			"EqualWidthRows" :> DoEqualWidthRowsLayout[boxes, arrows],
			"Graph" :> DoGraphLayout[boxes, arrows],
			"Manual" :> DoManualLayout[boxes, arrows],
			_ :> RaiseError["Unknown diagram layout algorithm: ``", algo]
		}];
	];

	RaiseAssert[
		MatchQ[result, PlacedDiagram[_?AssociationQ, _?ListQ]],
		"diagram layout implementation for `` did not return expected result: ``",
		InputForm[algo],
		InputForm[result]
	];

	result
]
