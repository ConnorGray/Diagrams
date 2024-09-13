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
	},
	Layout -> {LayoutUtils -> PlaceArrowsBasedOnBoxes}
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
	placedBoxes,
	arrowPlacementSpec,
	placedArrows,
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

	(*--------------------------------*)
	(* Compute box layout             *)
	(*--------------------------------*)

	Block[{
		$BoxPadding = Replace[
			Lookup[algoOpts, BoxPadding, Automatic],
			Automatic :> $BoxPadding
		]
	},
		{placedBoxes, arrowPlacementSpec} = Replace[algo, {
			"Row" :> {DoRowLayout[boxes], Automatic},
			(* Only permit arrows placed automatically to attach to the bottom or
				top edge of a box. *)
			"Rows" :> {DoRowsLayout[boxes, arrows], {Top, Bottom}},
			"EqualWidthRows" :> {DoEqualWidthRowsLayout[boxes, arrows], {Top, Bottom}},
			"Graph" :> {DoGraphLayout[boxes, arrows], Automatic},
			"Manual" :> {DoManualLayout[boxes, arrows], Automatic},
			_ :> RaiseError["Unknown diagram layout algorithm: ``", algo]
		}];
	];

	RaiseAssert[
		MatchQ[placedBoxes, _?AssociationQ],
		"Diagram layout `` did not return expected result: ``",
		InputForm[algo],
		InputForm[placedBoxes]
	];

	(*--------------------------------*)
	(* Compute arrow placement        *)
	(*--------------------------------*)

	placedArrows = PlaceArrowsBasedOnBoxes[
		arrows,
		placedBoxes,
		arrowPlacementSpec
	];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	result = PlacedDiagram[
		placedBoxes,
		placedArrows
	];

	RaiseAssert[
		MatchQ[result, PlacedDiagram[_?AssociationQ, _?ListQ]],
		"diagram layout implementation for `` did not return expected result: ``",
		InputForm[algo],
		InputForm[result]
	];

	result
]
