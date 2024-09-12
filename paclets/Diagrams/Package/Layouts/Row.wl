Package["Diagrams`Layouts`Row`"]

PackageUse[Diagrams -> {
	Diagram, DiaBox, DiaArrow, DiagramElementContent, DiagramElementId,
	PlacedDiagram,
	Layouts -> DoRowLayout,
	Layout -> {
		$Margin,
		PlacedBox,
		LayoutUtils -> {
			MakePlacedBox, PlacedBoxBorderRectangle
		}
	},
	Utils -> {RectangleWidth},
	Errors -> {RaiseError, RaiseAssert, SetFallthroughError}
}]

(*========================================================*)

SetFallthroughError[DoRowLayout]

(* Layout all diagram boxes in a single row. *)
DoRowLayout[
	boxes: {___DiaBox}
] := Module[{
	xOffset,
	placedBoxes
},
	xOffset = 0.0;
	placedBoxes = <||>;

	(*-------------*)
	(* Place boxes *)
	(*-------------*)

	Scan[
		Replace[{
			box: _DiaBox :> Module[{
				id = DiagramElementId[box],
				borderRect,
				placedBox
			},
				placedBox = MakePlacedBox[box, {xOffset, 0}];

				borderRect = PlacedBoxBorderRectangle[placedBox];
				xOffset += RectangleWidth[borderRect] + $Margin;

				AssociateTo[placedBoxes, id -> placedBox];
			],
			other_ :> RaiseError["unexpected diagram box structure: ``", other]
		}],
		boxes
	];

	placedBoxes
]
