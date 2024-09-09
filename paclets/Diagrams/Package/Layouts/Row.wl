Package["Diagrams`Layouts`Row`"]

PackageUse[Diagrams -> {
	Diagram, DiaBox, DiaArrow, DiagramElementContent, DiagramElementId,
	PlacedDiagram,
	Layouts -> DoRowLayout,
	Layout -> {
		$Margin,
		PlacedBox,
		LayoutUtils -> {
			MakePlacedBox, PlacedBoxBorderRectangle, PlaceArrowsBasedOnBoxes
		}
	},
	Utils -> {RectangleWidth},
	Errors -> {RaiseError, RaiseAssert, SetFallthroughError}
}]

(*========================================================*)

SetFallthroughError[DoRowLayout]

(* Layout all diagram boxes in a single row. *)
DoRowLayout[
	boxes: {___DiaBox},
	arrows: {___DiaArrow}
] := Module[{
	xOffset,
	placedBoxes,
	placedArrows
},
	xOffset = 0.0;
	placedBoxes = <||>;
	placedArrows = {};

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

	(*--------------*)
	(* Place arrows *)
	(*--------------*)

	placedArrows = PlaceArrowsBasedOnBoxes[arrows, placedBoxes];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	PlacedDiagram[
		placedBoxes,
		placedArrows
	]
]
