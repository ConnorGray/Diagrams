Package["Diagrams`Layouts`Row`"]

PackageUse[Diagrams -> {
	Diagram, DiaBox, DiaArrow, DiagramElementContent, DiagramElementId,
	PlacedDiagram,
	Layouts -> DoRowLayout,
	Layout -> {
		$Margin,
		PlacedBox,
		Utils -> {MakePlacedBox, PlacedBoxBorderRectangle, PlaceArrowsBasedOnBoxes}
	},
	Utils -> {RectangleWidth},
	Errors -> {RaiseError, RaiseAssert}
}]

(* Layout all diagram boxes in a single row. *)
DoRowLayout[
	Diagram[
		boxes:{___DiaBox},
		arrows:{___DiaArrow},
		___?OptionQ
	]
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
			box_DiaBox :> Module[{
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
