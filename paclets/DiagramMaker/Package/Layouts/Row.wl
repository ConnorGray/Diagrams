Package["DiagramMaker`Layouts`Row`"]

PackageUse[DiagramMaker -> {
	Diagram, DiaBox, DiaArrow, DiagramElementText, DiagramElementId,
	PlacedDiagram,
	Layouts -> DoRowLayout,
	Layout -> {
		$Margin,
		PlacedBox,
		Utils -> {MakeBoxRectangles, PlaceArrowsBasedOnBoxes}
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
				text = DiagramElementText[box],
				textRect, borderRect,
				placedBox
			},
				{textRect, borderRect} = MakeBoxRectangles[text, {xOffset, 0}];

				placedBox = PlacedBox[box, textRect, borderRect];

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
