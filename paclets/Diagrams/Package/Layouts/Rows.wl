Package["Diagrams`Layouts`Rows`"]

PackageUse[Diagrams -> {
	Diagram, DiaBox, DiaArrow, DiagramElementContent, DiagramElementId,
	PlacedDiagram, DiagramBoxes, DiagramArrows,
	Layouts -> DoRowsLayout,
	Layout -> {
		$Margin,
		PlacedBox,
		LayoutUtils -> {
			MakePlacedBox, RowWidth, PlaceArrowsBasedOnBoxes,
			PlacedBoxBorderRectangle, GroupBoxesByGraphRow
		}
	},
	Utils -> {RectangleWidth, RectangleHeight},
	Errors -> {RaiseError, RaiseAssert, SetFallthroughError}
}]

(*========================================================*)

SetFallthroughError[DoRowsLayout]

(* Layout all diagram boxes in a series of rows. *)
DoRowsLayout[
	boxes0: _List,
	arrows0: _List
] := Module[{
	boxes = boxes0,
	arrows = arrows0,
	rows,
	maxRowWidth = 0,
	xOffset = 0.0,
	yOffset = 0.0,
	placedBoxes = <||>,
	placedArrows = {}
},
	rows = GroupBoxesByGraphRow[boxes, arrows];

	RaiseAssert[MatchQ[rows, {{DiaBox[__] ...} ...}]];

	(*-------------*)
	(* Place boxes *)
	(*-------------*)

	maxRowWidth = Max @ Map[RowWidth, rows];

	RaiseAssert[Positive[maxRowWidth]];

	Scan[
		Replace[{
			row:{___DiaBox} :> Module[{
				rowWidth = RowWidth[row],
				(* The maximum height of a box in this row. *)
				rowMaxHeight = 0
			},
				RaiseAssert[PossibleZeroQ[xOffset]];

				(* Shift this row to the right by the required amount to
				   horizontally center it with whatever the widest row is. *)
				xOffset = (maxRowWidth - rowWidth) / 2.0;

				Scan[
					Replace[{
						box_DiaBox :> Module[{
							id = DiagramElementId[box],
							borderRect,
							placedBox
						},
							placedBox = MakePlacedBox[box, {xOffset, yOffset}];

							borderRect = PlacedBoxBorderRectangle[placedBox];
							xOffset += RectangleWidth[borderRect] + $Margin;
							rowMaxHeight = Max[rowMaxHeight, RectangleHeight[borderRect]];

							AssociateTo[placedBoxes, id -> placedBox];
						],
						other_ :> RaiseError["unexpected diagram box structure: ``", other]
					}],
					row
				];

				RaiseAssert[NumberQ[rowMaxHeight]];

				(* Start the next row at the far left. *)
				xOffset = 0;
				(* Place the next row higher up. *)
				yOffset += $Margin + rowMaxHeight;
			]
		}],
		rows
	];

	(*--------------*)
	(* Place arrows *)
	(*--------------*)

	placedArrows = PlaceArrowsBasedOnBoxes[
		arrows,
		placedBoxes,
		(* Only permit arrows placed automatically to attach to the bottom or
		   top edge of a box. *)
		{Top, Bottom}
	];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	PlacedDiagram[
		placedBoxes,
		placedArrows
	]
]
