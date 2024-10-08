Package["Diagrams`Layouts`EqualWidthRows`"]

PackageUse[Diagrams -> {
	Diagram, PlacedDiagram, DiagramElementId,
	DiagramElementContent, DiaBox,
	Layouts -> DoEqualWidthRowsLayout,
	Layout -> {
		$Margin, $BoxPadding, PlacedBox,
		LayoutUtils -> {
			MakePlacedBox, PlacedBoxBorderRectangle, RowWidth, GroupBoxesByGraphRow
		}
	},
	Utils -> {RectangleWidth, RectangleHeight},
	Errors -> {RaiseError, RaiseAssert, SetFallthroughError}
}]

(*========================================================*)

SetFallthroughError[DoEqualWidthRowsLayout]

DoEqualWidthRowsLayout[
	boxes0: _List,
	arrows0: _List
] := Module[{
	boxes = boxes0,
	arrows = arrows0,
	rows,
	maxRowWidth = 0,
	xOffset = 0.0,
	yOffset = 0.0,
	placedBoxes = <||>
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
				rowMaxHeight = 0,
				extraPadding
			},
				RaiseAssert[PossibleZeroQ[xOffset]];

				(* Add extra padding to each box in this row so that the overall
				   width of this row ends up equal to the width of the naturally
				   widest row. *)
				extraPadding = maxRowWidth - rowWidth;
				extraPadding /= Length[row];
				extraPadding /= 2;

				RaiseAssert[NumberQ[extraPadding]];

				Scan[
					Replace[{
						box_DiaBox :> Module[{
							id = DiagramElementId[box],
							borderRect,
							placedBox
						},
							placedBox = MakePlacedBox[
								box,
								{xOffset, yOffset},
								{
									(* Add extra padding to the X-axis so that
									   each row has the same width. *)
									$BoxPadding + extraPadding,
									(* Use the default Y-axis padding. *)
									$BoxPadding
								}
							];

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

	placedBoxes
]

