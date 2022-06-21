BeginPackage["DiagramMaker`Layouts`EqualWidthRows`"]

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Utils`"]
Needs["DiagramMaker`Layout`"]
Needs["DiagramMaker`Layout`Utils`"]
Needs["DiagramMaker`Layouts`"]


DoEqualWidthRowsLayout[
	diagram_Diagram
] := Module[{
	boxes = DiagramBoxes[diagram],
	arrows = DiagramArrows[diagram],
	rows,
	maxRowWidth = 0,
	xOffset = 0.0,
	yOffset = 0.0,
	placedBoxes = <||>,
	placedArrows = {}
},
	rows = GroupBoxesByGraphRow[diagram];

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
							id = DiaElementId[box],
							text = DiaElementText[box],
							textRect, borderRect,
							placedBox
						},
							{textRect, borderRect} = MakeBoxRectangles[
								text,
								{xOffset, yOffset},
								{
									(* Add extra padding to the X-axis so that
									   each row has the same width. *)
									$BoxPadding + extraPadding,
									(* Use the default Y-axis padding. *)
									$BoxPadding
								}
							];

							placedBox = PlacedBox[box, textRect, borderRect];

							xOffset += RectangleWidth[borderRect] + $margin;

							AssociateTo[placedBoxes, id -> placedBox];
						],
						other_ :> RaiseError["unexpected diagram box structure: ``", other]
					}],
					row
				];

				(* Start the next row at the far left. *)
				xOffset = 0;
				(* Place the next row higher up. *)
				(* FIXME: Compute this offset using the maximum height of the
					boxes in the previous row. *)
				yOffset += 100.0;
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



End[]
EndPackage[]