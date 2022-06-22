BeginPackage["DiagramMaker`Layouts`Rows`"]

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Utils`"]
Needs["DiagramMaker`Layout`"]
Needs["DiagramMaker`Layout`Utils`"]
Needs["DiagramMaker`Layouts`"]

(* Layout all diagram boxes in a series of rows. *)
DoRowsLayout[
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
				rowWidth = RowWidth[row]
			},
				RaiseAssert[PossibleZeroQ[xOffset]];

				(* Shift this row to the right by the required amount to
				   horizontally center it with whatever the widest row is. *)
				xOffset = (maxRowWidth - rowWidth) / 2.0;

				Scan[
					Replace[{
						box_DiaBox :> Module[{
							id = DiagramElementId[box],
							text = DiagramElementText[box],
							textRect, borderRect,
							placedBox
						},
							{textRect, borderRect} = MakeBoxRectangles[
								text,
								{xOffset, yOffset}
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