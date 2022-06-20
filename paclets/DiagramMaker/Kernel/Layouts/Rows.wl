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
	diagram:Diagram[
		boxes:{___DiaBox},
		arrows:{___DiaArrow},
		___?OptionQ
	]
] := Module[{
	rows,
	boxesById = makeBoxesById[boxes],
	maxRowWidth = 0,
	xOffset = 0.0,
	yOffset = 0.0,
	placedBoxes = <||>,
	placedArrows = {}
},
	rows = Module[{
		graph,
		embedding,
		vertices
	},
		graph = DiagramGraph[diagram];
		RaiseAssert[GraphQ[graph]];

		embedding = GraphEmbedding[
			graph,
			(* FIXME: Support this as a GraphLayout option. *)
			{"LayeredEmbedding"}
		];
		vertices = VertexList[graph];

		RaiseAssert[
			MatchQ[embedding, {{_?NumberQ, _?NumberQ} ...}],
			"unexpected embedding value: ``",
			embedding
		];
		RaiseAssert[MatchQ[vertices, {___?StringQ}]];
		RaiseAssert[Length[embedding] === Length[vertices]];

		embedding = Association[Rule @@@ Transpose[{vertices, embedding}]];

		RaiseAssert[MatchQ[embedding, <| (_?StringQ -> {_, _}) ...|>]];

		(* Group the vertices by their Y coordinate. The "LayeredEmbedding"
		   tries to put nodes on parallel horizontal lines, so this will give us
		   rows. *)
		rows = GroupBy[embedding, Last];
		rows = Map[Keys, Values[KeySort[rows]]];

		rows
	];

	RaiseAssert[MatchQ[rows, {{___?StringQ} ...}]];

	rows = Map[
		id |-> Lookup[boxesById, id, RaiseError["FIXME"]],
		rows,
		{2}
	];

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
							id = DiaElementId[box],
							text = DiaElementText[box],
							textRect, borderRect,
							placedBox
						},
							{textRect, borderRect} = makeBoxRectangles[
								text,
								{xOffset, yOffset}
							];

							placedBox = PlacedBox[box, textRect, borderRect];

							xOffset += RectangleWidth[placedBox[[2]]] + $margin;

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