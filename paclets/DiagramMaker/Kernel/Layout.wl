BeginPackage["DiagramMaker`Layout`"]

PlacedBox::usage = "PlacedBox[box, textRect, borderRect]"
PlacedArrow::usage = "PlacedBox[arrow, startPoint, endPoint]"

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]


LayoutDiagram[
	diagram_Diagram,
	algo : _?StringQ : "RowsLayout"
] := Replace[algo, {
	"RowLayout" :> doRowLayout[diagram],
	"RowsLayout" :> doRowsLayout[diagram],
	"GraphLayout" :> doGraphLayout[diagram],
	_ :> RaiseError["Unknown diagram layout algorithm: ``", algo]
}]

(*========================================================*)
(* Layout algorithm implementations                       *)
(*========================================================*)

$textWidth = 300.0;
$padding = 8.0;
$margin = 32.0;

(*====================================*)
(* Row Layout                         *)
(*====================================*)

(* Layout all diagram boxes in a single row. *)
doRowLayout[
	Diagram[
		boxes:{___DiaBox},
		arrows:{___DiaArrow}
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
			box:DiaBox[id_?StringQ] :> Module[{
				borderLeft, borderRight,
				textLeft, textRight,
				textWidth, textHeight,
				placedBox
			},
				borderLeft = xOffset;
				textLeft = xOffset + $padding;

				{textWidth, textHeight} =
					RaiseConfirm @ RenderedTextSize[id, $textWidth];

				(* Note: Add fudge factor to prevent text wrapping done by Skia,
				         even though we're using the width it told us. *)
				textWidth = textWidth + 1.0;

				textRight = textLeft + textWidth;
				borderRight = textRight + $padding;

				placedBox = PlacedBox[
					box,
					(* Text rectangle *)
					Rectangle[{textLeft, $padding}, {textRight, $padding + textHeight}],
					(* Border rectangle *)
					Rectangle[{borderLeft, 0.0}, {borderRight, $padding + textHeight + $padding}]
				];

				xOffset += rectangleWidth[placedBox[[2]]] + $margin;

				AssociateTo[placedBoxes, id -> placedBox];
			],
			other_ :> RaiseError["unexpected diagram box structure: ``", other]
		}],
		boxes
	];

	(*--------------*)
	(* Place arrows *)
	(*--------------*)

	placedArrows = placeArrowsBasedOnBoxes[arrows, placedBoxes];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	PlacedDiagram[
		placedBoxes,
		placedArrows
	]
]

(*====================================*)
(* Rows Layout                        *)
(*====================================*)

(* Layout all diagram boxes in a series of rows. *)
doRowsLayout[
	diagram:Diagram[
		boxes:{___DiaBox},
		arrows:{___DiaArrow}
	]
] := Module[{
	rows,
	boxesById = makeBoxesById[boxes],
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
		rows = Map[Keys, Values[Sort[rows]]];

		Reverse[rows]
	];

	RaiseAssert[MatchQ[rows, {{___?StringQ} ...}]];

	(*-------------*)
	(* Place boxes *)
	(*-------------*)

	Scan[
		Replace[{
			row:{___?StringQ} :> Module[{},
				Scan[
					Replace[{
						id_?StringQ :> Module[{
							box,
							borderLeft, borderRight,
							textLeft, textRight,
							textWidth, textHeight,
							placedBox
						},
							box = Lookup[boxesById, id, RaiseError["FIXME"]];

							RaiseAssert[MatchQ[box, DiaBox[id]]];

							borderLeft = xOffset;
							textLeft = xOffset + $padding;

							{textWidth, textHeight} =
								RaiseConfirm @ RenderedTextSize[id, $textWidth];

							(* Note: Add fudge factor to prevent text wrapping done by Skia,
									even though we're using the width it told us. *)
							textWidth = textWidth + 1.0;

							textRight = textLeft + textWidth;
							borderRight = textRight + $padding;

							placedBox = PlacedBox[
								box,
								(* Text rectangle *)
								Rectangle[
									{textLeft, yOffset + $padding},
									{textRight, yOffset + $padding + textHeight}
								],
								(* Border rectangle *)
								Rectangle[
									{borderLeft, yOffset},
									{borderRight, yOffset + $padding + textHeight + $padding}
								]
							];

							xOffset += rectangleWidth[placedBox[[2]]] + $margin;

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

	placedArrows = placeArrowsBasedOnBoxes[arrows, placedBoxes];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	PlacedDiagram[
		placedBoxes,
		placedArrows
	]
]

(*====================================*)
(* Graph Layout                       *)
(*====================================*)

(* Layout all diagram boxes based on their Graph[..] layout. *)
doGraphLayout[
	diagram:Diagram[
		boxes:{___DiaBox},
		arrows:{___DiaArrow}
	]
] := Module[{
	graph,
	embedding,
	vertices,
	placedBoxes = <||>,
	placedArrows = {}
},
	graph = DiagramGraph[diagram];
	RaiseAssert[GraphQ[graph]];

	embedding = GraphEmbedding[
		graph,
		(* FIXME: Support this as a GraphLayout option. *)
		{"LayeredEmbedding"}
	];
	vertices = VertexList[graph];

	RaiseAssert[MatchQ[embedding, {{_?NumberQ, _?NumberQ} ...}]];
	RaiseAssert[MatchQ[vertices, {___?StringQ}]];
	RaiseAssert[Length[embedding] === Length[vertices]];

	embedding = Association[Rule @@@ Transpose[{vertices, embedding}]];

	RaiseAssert[MatchQ[embedding, <| (_?StringQ -> {_, _}) ...|>]];

	(*-------------*)
	(* Place boxes *)
	(*-------------*)

	Scan[
		Replace[{
			box:DiaBox[id_?StringQ] :> Module[{
				embeddingPos,
				borderLeft, borderRight,
				textLeft, textRight,
				textWidth, textHeight,
				placedBox
			},
				{borderLeft, borderBottom} = {150, 100} * Lookup[embedding, id, RaiseError["FIXME"]];

				textLeft = borderLeft + $padding;

				{textWidth, textHeight} =
					RaiseConfirm @ RenderedTextSize[id, $textWidth];

				(* Note: Add fudge factor to prevent text wrapping done by Skia,
				         even though we're using the width it told us. *)
				textWidth = textWidth + 1.0;

				textRight = textLeft + textWidth;
				borderRight = textRight + $padding;

				placedBox = PlacedBox[
					box,
					(* Text rectangle *)
					Rectangle[
						{textLeft, borderBottom + $padding},
						{textRight, borderBottom + $padding + textHeight}
					],
					(* Border rectangle *)
					Rectangle[
						{borderLeft, borderBottom},
						{borderRight, borderBottom + $padding + textHeight + $padding}
					]
				];

				AssociateTo[placedBoxes, id -> placedBox];
			],
			other_ :> RaiseError["unexpected diagram box structure: ``", other]
		}],
		boxes
	];

	(*--------------*)
	(* Place arrows *)
	(*--------------*)

	placedArrows = placeArrowsBasedOnBoxes[arrows, placedBoxes];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	PlacedDiagram[
		placedBoxes,
		placedArrows
	]
]

(*====================================*)
(* Common                             *)
(*====================================*)

placeArrowsBasedOnBoxes[
	arrows:{___DiaArrow},
	placedBoxes_
] := Module[{},
	Map[
		Replace[{
			arrow:DiaArrow[
				start_?StringQ -> end_?StringQ,
				label_?StringQ
			] :> Module[{
				startBox,
				endBox,
				autoStartSide,
				autoEndSide,
				startAt,
				endAt,
				startPoint,
				endPoint
			},
				startBox = Lookup[
					placedBoxes,
					start,
					RaiseError["arrow start does not refer to a known box: ``", start]
				];

				endBox = Lookup[
					placedBoxes,
					end,
					RaiseError["arrow end does not refer to a known box: ``", end]
				];

				(* FIXME: Use the `closest_sides()` algorithm for this. *)
				{autoStartSide, autoEndSide} = closestSides[
					startBox[[3]],
					endBox[[3]]
				];

				startAt = autoStartSide;
				endAt = autoEndSide;

				startPoint = boxAttachmentPoint[startBox, startAt];
				endPoint = boxAttachmentPoint[endBox, endAt];

				PlacedArrow[
					arrow,
					startPoint,
					endPoint
				]
			],
			other_ :> RaiseError["unexpected diagram arrow structure: ``", other]
		}],
		arrows
	]
]

(*========================================================*)
(* Helper functions                                       *)
(*========================================================*)

boxAttachmentPoint[
	PlacedBox[
		_,
		_Rectangle,
		borderRect:Rectangle[
			{borderLeft_, borderBottom_},
			{borderRight_, borderTop_}
		]
	],
	attachment_
] := Module[{
	point,
	(* TODO: support {side_, lerpFactor_} as a specification. *)
	(* Linear interpolation factor. *)
	lerpFactor = 0.5,
	x, y
},
	point = Replace[attachment, {
		Left | Right | Top | Bottom :> (
			x = Replace[attachment, {
				Left :> borderLeft,
				Right :> borderRight,
				Top | Bottom :> borderLeft + lerpFactor * rectangleWidth[borderRect]
			}];

			y = Replace[attachment, {
				Left | Right :> borderBottom + lerpFactor * rectangleHeight[borderRect],
				Top :> borderTop,
				Bottom :> borderBottom
			}];

			{x, y}
		),
		other_ :> RaiseError["unsupported attachment specification: ``", other]
	}];

	RaiseAssert[MatchQ[point, {_?NumberQ, _?NumberQ}]];

	point
]

closestSides[
	a_Rectangle,
	b_Rectangle
] := Module[{
	sides,
	best
},
	sides = {Left, Right, Top, Bottom};

	best = {Infinity, {Left, Left}};

	Do[
		Module[{
			aPoints = rectangleSidePoints[a, aSide],
			bPoints = rectangleSidePoints[b, bSide],
			distance
		},
			RaiseAssert[MatchQ[
				aPoints,
				{{_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ}}
			]];

			distance = sidesDistanceFactor[aPoints, bPoints];

			RaiseAssert[NumberQ[distance]];

			If[distance < best[[1]],
				best = {distance, {aSide, bSide}};
			];
		],
		{aSide, sides},
		{bSide, sides}
	];

	best[[2]]
]

makeBoxesById[boxes:{___DiaBox}] := Module[{
	boxesById = <||>
},
	Scan[
		Replace[{
			box:DiaBox[id_?StringQ] :> (
				If[KeyMemberQ[boxesById, id],
					RaiseError["boxes list contains conflicting IDs: ``", id];
				];

				AssociateTo[boxesById, id -> box];
			),
			other_ :> RaiseError["unexpected diagram box structure: ``", other]
		}],
		boxes
	];

	boxesById
]

(*====================================*)
(* Utility functions                  *)
(*====================================*)

rectangleWidth[arg_] := Replace[arg, {
	Rectangle[{xMin_?NumberQ, _}, {xMax_?NumberQ, _}] :> Abs[xMax - xMin],
	_ :> RaiseError["unable to get width of rectangle: ``", arg]
}]

rectangleHeight[arg_] := Replace[arg, {
	Rectangle[{_, yMin_?NumberQ}, {_, yMax_?NumberQ}] :> Abs[yMax - yMin],
	_ :> RaiseError["unable to get height of rectangle: ``", arg]
}]

rectangleSidePoints[
	rect:Rectangle[{left_, bottom_}, {right_, top_}],
	side_
] := Module[{
	topLeft = {left, top},
	topRight = {right, top},
	bottomLeft = {left, bottom},
	bottomRight = {right, bottom}
},
	Replace[side, {
		Left :> {bottomLeft, topLeft},
		Right :> {bottomRight, topRight},
		Top :> {topLeft, topRight},
		Bottom :> {bottomLeft, bottomRight},
		other_ :> RaiseError["unrecognized rectangle side specification: ``", other]
	}]
]

(****************************************)

sidesDistanceFactor[
	{a0:{_, _}, a1:{_, _}},
	{b0:{_, _}, b1:{_, _}}
] := Plus[
	EuclideanDistance[a0, b0],
	EuclideanDistance[a0, b1],
	EuclideanDistance[a1, b0],
	EuclideanDistance[a1, b1]
]

End[]

EndPackage[]