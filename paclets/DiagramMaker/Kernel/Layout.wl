BeginPackage["DiagramMaker`Layout`"]

PlacedBox::usage = "PlacedBox[box, textRect, borderRect]"
PlacedArrow::usage = "PlacedBox[arrow, startPoint, endPoint]"

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Utils`"]


LayoutDiagram[
	diagram_Diagram,
	algo0 : _?StringQ : Automatic
] := Module[{
	diagramOpts = Options[diagram],
	algo,
	result
},
	algo = Replace[algo0, Automatic :> OptionValue[Diagram, diagramOpts, DiagramLayout]];

	(* Default to "RowsLayout". *)
	algo = Replace[algo, Automatic :> "RowsLayout"];

	result = Replace[algo, {
		"RowLayout" :> doRowLayout[diagram],
		"RowsLayout" :> doRowsLayout[diagram],
		"GraphLayout" :> doGraphLayout[diagram],
		_ :> RaiseError["Unknown diagram layout algorithm: ``", algo]
	}];

	RaiseAssert[
		MatchQ[result, PlacedDiagram[_?AssociationQ, _?ListQ]],
		"diagram layout implementation for `` did not return expected result: ``",
		InputForm[algo],
		InputForm[result]
	];

	result
]

(*------------------------------------*)

LayoutDiagram[args___] :=
	RaiseError["unexpected arguments to LayoutDiagram: ``", InputForm[{args}]]

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
			box:DiaBox[id_?StringQ] :> Module[{
				textRect, borderRect,
				placedBox
			},
				{textRect, borderRect} = makeBoxRectangles[id, {xOffset, 0}];

				placedBox = PlacedBox[box, textRect, borderRect];

				xOffset += RectangleWidth[placedBox[[2]]] + $margin;

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
		arrows:{___DiaArrow},
		___?OptionQ
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
		rows = Map[Keys, Values[KeySort[rows]]];

		rows
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
							textRect, borderRect,
							placedBox
						},
							box = Lookup[boxesById, id, RaiseError["FIXME"]];

							RaiseAssert[MatchQ[box, DiaBox[id]]];

							{textRect, borderRect} = makeBoxRectangles[
								id,
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
		arrows:{___DiaArrow},
		___?OptionQ
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
				borderLeft, borderBottom,
				textRect, borderRect,
				placedBox
			},
				{borderLeft, borderBottom} = {150, 100} * Lookup[embedding, id, RaiseError["FIXME"]];

				{textRect, borderRect} = makeBoxRectangles[id, {borderLeft, borderBottom}];

				placedBox = PlacedBox[box, textRect, borderRect];

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
				Top | Bottom :> borderLeft + lerpFactor * RectangleWidth[borderRect]
			}];

			y = Replace[attachment, {
				Left | Right :> borderBottom + lerpFactor * RectangleHeight[borderRect],
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

makeBoxRectangles[
	str_?StringQ,
	{xOffset_, yOffset_}
] := Module[{
	textWidth, textHeight,
	textRect, borderRect
},
	{textWidth, textHeight} =
		RaiseConfirm @ RenderedTextSize[str, $textWidth];

	(* Note: Add fudge factor to prevent text wrapping done by Skia,
			even though we're using the width it told us. *)
	textWidth = textWidth + 1.0;

	textRect = Rectangle[{0, 0}, {textWidth, textHeight}];
	textRect = AbsoluteTranslate[textRect, {$padding, $padding}];
	textRect = AbsoluteTranslate[textRect, {xOffset, yOffset}];

	borderRect = Rectangle[
		{0, 0},
		{
			$padding + textWidth + $padding,
			$padding + textHeight + $padding
		}
	];
	borderRect = AbsoluteTranslate[borderRect, {xOffset, yOffset}];

	{textRect, borderRect}
]

(*====================================*)
(* Utility functions                  *)
(*====================================*)

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

(****************************************)

AbsoluteTranslate[
	primitives_?ListQ,
	vector:{_, _}
] := Map[
	AbsoluteTranslate[#, vector] &,
	primitives
]

AbsoluteTranslate[
	Line[pts:{{Except[_List], Except[_List]} ...}],
	vector:{_, _}
] :=
	Line[pts + vector]

AbsoluteTranslate[
	Rectangle[min:{_, _}, max:{_, _}, opts___],
	vector:{_, _}
] :=
	Rectangle[min + vector, max + vector, opts]


End[]

EndPackage[]