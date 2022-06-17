BeginPackage["DiagramMaker`Layout`"]

PlacedBox::usage = "PlacedBox[box, textRect, borderRect]"
PlacedArrow::usage = "PlacedBox[arrow, startPoint, endPoint]"

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]


LayoutDiagram[
	diagram_Diagram,
	algo : _?StringQ : "RowLayout"
] := Replace[algo, {
	"RowLayout" :> doRowLayout[diagram],
	"GraphLayout" :> doGraphLayout[diagram],
	_ :> RaiseError["Unknown diagram layout algorithm: ``", algo]
}]

(*========================================================*)
(* Layout algorithm implementations                       *)
(*========================================================*)

$textWidth = 256.0;
$padding = 8.0;

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
	$margin = 32.0,
	$padding = 8.0,
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
				autoStartSide = Right;
				autoEndSide = Left;

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


End[]

EndPackage[]