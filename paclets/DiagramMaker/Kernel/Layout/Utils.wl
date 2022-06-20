BeginPackage["DiagramMaker`Layout`Utils`"]

PlaceArrowsBasedOnBoxes

GroupBoxesByGraphRow::usage = "GroupBoxesByGraphRow[diagram]"

RowWidth
MakeBoxRectangles


Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Layout`"]
Needs["DiagramMaker`Utils`"]


(*======================================*)

PlaceArrowsBasedOnBoxes[
	arrows:{___DiaArrow},
	placedBoxes_,
	sides : _ : Automatic
] := Module[{
	(*
		<|
			(* The number of arrows that start or end at this side of the box's
			   border. *)
			{"BoxId", Left} -> _?IntegerQ,
			...
		|>
	*)
	arrowsData = <||>,
	(* This Association has the same structure as arrowsData, but counts up from
	   0 as the layout is computed. *)
	arrowsDataCounts = <||>,
	placedArrows = {},
	getSideLerpFactor
},
	getSideLerpFactor[boxId_?StringQ, side_] := Module[{
		count = Lookup[arrowsDataCounts, Key @ {boxId, side}, 0],
		total = RaiseConfirm @ Lookup[arrowsData, Key @ {boxId, side}]
	},
		(* Increase the lerp factor so that the next arrow that starts or ends
		   at this side will not touch the same point. *)
		AssociateTo[arrowsDataCounts, {boxId, side} -> count + 1];

		(count + 1) / (total + 1)
	];

	(*---------------------------------------------------------*)
	(* Populate `arrowsData` with information about the number *)
	(* of arrows that connect to each side of each box.        *)
	(*---------------------------------------------------------*)

	Scan[
		Replace[{
			arrow:DiaArrow[
				start_?StringQ -> end_?StringQ,
				___
			] :> Module[{
				startBox, endBox,
				autoStartSide, autoEndSide,
				startKey, endKey,
				startValue, endValue
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

				{autoStartSide, autoEndSide} = closestSides[
					startBox[[3]],
					endBox[[3]],
					sides
				];

				startKey = {start, autoStartSide};
				endKey = {end, autoEndSide};

				startValue = Lookup[arrowsData, Key @ startKey, 0];
				endValue = Lookup[arrowsData, Key @ endKey, 0];

				startValue += 1;
				endValue += 1;

				AssociateTo[arrowsData, startKey -> startValue];
				AssociateTo[arrowsData, endKey -> endValue];
			],
			other_ :> RaiseError["unexpected diagram arrow structure: ``", other]
		}],
		arrows
	];

	(*--------------------------------------*)
	(* Compute the placement of each arrow. *)
	(*--------------------------------------*)

	Map[
		Replace[{
			arrow:DiaArrow[
				start_?StringQ -> end_?StringQ,
				___
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

				{autoStartSide, autoEndSide} = closestSides[
					startBox[[3]],
					endBox[[3]],
					sides
				];

				startAt = {autoStartSide, getSideLerpFactor[start, autoStartSide]};
				endAt = {autoEndSide, getSideLerpFactor[end, autoEndSide]};

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

(*======================================*)

(* Group the elements of `boxes` by the row they occupy in a Graph layout. *)
(*
	Many Graph[..] layout algorithms will lay out graph nodes along shared
	horizontal lines. Graph layout algorithms that do not quantize nodes to
	parallel lines are not compatible with this function.

	This function is a slightly kludgy way of letting Graph[..] to the heavy
	lifting for a first approximation of the diagram layout, which can then
	further refined based on e.g. box sizes.
*)
GroupBoxesByGraphRow[
	diagram:Diagram[
		boxes:{___DiaBox},
		{___DiaArrow},
		___?OptionQ
	]
] := Module[{
	rows,
	boxesById = makeBoxesById[boxes]
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

	rows
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
	x, y
},
	point = Replace[attachment, {
		{
			side : (Left | Right | Top | Bottom),
			(* Linear interpolation factor. *)
			lerpFactor_?NumberQ
		} :> (
			x = Replace[side, {
				Left :> borderLeft,
				Right :> borderRight,
				Top | Bottom :> borderLeft + lerpFactor * RectangleWidth[borderRect]
			}];

			y = Replace[side, {
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
	b_Rectangle,
	(* The sides that will be considered as possible attachment points. *)
	sides0 : (Automatic | _?ListQ) : Automatic
] := Module[{
	sides,
	best
},
	sides = Replace[sides0, Automatic -> {Left, Right, Top, Bottom}];
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

MakeBoxRectangles[str_?StringQ] := MakeBoxRectangles[str, {0, 0}]

MakeBoxRectangles[
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

(* Returns the total width of `row` if the boxes were layed out next to each
   other in a row with padding of `$padding`. *)
RowWidth[row:{___DiaBox}] := Module[{
	boxWidths,
	totalPadding
},
	(* Get the border rect width for each box in this row. *)
	boxWidths = Map[
		box |-> RectangleWidth[MakeBoxRectangles[DiaElementText[box]][[2]]],
		row
	];

	RaiseAssert[MatchQ[boxWidths, {___?NumberQ}], "boxWidths: ``", boxWidths];

	totalPadding = $padding * (Length[row] - 1);

	Total[boxWidths] + totalPadding
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