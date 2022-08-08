Package["Diagrams`Layout`Utils`"]

PackageExport[{
	PlaceArrowsBasedOnBoxes,
	GroupBoxesByGraphRow,

	RowWidth,

	LayoutBoxContent,

	PlacedBoxBorderRectangle,

	AbsoluteTranslate,
	RectangleBoundingBox,
	Bounded
}]

GroupBoxesByGraphRow::usage = "GroupBoxesByGraphRow[diagram]"

Bounded::usage = "Bounded[g, rect] represents a placed diagram element g whose content bounding box is rect."

PackageUse[Diagrams -> {
	Diagram, DiagramBoxes, DiagramGraph, DiagramArrowIds, DiagramElementId,
	DiagramElementContent, DiaArrow, DiaBox, RenderedTextSize,
	Errors -> {
		RaiseError, RaiseConfirm, RaiseAssert, RaiseConfirmMatch,
		AddUnmatchedArgumentsHandler
	},
	Render -> SizedText,
	Layout -> {$TextWidth, $BoxPadding, $Margin, PlacedBox, PlacedArrow},
	Utils -> {RectangleWidth, RectangleHeight}
}]

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
	(* Functions *)
	incrSideAttachmentCount,
	getSideLerpFactor
},
	incrSideAttachmentCount[id_?StringQ, side_] := Module[{
		key, value
	},
		key = {id, side};

		value = Lookup[arrowsData, Key @ key, 0];
		value += 1;

		AssociateTo[arrowsData, key -> value];
	];
	incrSideAttachmentCount[args___] := RaiseError["bad args: ", {args}];

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
		arrow |-> Module[{
			startSpec,     endSpec,
			startId,       endId,
			startBox,      endBox,
			autoStartSide, autoEndSide
		},
			{startSpec, endSpec} = Replace[arrow, {
				DiaArrow[lhs_ -> rhs_, ___] :> {lhs, rhs},
				_ :> RaiseError["invalid diagram arrow specification: ", arrow]
			}];

			{startId, endId} = DiagramArrowIds[arrow];

			startBox = Lookup[
				placedBoxes,
				startId,
				RaiseError["arrow start does not refer to a known box: ``", startId]
			];

			endBox = Lookup[
				placedBoxes,
				endId,
				RaiseError["arrow end does not refer to a known box: ``", endId]
			];

			{autoStartSide, autoEndSide} = closestSides[
				PlacedBoxBorderRectangle[startBox],
				PlacedBoxBorderRectangle[endBox],
				sides
			];

			RaiseAssert @ MatchQ[autoStartSide, Left | Right | Top | Bottom];

			(* FIXME:
				The logic in this function for automatically distributing arrow
				attachment points along a side doesn't take into account points
				whose location is determined using more specific specifications
				like {_, Nearest}. *)
			incrSideAttachmentCount[startId, autoStartSide];
			incrSideAttachmentCount[endId, autoEndSide];
		],
		arrows
	];

	(*--------------------------------------*)
	(* Compute the placement of each arrow. *)
	(*--------------------------------------*)

	Map[
		arrow |-> Module[{
			startSpec,     endSpec,
			startId,       endId,
			startBox,      endBox,
			autoStartSide, autoEndSide,
			startAt,       endAt,
			startPoint,    endPoint
		},
			{startSpec, endSpec} = Replace[arrow, {
				DiaArrow[lhs_ -> rhs_, ___] :> {lhs, rhs},
				_ :> RaiseError["invalid diagram arrow specification: ", arrow]
			}];

			{startId, endId} = DiagramArrowIds[arrow];

			startBox = Lookup[
				placedBoxes,
				startId,
				RaiseError["arrow start does not refer to a known box: ``", startId]
			];

			endBox = Lookup[
				placedBoxes,
				endId,
				RaiseError["arrow end does not refer to a known box: ``", endId]
			];

			{autoStartSide, autoEndSide} = closestSides[
				PlacedBoxBorderRectangle[startBox],
				PlacedBoxBorderRectangle[endBox],
				sides
			];

			startAt = {autoStartSide, getSideLerpFactor[startId, autoStartSide]};
			endAt = {autoEndSide, getSideLerpFactor[endId, autoEndSide]};

			startPoint = boxAttachmentPoint[startBox, startAt];
			endPoint = boxAttachmentPoint[endBox, endAt];

			{startPoint, endPoint} = Replace[startSpec -> endSpec, {
				(_?StringQ -> _?StringQ) :> {startPoint, endPoint},
				(_?StringQ -> {_?StringQ, Nearest}) :> {
					startPoint,
					RegionNearest[PlacedBoxBorderRectangle[endBox], startPoint]
				},
				({_?StringQ, Nearest} -> _?StringQ) :> {
					RegionNearest[PlacedBoxBorderRectangle[startBox], endPoint],
					endPoint
				},
				{{lhs_?StringQ, Nearest} -> {rhs_?StringQ, Nearest}} :> RaiseError[
					"unsupported use of {_, Nearest} specification on both diagram arrow sides: ``",
					InputForm[arrow]
				],
				_ :> RaiseError["unsupported diagram arrow attachment specification(s): ``", arrow]
			}];

			RaiseConfirmMatch[{startPoint, endPoint}, {{_?NumberQ, _?NumberQ} ..}];

			PlacedArrow[arrow, startPoint, endPoint]
		],
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
GroupBoxesByGraphRow[diagram_Diagram] := With[{
	boxes = DiagramBoxes[diagram]
},
Module[{
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
		id |-> RaiseConfirm @ Lookup[boxesById, id],
		rows,
		{2}
	];

	RaiseAssert[MatchQ[rows, {{DiaBox[__] ...} ...}]];

	rows
]]

(*========================================================*)
(* Helper functions                                       *)
(*========================================================*)

boxAttachmentPoint[
	PlacedBox[
		_DiaBox,
		placedContent: _,
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

AddUnmatchedArgumentsHandler[boxAttachmentPoint]

(*====================================*)

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
			box_DiaBox :> Module[{
				id = DiagramElementId[box]
			},
				If[KeyMemberQ[boxesById, id],
					RaiseError["boxes list contains conflicting IDs: ``", id];
				];

				AssociateTo[boxesById, id -> box];
			],
			other_ :> RaiseError["unexpected diagram box structure: ``", other]
		}],
		boxes
	];

	boxesById
]

(*====================================*)

LayoutBoxContent[boxContent: _] := LayoutBoxContent[boxContent, {0, 0}]

LayoutBoxContent[
	boxContent0: _,
	{xOffset: _?NumberQ, yOffset: _?NumberQ},
	(* Empty space between the box's text rectangle and the box's border
	   rectangle. *)
	padding0 : _ : Automatic
] := Module[{
	boxContent = Replace[boxContent0, element:Except[_?ListQ] :> {element}],
	xPadding, yPadding,
	elements = {},
	contentBoundingRect,
	borderRect
},
	RaiseConfirmMatch[elements, _?ListQ];

	{xPadding, yPadding} = Replace[padding0, {
		Automatic :> {$BoxPadding, $BoxPadding},
		padding_?NumberQ :> {padding, padding},
		{x_?NumberQ, y_?NumberQ} :> {x, y},
		other_ :> RaiseError["unrecognized rectangle padding specification: ``", other]
	}];

	elements = Map[
		element |-> Replace[element, {
			str_?StringQ | Text[str_?StringQ] :> Module[{textWidth, textHeight, rect},
				{textWidth, textHeight} =
					RaiseConfirm @ RenderedTextSize[str, $TextWidth];

				rect = Rectangle[{0, 0}, {textWidth, textHeight}];
				rect = AbsoluteTranslate[rect, {xPadding, yPadding}];
				rect = AbsoluteTranslate[rect, {xOffset, yOffset}];

				Bounded[{SizedText[str, rect]}, rect]
			],
			Column[columnElements_?ListQ] :> Module[{
				placedColumnElements,
				columnYOffset = 0
			},
				placedColumnElements = Map[
					columnElement |-> Module[{result},
						result = LayoutBoxContent[
							columnElement,
							{xOffset, yOffset + columnYOffset},
							(* Don't include inner box padding. *)
							0
						];
						RaiseConfirmMatch[
							result,
							{
								{Bounded[_?ListQ, _Rectangle]...},
								_Rectangle,
								_Rectangle
							}
						];

						RaiseAssert[result[[2]] === result[[3]]];

						columnYOffset += RectangleHeight[result[[2]]];

						result
					],
					(* NOTE:
						Visit the column items in reverse order so that we can
						calculate offsets in the +Y direction instead of -Y.
						This makes it easier to avoid placing anything beneath
						the Y axis.
					*)
					Reverse[columnElements]
				];

				RaiseConfirmMatch[
					placedColumnElements,
					{{{___Bounded}, _Rectangle, _Rectangle}...}
				];

				boundingBoxes = Part[placedColumnElements, All, 2];
				RaiseConfirmMatch[boundingBoxes, {Rectangle[__]...}];

				placedColumnElements = Part[placedColumnElements, All, 1];

				RaiseConfirmMatch[
					placedColumnElements,
					{{Bounded[{(_SizedText)...}, _Rectangle]...}...}
				];

				placedColumnElements = Part[placedColumnElements, All, 1, 1];

				RaiseConfirmMatch[placedColumnElements, {{(_SizedText)...}...}];

				placedColumnElements = Flatten[placedColumnElements];

				RaiseConfirmMatch[placedColumnElements, {(_SizedText)...}];

				columnBoundingBox = RectangleBoundingBox[boundingBoxes];

				Bounded[placedColumnElements, columnBoundingBox]
			],
			other_ :> RaiseError[
				"Unrecognized diagram box content element: ``",
				InputForm[other]
			]
		}],
		boxContent
	];

	RaiseConfirmMatch[elements, { Bounded[{(_SizedText)...}, _Rectangle]... }];

	contentBoundingRect = RectangleBoundingBox[Part[elements, All, 2]];

	borderRect = Rectangle[
		contentBoundingRect[[1]] - {xPadding, yPadding},
		contentBoundingRect[[2]] + {xPadding, yPadding}
	];

	{elements, contentBoundingRect, borderRect}
]

AddUnmatchedArgumentsHandler[LayoutBoxContent]

(*====================================*)

(* NOTE: This function has been superceded by LayoutBoxContent. Kept for the
	time being in case this algorithm is useful again.
MakeBoxRectangles[elementContent: _] := MakeBoxRectangles[elementContent, {0, 0}]

MakeBoxRectangles[
	elementContent: _,
	{xOffset_, yOffset_},
	(* Empty space between the box's text rectangle and the box's border
	   rectangle. *)
	padding0 : _ : Automatic
] := Module[{
	str = Replace[elementContent, {
		str_?StringQ :> str,
		other_ :> RaiseError[
			"Unsupported diagram element content type: ``",
			InputForm[other]
		]
	}],
	xPadding, yPadding,
	textWidth, textHeight,
	textRect, borderRect
},
	{xPadding, yPadding} = Replace[padding0, {
		Automatic :> {$BoxPadding, $BoxPadding},
		padding_?NumberQ :> {padding, padding},
		{x_?NumberQ, y_?NumberQ} :> {x, y},
		other_ :> RaiseError["unrecognized rectangle padding specification: ``", other]
	}];

	{textWidth, textHeight} =
		RaiseConfirm @ RenderedTextSize[str, $TextWidth];

	(* Note: Add fudge factor to prevent text wrapping done by Skia,
			even though we're using the width it told us. *)
	textWidth = textWidth + 1.0;

	textRect = Rectangle[{0, 0}, {textWidth, textHeight}];
	textRect = AbsoluteTranslate[textRect, {xPadding, yPadding}];
	textRect = AbsoluteTranslate[textRect, {xOffset, yOffset}];

	borderRect = Rectangle[
		{0, 0},
		{
			xPadding + textWidth + xPadding,
			yPadding + textHeight + yPadding
		}
	];
	borderRect = AbsoluteTranslate[borderRect, {xOffset, yOffset}];

	{textRect, borderRect}
]

AddUnmatchedArgumentsHandler[MakeBoxRectangles]
*)

(*====================================*)

(* Returns the total width of `row` if the boxes were layed out next to each
   other in a row with padding of `$BoxPadding` and margin of `$Margin`. *)
RowWidth[row:{___DiaBox}] := Module[{
	boxWidths,
	totalMargin
},
	(* Get the border rect width for each box in this row. *)
	boxWidths = Map[
		box |-> Replace[LayoutBoxContent[DiagramElementContent[box]], {
			{_?ListQ, contentRect_Rectangle, borderRect_Rectangle} :> RectangleWidth[borderRect],
			other_ :> RaiseError["Unexpected LayoutBoxContent return value: ``", other]
		}],
		row
	];

	RaiseAssert[MatchQ[boxWidths, {___?NumberQ}], "boxWidths: ``", boxWidths];

	totalMargin = $Margin * (Length[row] - 1);

	Total[boxWidths] + totalMargin
]

(*====================================*)

PlacedBoxBorderRectangle[
	PlacedBox[
		_DiaBox,
		_,
		_Rectangle, (* Content bounding rectangle. *)
		borderRect_Rectangle
	]
] := borderRect

AddUnmatchedArgumentsHandler[PlacedBoxBorderRectangle]

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

(*====================================*)

RectangleBoundingBox[rect_Rectangle] := rect;

RectangleBoundingBox[rects:{___Rectangle}] := Module[{
	minX, minY, maxX, maxY
},
	RaiseConfirmMatch[
		rects,
		{Rectangle[{_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ}]...}
	];

	minX = Min @ Part[rects, All, 1, 1];
	minY = Min @ Part[rects, All, 1, 2];
	maxX = Max @ Part[rects, All, 2, 1];
	maxY = Max @ Part[rects, All, 2, 2];

	Rectangle[{minX, minY}, {maxX, maxY}]
]

AddUnmatchedArgumentsHandler[RectangleBoundingBox]