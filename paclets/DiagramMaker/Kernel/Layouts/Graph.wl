BeginPackage["DiagramMaker`Layouts`Graph`"]

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Utils`"]
Needs["DiagramMaker`Layout`"]
Needs["DiagramMaker`Layout`Utils`"]
Needs["DiagramMaker`Layouts`"]

(* Layout all diagram boxes based on their Graph[..] layout. *)
DoGraphLayout[
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
			box_DiaBox :> Module[{
				id = DiaElementId[box],
				text = DiaElementText[box],
				borderLeft, borderBottom,
				textRect, borderRect,
				placedBox
			},
				{borderLeft, borderBottom} = {150, 100} * RaiseConfirm @ Lookup[embedding, id];

				{textRect, borderRect} = MakeBoxRectangles[text, {borderLeft, borderBottom}];

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

	placedArrows = PlaceArrowsBasedOnBoxes[arrows, placedBoxes];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	PlacedDiagram[
		placedBoxes,
		placedArrows
	]
]



End[]
EndPackage[]