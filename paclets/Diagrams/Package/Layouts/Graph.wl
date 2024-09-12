Package["Diagrams`Layouts`Graph`"]

PackageUse[Diagrams -> {
	Diagram, DiaBox, DiaArrow, DiagramElementContent, DiagramElementId,
	PlacedDiagram, DiagramGraph,
	Layouts -> DoGraphLayout,
	Layout -> {
		PlacedBox,
		LayoutUtils -> {
			MakePlacedBox, RowWidth,
			GroupBoxesByGraphRow
		}
	},
	Utils -> {RectangleWidth, RectangleHeight},
	Errors -> {RaiseError, RaiseAssert, RaiseConfirm, SetFallthroughError}
}]

(*========================================================*)

SetFallthroughError[DoGraphLayout]

(* Layout all diagram boxes based on their Graph[..] layout. *)
DoGraphLayout[
	boxes0: _List,
	arrows0: _List
] := Module[{
	boxes = boxes0,
	arrows = arrows0,
	graph,
	embedding,
	vertices,
	placedBoxes = <||>
},
	graph = DiagramGraph[boxes, arrows];
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
				id = DiagramElementId[box],
				borderLeft, borderBottom,
				placedBox
			},
				{borderLeft, borderBottom} = {150, 100} * RaiseConfirm @ Lookup[embedding, id];

				placedBox = MakePlacedBox[box, {borderLeft, borderBottom}];

				AssociateTo[placedBoxes, id -> placedBox];
			],
			other_ :> RaiseError["unexpected diagram box structure: ``", other]
		}],
		boxes
	];

	placedBoxes
]
