BeginPackage["DiagramMaker`"]

(* Declare your package's public symbols here. *)

DiagramBlock::usage = "Represents a block element in a diagram."
DiagramArrow::usage = "Represents an arrow defining a relationship between two diagram block elements."

RenderDiagram::usage = "Renders a diagram to a graphical represenation."
DiagramGraph::usage = "Returns the Graph object representing the relations between graph blocks."

Begin["`Private`"]

$functions = LibraryFunctionLoad[
	"libdiagram_maker_wll",
	"load_diagram_maker_functions",
	LinkObject,
	LinkObject
][];

Assert[MatchQ[$functions, <| (_?StringQ -> _)... |>]];

(*--------------------------------------------------------------------------------------*)

Subgraphs[graph_Graph] := Map[
	Subgraph[graph, #, VertexLabels -> "Name"] &,
	ConnectedComponents[UndirectedGraph[graph]]
]

(*--------------------------------------------------------------------------------------*)

RenderDiagram[
	DiagramBlock[name_?StringQ, blocks : {___}, relations : {___}]
] := Framed[
	Column[{
		Item[Style[name, "Title"], Alignment -> Center]
	}],
	RoundingRadius -> 5,
	Background -> GrayLevel[0.95]
]

(*--------------------------------------------------------------------------------------*)

(* renderVertex[vertex_] := Framed[Style[vertex, Bold]] *)

renderVertex[{xc_, yc_}, name_, {w_, h_}] := Block[{
	xmin = xc - w,
	xmax = xc + w,
	ymin = yc - h,
	ymax = yc + h
},
	{
		Text[Style[name, Bold], {xc, yc}],

		GrayLevel[0.95],
		Rectangle[{xmin, ymin}, {xmax, ymax}]
	}
   (* Polygon[{{xmin, ymin}, {xmax, ymax}, {xmin, ymax}, {xmax, ymin}}] *)
]

DiagramGraph[
  	block0 : DiagramBlock[name0_?StringQ, blocks0 : {___}, {___}]
] := Module[{
	blocksByName,
	relations,
	relationLabels
},
	blocksByName = Association @ Cases[
		Prepend[blocks0, block0],
		(block : DiagramBlock[name_?StringQ, ___]) -> (name -> block),
		Infinity
	];

	{relationLabels, relations} = Transpose @ Cases[
		Prepend[blocks0, block0],
		DiagramArrow[label_?StringQ, lhs_?StringQ -> rhs_?StringQ, opts___] -> {(lhs -> rhs) -> label, lhs -> rhs},
		Infinity
	];

	Print["relationLabels: ", relationLabels];

	Graph[
		Keys[blocksByName],
		relations,
		(* GraphLayout -> "SpringEmbedding", *)
		EdgeLabels -> relationLabels
		(* EdgeShapeFunction -> GraphElementData[{"DashedLine", "ArrowSize" -> 0.05}], *)
		(* VertexLabels -> "Name", *)
		(* VertexShapeFunction -> renderVertex *)
	]

	(* Print["blocksByName: ", blocksByName]; *)

	(* todo[] *)
]


End[] (* End `Private` *)

EndPackage[]
