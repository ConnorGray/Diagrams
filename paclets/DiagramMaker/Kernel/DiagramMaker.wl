BeginPackage["DiagramMaker`"]

(* Declare your package's public symbols here. *)

Diagram::usage = "Diagram[{elements}] represents a diagram composed of elements."

DiaBox::usage = "DiaBox[..] is a diagram element of a generic box."
DiaArrow::usage = "DiaArrow[..] is a diagram element that defines a relationship between two diagram box elements."

DiagramImage::usage = "DiagramImage[diagram] returns an image containing the graphical representation of diagram."
DiagramGraph::usage = "DiagramGraph[diagram] returns a Graph object representing the relations between diagram elements blocks."

DiagramGraphicsImage

Begin["`Private`"]

$functions = LibraryFunctionLoad[
	"libdiagram_maker_wll",
	"load_diagram_maker_functions",
	LinkObject,
	LinkObject
][];

Assert[MatchQ[$functions, <| (_?StringQ -> _)... |>]];

(*----------------------------------------------------------------------------*)

Diagram /: MakeBoxes[
	obj : Diagram[boxes:{___}, arrows:{___}, opts___?OptionQ],
	form : StandardForm
] := Module[{icon},
	(* icon = Thumbnail[DiagramImage[obj]]; *)
	BoxForm`ArrangeSummaryBox[
		Diagram,
		obj,
		ImageCrop[DiagramImage[obj]],
		{
			BoxForm`SummaryItem[{"Boxes: ", Length[boxes]}],
			BoxForm`SummaryItem[{"Arrows: ", Length[arrows]}]
		},
		{},
		form
	]
]

(*----------------------------------------------------------------------------*)

Subgraphs[graph_Graph] := Map[
	Subgraph[graph, #, VertexLabels -> "Name"] &,
	ConnectedComponents[UndirectedGraph[graph]]
]

(*----------------------------------------------------------------------------*)

DiagramImage[args___] := Module[{result},
	result = $functions["diagram_image"][args];

	Replace[result, {
		bytes:{___?IntegerQ} :> ImportByteArray[ByteArray[bytes], "PNG"]
	}]
]

(*----------------------------------------------------------------------------*)

DiagramGraphicsImage[args___] := Module[{result},
	result = $functions["graphics_image"][args];

	Replace[result, {
		bytes:{___?IntegerQ} :> ImportByteArray[ByteArray[bytes], "PNG"]
	}]
]

(*----------------------------------------------------------------------------*)

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
		DiagramArrow[label_?StringQ, lhs_?StringQ -> rhs_?StringQ, opts___]
			-> {(lhs -> rhs) -> label, lhs -> rhs},
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
