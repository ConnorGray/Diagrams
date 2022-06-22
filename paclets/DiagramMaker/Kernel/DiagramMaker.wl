BeginPackage["DiagramMaker`"]

(* Declare your package's public symbols here. *)

Diagram::usage = "Diagram[{elements}] represents a diagram composed of elements."
PlacedDiagram::usage = "PlacedDiagram[boxes, arrows]"

DiaBox::usage = "DiaBox[..] is a diagram element of a generic box."
DiaArrow::usage = "DiaArrow[..] is a diagram element that defines a relationship between two diagram box elements."

DiagramImage::usage = "DiagramImage[diagram] returns an image containing the graphical representation of diagram."
DiagramGraph::usage = "DiagramGraph[diagram] returns a Graph object representing the relations between diagram elements blocks."

LayoutDiagram::usage = "LayoutDiagram[diagram] uses a suitable layout algorithm to produce a PlacedDiagram"

DiagramLayout::usage = "DiagramLayout is an option to Diagram and related functions that specifies what layout to use."
DiagramTheme::usage  = "DiagramTheme is an option to Diagram and related functions that specifies an overall theme for visualization elements and styles."

RenderPlacedDiagramToGraphics

DiagramGraphicsImage

RenderedTextSize

DiagramTitle::usage = "DiagramTitle[diagram]"
DiagramBoxes::usage = "DiagramBoxes[diagram]"
DiagramArrows::usage = "DiagramArrows[diagram]"

DiagramElementId::usage = "DiagramElementId[elem] will return the unique identifer associated with a diagram element."
DiagramElementText::usage = "DiagramElementText[elem] will return the textual description associated with a diagram element, if applicable."

(*--------*)
(* Errors *)
(*--------*)

DiagramMaker::error = "``"
DiagramMaker::assertfail = "``"

Begin["`Private`"]


Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Layout`"]
Needs["DiagramMaker`Render`"]
Needs["DiagramMaker`Utils`"]


$functions = LibraryFunctionLoad[
	"libdiagram_maker_wll",
	"load_diagram_maker_functions",
	LinkObject,
	LinkObject
][];

Assert[MatchQ[$functions, <| (_?StringQ -> _)... |>]];

(*----------------------------------------------------------------------------*)

Options[Diagram] = {
	DiagramLayout -> Automatic,
	DiagramTheme -> Automatic
};

Diagram /: MakeBoxes[
	obj : Diagram[boxes:{___}, arrows:{___}, opts___?OptionQ],
	form : StandardForm
] := Module[{icon},
	(* icon = Thumbnail[DiagramImage[obj]]; *)
	BoxForm`ArrangeSummaryBox[
		Diagram,
		obj,
		DiagramImage[obj],
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

Options[DiagramImage] = {Method -> Automatic}

DiagramImage[diagram_Diagram, OptionsPattern[]] := Replace[OptionValue[Method], {
	Automatic :> Module[{
		theme = RaiseConfirm @ Lookup[Options[diagram], DiagramTheme, Automatic],
		placed,
		graphics,
		title = DiagramTitle[diagram],
		center
	},
		placed = LayoutDiagram[diagram];
		graphics = RenderPlacedDiagramToGraphics[placed, theme];

		RaiseAssert[ListQ[graphics]];

		graphics = ReplaceAll[graphics,
			SizedText[str_?StringQ, rect:Rectangle[min_, max_]] :> (
				center = Mean[{min, max}];
				size = RectangleSize[rect];

				(* FIXME:
					Compute this font size based on the overall size of
					the graphic. *)
				Splice[{
					FontSize -> Scaled[0.05],
					Inset[Text[str], center, Automatic, size]
				}]
			)
		];

		If[StringQ[title],
			Labeled[
				Graphics[graphics],
				title,
				Top,
				Frame -> True,
				FrameMargins -> 10,
				Background -> LightGray,
				RoundingRadius -> 6,
				Spacings -> 4
			]
			,
			Graphics[graphics]
		]
	],
	"alpha-v2" :> Module[{placed, graphics},
		placed = LayoutDiagram[diagram];
		graphics = RenderPlacedDiagramToGraphics[placed];

		DiagramGraphicsImage[Graphics[N @ Flatten @ graphics]]
	],
	"alpha-v1" :> Module[{result},
		result = $functions["diagram_image"][diagram];

		Replace[result, {
			bytes:{___?IntegerQ} :> ImageCrop @ ImportByteArray[ByteArray[bytes], "PNG"]
		}]
	]
}]

(*----------------------------------------------------------------------------*)

DiagramGraphicsImage[args___] := Module[{result},
	result = $functions["graphics_image"][args];

	Replace[result, {
		bytes:{___?IntegerQ} :> ImageCrop @ ImportByteArray[ByteArray[bytes], "PNG"]
	}]
]

(*----------------------------------------------------------------------------*)

RenderedTextSize[args___] :=
	$functions["rendered_text_size"][args]

(*----------------------------------------------------------------------------*)

DiagramGraph[
	diagram_Diagram
] := Module[{
	boxes = DiagramBoxes[diagram],
	arrows = DiagramArrows[diagram],
	vertices,
	edges
},
	vertices = Cases[
		boxes,
		box_DiaBox :> Replace[box, {
			DiaBox[id_?StringQ, ___?OptionQ] :> id,
			other_ :> RaiseError["unexpected DiaBox structure: ``", other]
		}]
	];

	edges = Cases[
		arrows,
		arrow_DiaArrow :> Replace[arrow, {
			DiaArrow[
				lhs_?StringQ -> rhs_?StringQ,
				_?StringQ,
				___
			] :> DirectedEdge[lhs, rhs],
			other_ :> RaiseError["unexpected DiaArrow structure: ``", other]
		}]
	];

	Graph[vertices, edges, VertexLabels -> "Name"]
]

(*------------------------------------*)

DiagramGraph[args___] :=
	RaiseError["unexpected arguments to DiagramGraph: ``", InputForm[{args}]]

(****************************************)

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

(*====================================*)
(* Diagram property accessors         *)
(*====================================*)

DiagramTitle[
	diagram:Diagram[
		Optional[title_?StringQ, None],
		{___DiaBox},
		{___DiaArrow},
		___?OptionQ
	]
] := title

DiagramTitle[args___] :=
	RaiseError["unexpected arguments to DiagramTitle: ``", InputForm[{args}]]

(*====================================*)

DiagramBoxes[
	diagram:Diagram[
		Optional[_?StringQ, None],
		boxes:{___DiaBox},
		{___DiaArrow},
		___?OptionQ
	]
] := boxes

DiagramBoxes[args___] :=
	RaiseError["unexpected arguments to DiagramBoxes: ``", InputForm[{args}]]

(*====================================*)

DiagramArrows[
	diagram:Diagram[
		Optional[_?StringQ, None],
		{___DiaBox},
		arrows:{___DiaArrow},
		___?OptionQ
	]
] := arrows

DiagramArrows[args___] :=
	RaiseError["unexpected arguments to DiagramArrows: ``", InputForm[{args}]]

(*====================================*)

DiagramElementId[DiaBox[id_?StringQ, ___?OptionQ]] := id

DiagramElementId[args___] :=
	RaiseError["unexpected arguments to DiagramElementId: ``", InputForm[{args}]]

(*====================================*)

DiagramElementText[DiaBox[id_?StringQ, ___?OptionQ]] := id

DiagramElementText[args___] :=
	RaiseError["unexpected arguments to DiagramElementText: ``", InputForm[{args}]]


End[] (* End `Private` *)

EndPackage[]
