BeginPackage["DiagramMaker`"]


Diagram::usage = "Diagram[{elements}] represents a diagram composed of elements."
PlacedDiagram::usage = "PlacedDiagram[boxes, arrows]"

DiaBox::usage = "DiaBox[..] is a diagram element of a generic box."
DiaArrow::usage = "DiaArrow[..] is a diagram element that defines a relationship between two diagram box elements."

(*---------------------------*)
(* Operations on Diagram[..] *)
(*---------------------------*)

DiagramImage::usage = "DiagramImage[diagram] returns an image containing the graphical representation of diagram."
DiagramGraph::usage = "DiagramGraph[diagram] returns a Graph object representing the relations between diagram elements blocks."

(*---------------------------------------------*)
(* Options on Diagram[..] and diagram elements *)
(*---------------------------------------------*)

DiagramLayout::usage = "DiagramLayout is an option to Diagram and related functions that specifies what layout to use."
DiagramTheme::usage  = "DiagramTheme is an option to Diagram and related functions that specifies an overall theme for visualization elements and styles."

BoxPadding::usage = "BoxPadding is a suboption to DiagramLayout that specifies the amount of padding to use between a DiaBox's text and the box border."

(*---------------------------------------------------------------*)
(* Operations querying data in a Diagram[..] or diagram element. *)
(*---------------------------------------------------------------*)

DiagramTitle::usage = "DiagramTitle[diagram]"
DiagramBoxes::usage = "DiagramBoxes[diagram]"
DiagramArrows::usage = "DiagramArrows[diagram]"

DiagramElementId::usage = "DiagramElementId[elem] will return the unique identifer associated with a diagram element."
DiagramElementText::usage = "DiagramElementText[elem] will return the textual description associated with a diagram element, if applicable."

DiagramArrowIds::usage = "DiagramArrowIds[arrow] will return the id of the start and end element connected by arrow."

AttachmentId::usage = "AttachmentId[spec] will return the element ID specified by spec."

(*--------------------------------------------*)
(* Internal / intermediate diagram operations *)
(*--------------------------------------------*)

LayoutDiagram::usage = "LayoutDiagram[diagram] uses a suitable layout algorithm to produce a PlacedDiagram"

RenderPlacedDiagramToGraphics

DiagramGraphicsImage

RenderedTextSize

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
		title = DiagramTitle[diagram]
	},
		placed = LayoutDiagram[diagram];
		graphics = RenderPlacedDiagramToGraphics[placed, theme];

		RaiseAssert[ListQ[graphics]];

		graphics = ReplaceAll[graphics,
			sizedText_SizedText :> makeSizedTextInset[sizedText]
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

(*------------------------------------*)

makeSizedTextInset[sizedText_SizedText] := Module[{
	str,
	rect, min, max,
	center,
	width, height,
	fontSize
},
	{str, rect, min, max} = Replace[sizedText, {
		SizedText[str_?StringQ, rect:Rectangle[min_, max_]] :> {str, rect, min, max},
		other_ :> RaiseError["unexpected SizedText specification: ``", sizedText]
	}];

	center = Mean[{min, max}];
	{width, height} = RectangleSize[rect];

	(* Note:
		Font size in Wolfram graphics can be specified in one of two
		ways:

			FontSize -> _?NumberQ
			FontSize -> Scaled[_?NumberQ]

		The units of the first specification are printers points,
		which are based on the physical size-in-inches of the users
		screen. When using the first specification, the
		size-in-inches of the rendered text will be the same no
		matter what the size-in-inches of the overall graphic is. If
		the user resizes the graphic larger or smaller, the text
		will not change size.

		The units of the second specification are scaled based on
		the plot range of the graphic: if the user resizes the
		graphic larger or smaller, the text will resize to be the
		same size relative to the overall graphic.

		Using the first specification makes manual positioning and
		layout of text in a graphic essentially impossible, because
		the text will appear to change size relative to the overall
		graphic if the user resizes it. E.g. at the extreme end, if
		the user makes the graphic very small, the text will still
		be the same physical size, and would likely spill over and
		cover up the other elements in the diagram graphic.

		Using the second specification is a bit better, because the
		text will be the same relative size if the graphic is made
		smaller or larger, meaning that it will appear to stay in
		the position that we placed it in no matter how the user
		resizes the graphic.

		However, using scaled coordinates leaves one problem
		unsolved: what should the actual value passed to Scaled[..]
		be?

		Scaled[..] takes a value that is a percentage of the plot
		range. For almost all graphics primitives, it is relatively
		straightforward to determine the scaled size from the
		absolute size in graphics system coordinates: you just
		divide the x and y coordinates that appear explicitly in the
		primitive specification by the width and height,
		respectively, of the overall graphic.

		However, the Text[..] primitive is unique, in that the size
		of the rendered text is not specified in any argument that
		Text accepts. Compare with e.g. Point or Line or Rectangle,
		which all take explicit positions as arguments, so their
		inherent size within graphics coordinate space is clear and
		unambiguous.

		The only way to adjust the size of text is to set the
		FontSize option, which, as mentioned, takes units of either
		printers points or scaled units. Determining the right
		scaling factor is made further difficult by the fact that
		the scaling factor doesn't scale the overall width of the
		complete rendered text string, it effectively scales the
		point size of the font to the plot range, which means that
		the final width of the text is based on the font metrics of
		each individual font.

		If we want our text `str` to fill the complete area of the
		`textRect` computed during layout, we need to know the font
		metrics of each letter in `str` to get the total width of
		the string in printers points, and then do
		1/totalPrintersPoints to get the optimal Scaled[..] factor.

		At the moment, I don't know a good way to get the precise
		font metrics, so the calculation below is an approximation.

		The code below uses:
			Inset[Graphics[{FontSize -> fontSize, Text[...]}]]

		instead of just {FontSize -> fontSize, Text[...]} so that
		the Scaled[..] coordinates only need to be computed in
		terms of the rendered size of `str`, not the rendered size
		of the overall diagram graphic.
	*)
	(* FIXME:
		Determine the correct font metrics to make this factor precise, and
		instead of using StringLength of the entire string (which is incorrect)
		if there are multiple lines, use the widest line. *)
	fontSize = Scaled[1.75 / StringLength[str]];

	(* FIXME:
		Compute this font size based on the overall size of
		the graphic. *)
	Inset[
		Graphics[{
			FontSize -> fontSize,
			Text[str, {width, height} / 2]
		},
			Frame -> False,
			PlotRange -> {{0, width}, {0, height}},
			PlotRangePadding -> 0,
			ImagePadding -> 0
		],
		center,
		Automatic,
		{width, height},
		Background -> If[TrueQ[DiagramMaker`Layout`$DebugDiagramLayout],
			Directive[Opacity[0.2], Red],
			Automatic
		]
	]
]

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
			DiaArrow[lhs_ -> rhs_, _?StringQ, ___] :> DirectedEdge[
				AttachmentId[lhs],
				AttachmentId[rhs]
			],
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

(*====================================*)

DiagramArrowIds[arrow_DiaArrow] := Replace[arrow, {
	(* Check for a few possibly common mistakes first. *)
	DiaArrow[{lhs_?StringQ, Nearest} -> {rhs_?StringQ, Nearest}, ___] :> RaiseError[
		"unsupported use of {_, Nearest} specification on both diagram arrow sides: ``",
		InputForm[arrow]
	],
	DiaArrow[lhs_ -> rhs_, _?StringQ, ___] :> {AttachmentId[lhs], AttachmentId[rhs]},
	_ :> RaiseError["unexpected DiaArrow structure: ``", InputForm[arrow]]
}]

DiagramArrowIds[args___] :=
	RaiseError["unexpected arguments to DiagramArrowIds: ``", InputForm[{args}]]

(*====================================*)

AttachmentId[spec_] := Replace[spec, {
	id_?StringQ :> id,
	{id_?StringQ, Nearest} :> id,
	_ :> RaiseError["unrecognized attachment specification: ``", spec]
}]

AttachmentId[args___] :=
	RaiseError["unexpected arguments to AttachmentId: ``", InputForm[{args}]]



End[] (* End `Private` *)

EndPackage[]
