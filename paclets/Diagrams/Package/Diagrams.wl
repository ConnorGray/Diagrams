Package["Diagrams`"]

PackageModule["Concepts"]
PackageModule["Errors"]
PackageModule["Layout"]
PackageModule["Layouts"]
PackageModule["Render"]
PackageModule["Utils"]
PackageModule["Library"]
PackageModule["Kinds"]     (* Diagram Kinds *)


PackageExport[{
	Diagrams,

	DiagramError,

	Diagram,
	PlacedDiagram,

	DiaBox, DiaArrow,

	MakeDiagramPrimitives,

	(* Operations on Diagram[..] *)
	DiagramGraph,
	DiagramAnnotate,

	(* Options on Diagram[..] and diagram elements *)
	DiagramLayout, DiagramTheme,

	BoxPadding,

	(* Operations querying data in a Diagram[..] or diagram element *)
	DiagramElementId, DiagramElementContent,

	DiagramArrowIds,

	AttachmentId, AttachmentQ,

	(* Internal / intermediate diagram operations *)
	LayoutDiagram,
	RenderPlacedDiagramToGraphics,
	DiagramGraphicsImage,
	RenderedTextSize,

	(* TODO: Remove these, they are unused? *)
	Subgraphs, DiagramBlock, DiagramArrow,

	DiaID,

	(*----------------------*)
	(* Annotation concepts. *)
	(*----------------------*)
	DiaBracket,
	DiaHighlight,

	(*-------------------*)
	(* Diagram Functions *)
	(*-------------------*)

	BlockDiagram,
	BlockStackDiagram,
	TreeStackDiagram,
	MultiBlockStackDiagram,

	ProgramDiagram,
	FileSystemTreeDiagram
}]

Diagram::usage = "Diagram[{elements}] represents a diagram composed of elements."
PlacedDiagram::usage = "PlacedDiagram[boxes, arrows]"

DiaBox::usage = "DiaBox[..] is a diagram element of a generic box."
DiaArrow::usage = "DiaArrow[..] is a diagram element that defines a relationship between two diagram box elements."

MakeDiagramPrimitives::usage = "MakeDiagramPrimitives[expr] will convert expr into the lower-level diagram primitive forms used by Diagram."

(*---------------------------*)
(* Operations on Diagram[..] *)
(*---------------------------*)

DiagramGraph::usage = "DiagramGraph[boxes, arrows] returns a Graph object representing the relations between diagram elements blocks."

(*---------------------------------------------*)
(* Options on Diagram[..] and diagram elements *)
(*---------------------------------------------*)

DiagramLayout::usage = "DiagramLayout is an option to Diagram and related functions that specifies what layout to use."
DiagramTheme::usage  = "DiagramTheme is an option to Diagram and related functions that specifies an overall theme for visualization elements and styles."

BoxPadding::usage = "BoxPadding is a suboption to DiagramLayout that specifies the amount of padding to use between a DiaBox's text and the box border."

(*---------------------------------------------------------------*)
(* Operations querying data in a Diagram[..] or diagram element. *)
(*---------------------------------------------------------------*)

DiagramElementId::usage = "DiagramElementId[elem] will return the unique identifer associated with a diagram element."
DiagramElementContent::usage = "DiagramElementContent[elem] will return the textual or graphical content associated with a diagram element, if applicable."

DiagramArrowIds::usage = "DiagramArrowIds[arrow] will return the id of the start and end element connected by arrow."

AttachmentId::usage = "AttachmentId[spec] will return the element ID specified by spec."
AttachmentQ::usage = "AttachmentQ[spec] will return True if spec is a valid diagram attachment specification, and False otherwise."

(*--------------------------------------------*)
(* Internal / intermediate diagram operations *)
(*--------------------------------------------*)

LayoutDiagram::usage = "LayoutDiagram[boxes, arrows] uses a suitable layout algorithm to produce a PlacedDiagram"

RenderPlacedDiagramToGraphics

DiagramGraphicsImage

RenderedTextSize

(*--------*)
(* Errors *)
(*--------*)

(*========================================================*)

Diagrams::error = "``"
Diagrams::deprecated = "``"


PackageUse[Diagrams -> {
	Errors -> {
		CreateErrorType, RaiseError, Raise, RaiseConfirm,
		RaiseAssert, ConfirmReplace, SetFallthroughError
	},
	Render -> SizedText,
	Utils -> {RectangleSize, RectangleAttachmentPoint},
	Layout -> {
		$DebugDiagramLayout,
		LayoutUtils -> {AnnotationToGraphics}
	},
	Library -> {$LibraryFunctions}
}]

(*----------------------------------------------------------------------------*)

(* TODO:
	This is a legacy definition for backwards-compatibility. Remove this after
	I've updated all my old Diagram[..] instances and validated the new
	BlockDiagram function works the same. *)
(* FIXME:
	Should Diagram[..] actually eagerly evaluate the high-level element types
	into primitives? Perhaps this should be delayed until rendering time? *)
(*
	If either the boxes or arrows field contains non-primitive forms, use
	MakeDiagramPrimitives to reduce the forms to primitive ones.
*)
Diagram[
	Optional[title: _?StringQ, None],
	boxes:{___},
	arrows:{___},
	opts: ___?OptionQ
] := (
	Message[
		Diagrams::deprecated,
		"Diagrams[boxes_, arrows_] form has been replaced with BlockDiagram[..]"
	];

	BlockDiagram[boxes, arrows, opts]
)

(*======================================*)

	(* BoxForm`ArrangeSummaryBox[
		Diagram,
		obj,
		DiagramImage[obj],
		{
			BoxForm`SummaryItem[{"Boxes: ", Length[boxes]}],
			BoxForm`SummaryItem[{"Arrows: ", Length[arrows]}]
		},
		{},
		form
	] *)

Diagram /: MakeBoxes[
	diagram:Diagram[
		diagramProps_?AssociationQ,
		diagramOptsSeq___?OptionQ
	],
	form : StandardForm
] := Module[{
	graphics,
	chartLegends = Lookup[{diagramOptsSeq}, ChartLegends, None]
},
	graphics = diagramProps["Graphics"];

	RaiseAssert[
		MatchQ[graphics, _Graphics],
		"Unable to generate boxes for Diagram[<|...|>] with missing or invalid \"Graphics\" property."
	];

	(* Add the custom frame color that indicates this graphic is the rendered
		form of a Diagram[..]. (Same as how Tree has its own frame color.) *)
	graphics = Show[
		graphics,
		BaseStyle -> {Symbol["System`GraphicsHighlightColor"] -> Lighter[Blue]}
	];

	(*------------------------------------*)
	(* Show the ChartLegends, if present. *)
	(*------------------------------------*)

	graphics = ConfirmReplace[chartLegends, {
		None :> graphics,
		Placed[legend_, pos_] :> Labeled[graphics, legend, pos],
		other_ :> Raise[
			DiagramError,
			"Invalid form for Diagram ChartLegends option: ``",
			InputForm[other]
		]
	}];

	ToBoxes @ Interpretation[graphics, diagram]
]

(*----------------------------------------------------------------------------*)

Subgraphs[graph_Graph] := Map[
	Subgraph[graph, #, VertexLabels -> "Name"] &,
	ConnectedComponents[UndirectedGraph[graph]]
]

(*----------------------------------------------------------------------------*)

DiagramGraphicsImage[args___] := Module[{result},
	result = $LibraryFunctions["graphics_image"][args];

	Replace[result, {
		bytes:{___?IntegerQ} :> ImageCrop @ ImportByteArray[ByteArray[bytes], "PNG"]
	}]
]

(*----------------------------------------------------------------------------*)

RenderedTextSize[args___] :=
	$LibraryFunctions["rendered_text_size"][args]

(*----------------------------------------------------------------------------*)

SetFallthroughError[DiagramGraph]

DiagramGraph[
	boxes0: _List,
	arrows0: _List
] := Module[{
	boxes = boxes0,
	arrows = arrows0,
	vertices,
	edges
},
	vertices = Cases[
		boxes,
		box_DiaBox :> Replace[box, {
			DiaBox[id_?StringQ, Repeated[Except[_?OptionQ], {0, 1}], ___?OptionQ] :> id,
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

SetFallthroughError[MakeDiagramPrimitives]

MakeDiagramPrimitives[arrow_DiaArrow] := arrow
MakeDiagramPrimitives[box_DiaBox] := box


MakeDiagramPrimitives[elem_] :=
	RaiseError[
		"MakeDiagramPrimitives: unable to convert unrecognized diagram element form: ``.",
		InputForm[{elem}]
	]

(*====================================*)
(* Diagram property accessors         *)
(*====================================*)

SetFallthroughError[DiagramElementId]

DiagramElementId[DiaBox[
	id_?StringQ,
	Optional[content:Except[_?OptionQ], None],
	___?OptionQ
]] := id

(*====================================*)

SetFallthroughError[DiagramElementContent]

DiagramElementContent[DiaBox[id_?StringQ,                            ___?OptionQ]] := id
DiagramElementContent[DiaBox[id_?StringQ, content:Except[_?OptionQ], ___?OptionQ]] := content

DiagramElementContent[DiaArrow[_ -> _, id_?StringQ, Optional[_, None], ___?OptionQ]] := id

(*====================================*)

SetFallthroughError[DiagramArrowIds]

DiagramArrowIds[arrow_DiaArrow] := Replace[arrow, {
	(* Check for a few possibly common mistakes first. *)
	DiaArrow[{lhs_?StringQ, Nearest} -> {rhs_?StringQ, Nearest}, ___] :> RaiseError[
		"unsupported use of {_, Nearest} specification on both diagram arrow sides: ``",
		InputForm[arrow]
	],
	DiaArrow[lhs_ -> rhs_, _?StringQ, ___] :> {AttachmentId[lhs], AttachmentId[rhs]},
	_ :> RaiseError["unexpected DiaArrow structure: ``", InputForm[arrow]]
}]

(*====================================*)

SetFallthroughError[AttachmentId]

AttachmentId[spec_] := Replace[spec, {
	id_?StringQ :> id,
	{id_?StringQ, Nearest} :> id,
	_ :> RaiseError["unrecognized attachment specification: ``", spec]
}]

(*------------------------------------*)

SetFallthroughError[AttachmentQ]

AttachmentQ[spec_] := MatchQ[
	spec,
	Alternatives[
		_?StringQ,
		{_?StringQ, Nearest}
	]
]

(*========================================================*)

SetFallthroughError[DiagramAnnotate]

DiagramAnnotate[
	diagram0:Diagram[assoc_?AssociationQ, ___?OptionQ],
	annotations0_
] := Module[{
	diagram = diagram0,
	annotations = Replace[annotations0, elem:Except[_List] :> {elem}],
	graphics,
	regions,
	annotationGraphics
},
	graphics = RaiseConfirm @ Lookup[assoc, "Graphics"];
	regions  = RaiseConfirm @ Lookup[assoc, "Regions"];

	annotationGraphics = Map[
		AnnotationToGraphics,
		annotations
	];

	(* FIXME: Recalculate ImageSize to preserve the apparent size of the main
		diagram portion of the graphic. *)
	graphics = Show[graphics, Graphics[annotationGraphics]];

	diagram[[1, "Graphics"]] = graphics;

	diagram
]
