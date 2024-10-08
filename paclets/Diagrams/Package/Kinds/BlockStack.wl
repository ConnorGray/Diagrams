Package["Diagrams`Kinds`BlockStack`"]

PackageUse[Diagrams -> {
	Diagram,
	DiagramError,
	DiaID,
	DiaArrow,

	BlockStackDiagram,
	MultiBlockStackDiagram,
	TreeStackDiagram,

	Layout -> {
		LayoutUtils -> {
			AbsoluteTranslate, AnnotationToGraphics
		}
	},
	Utils -> {
		OutputElementsQ, ConstructOutputElements, ForwardOptions,
		RectangleAttachmentPoint
	},
	Errors -> {
		SetFallthroughError, Raise, Handle, WrapRaised, ConfirmReplace,
		RaiseConfirm, RaiseAssert
	}
}]

PackageExport[{
	(* Note: Exported for testing purposes. *)
	treeToLayers
}]

(*========================================================*)

$regions := Raise[DiagramError, "Invalid unscopped access of $regions"]

SetFallthroughError[addRegion]

addRegion[id: _DiaID, value: _] := (
	If[KeyExistsQ[$regions, id],
		(* TID:240721/5: Duplicate IDs in BlockStackDiagram *)
		Raise[
			DiagramError,
			"Region value already defined for ID: ``, value: ``",
			InputForm[id],
			InputForm[value]
		];
	];

	$regions[id] = value;
)

(*------------------------------------*)

SetFallthroughError[BlockStackDiagram]

Options[BlockStackDiagram] = {
	Sequence @@ Options[Graphics]
}

BlockStackDiagram[
	rows0_List,
	outputElems : _?OutputElementsQ : Automatic,
	optsSeq:OptionsPattern[]
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating BlockStackDiagram"
] @ Block[{
	$regions = <||>
}, Module[{
	rows = rows0,
	graphic
},
	graphic = Graphics[
		#,
		PlotRangePadding -> 0,
		ForwardOptions[optsSeq]
	]& @ FoldPairList[
		{yOffset, row} |-> Module[{
			rowHeight,
			rowElements,
			graphic
		},
			{rowHeight, rowElements} = ConfirmReplace[row, {
				{_?NumberQ, _List} :> row,
				other_ :> Raise[
					DiagramError,
					"Expected row of the form {height, {elements...}}, got: ``",
					InputForm[other]
				]
			}];

			graphic = blockStackDiagramRow[rowElements, rowHeight, yOffset];

			{
				graphic,
				yOffset + rowHeight
			}
		],
		0,
		rows
	];

	(*---------------------------------------*)
	(* Process output elements specification *)
	(*---------------------------------------*)

	ConstructOutputElements[
		outputElems,
		"Diagram",
		{
			"Diagram" -> Diagram[<|
				"Graphics" -> graphic,
				"Regions" -> $regions
			|>],
			"Graphics" :> graphic,
			"Regions" :> $regions
		}
	]
]]

(*====================================*)

SetFallthroughError[blockStackDiagramRow]

blockStackDiagramRow[
	row_List,
	rowSize_?NumberQ,
	yOffset_?NumberQ
] := Module[{
	defaultBackground = GrayLevel[0.8]
},
	FoldPairList[
		{xOffset, elem} |-> Module[{
			(* Default width *)
			width = 1,
			diaIDs = {},
			styleOpts = {},
			label,
			expr
		},
			(* Process element wrappers like Item[..] and DiaID[..]. *)
			(* TODO:
				This won't detect redundant or erroenous or conflicting wrapper
				values, e.g. Item[Item[..], ..].

				Add a utility function for doing this kind of wrapper processing
				that does things like only allow a single wrapper processing
				rule to fire once for a given element being processed. *)
			label = FixedPoint[
				Replace[{
					(* TODO: What about unrecognized styleOpts? *)
					Item[
						label_,
						width0 : _?NumberQ : 1,
						styleOptsSeq___?OptionQ
					] :> (
						width = width0;
						styleOpts = {styleOptsSeq};

						label
					),
					(id0_DiaID)[expr_] :> (
						(* TID:240724/2: Support multiple DiaID wrappers. *)
						AppendTo[diaIDs, id0];

						expr
					)
				}],
				elem,
				10
			];

			RaiseAssert[ListQ[styleOpts]];

			expr = renderBlock[
				diaIDs,
				label,
				rowSize,
				width,
				Lookup[
					styleOpts,
					Background,
					defaultBackground
				],
				{xOffset, yOffset},
				Sequence @@ styleOpts
			];

			{expr, xOffset + rowSize * width}
		],
		0,
		row
	]
]

(*====================================*)

SetFallthroughError[renderBlock]

renderBlock[
	diaIDs : {___DiaID},
	content_,
	rowSize_?NumberQ,
	width_?NumberQ,
	color0_,
	offsets:{_?NumberQ, _?NumberQ},
	styleOpts___
] := Module[{
	position = {(rowSize * width)/2, rowSize/2} + offsets,
	color = Replace[color0, Automatic :> RandomColor[Hue[_, 1, 0.7]]],
	rect
},
	rect = AbsoluteTranslate[
		Rectangle[{0, 0}, {rowSize * width, rowSize}],
		offsets
	];

	Scan[
		id |-> addRegion[id, rect],
		diaIDs
	];

	{
		color,
		EdgeForm[{Thickness[0.005], Gray}],
		rect,
		ColorNegate[color],
		ConfirmReplace[content, {
			text_?AtomQ :> (
				Text[
					Style[content, styleOpts, Bold, FontFamily -> "PT Mono"],
					position
				]
			),
			other_ :> (
				Translate[other, position]
			)
		}]
	}
];

(*========================================================*)

SetFallthroughError[TreeStackDiagram]

TreeStackDiagram[
	tree_Tree,
	outputElems : _?OutputElementsQ : Automatic
] := Module[{},
	BlockStackDiagram[
		Map[
			layer |-> {1, layer},
			treeToLayers[tree]
		],
		outputElems
	]
]

(*===================================*)

SetFallthroughError[treeToLayers]

(* Turn a Tree into a list of layers. *)
treeToLayers[tree_?TreeQ] := Module[{
	lowerLayers,
	baseWidth = treeBaseWidth[tree],
	children,
	maxDepth,
	treeData
},
	If[TreeLeafQ[tree] || TreeChildren[tree] === {},
		(* Return the bottom layer. *)
		Return[{
			{TreeData[tree]}
		}, Module]
	];

	(*------------------------------------------------------*)
	(* Process children into the layers beneath this level. *)
	(*------------------------------------------------------*)

	children = TreeChildren[tree];

	maxDepth = Max[Map[TreeDepth, children]];

	(* Wrap the shallower children in a sufficient number of "empty"
	   parent Tree's to make the depth of all children equal. *)
	(* TID:240721/4: treeToLayers handling of mixed-depth subtrees. *)
	children = Map[
		child |-> Nest[
			Tree[
				(* Note: Show a vertial ellipsis, working around Packages parse
					error. *)
				Item[FromCharacterCode[8942], Background -> White],
				{#}
			] &,
			child,
			(* 0 for most well-behaved trees, but >0 for subtrees that are
				shallower than their siblings. *)
			maxDepth - TreeDepth[child]
		],
		children
	];

	(* Error if lower layers lengths (i.e. depths) are not consistent. *)
	(* NOTE: Now that the above logic exists to make the depths all the same,
		this check is more of an assert. *)
	If[Not[SameQ @@ Map[TreeDepth, children]],
		Raise[
			DiagramError,
			<| "Tree" -> tree |>,
			"Mixed-depth trees are not supported.",
		];
	];

	lowerLayers = Map[
		treeToLayers,
		children
	];

	(* Join elements across layers in the processed children. *)
	lowerLayers = ConfirmReplace[
		lowerLayers,
		{layersSeq___} :> Join[layersSeq, 2]
	];

	(*--------------------------------------------*)
	(* Process the label of this node in the tree *)
	(*--------------------------------------------*)

	ConfirmReplace[TreeData[tree], {
		(* Elide creating a layer at this level, this is an "anonymous" parent
			node. *)
		Null :> lowerLayers,

		(*-------------------------------------------*)
		(* Handle `TreeData` value that is itself an *)
		(*      Item[label, ___?OptionQ]             *)
		(*-------------------------------------------*)

		(* TID:240721/2: treeToLayers handling of existing Item[..] tree data *)
		Item[
			label_,
			opts___?OptionQ
		] :> Append[
			lowerLayers,
			{Item[label, treeBaseWidth[tree], opts]}
		],

		(* TID:240721/3: treeToLayers handling of Item[..] with custom width *)
		treeData:Item[
			label_,
			width_?NumberQ,
			opts___?OptionQ
		] :> Raise[
			DiagramError,
			"Unsupported custom Item width in tree node data: ``",
			InputForm[treeData]
		],

		(*------------------------------*)
		(* Handle all other label kinds *)
		(*------------------------------*)

		label_ :> Append[
			lowerLayers,
			{Item[label, treeBaseWidth[tree]]}
		]
	}]
]

(*===================================*)

SetFallthroughError[treeBaseWidth]

treeBaseWidth[expr_] := ConfirmReplace[expr, {
	tree_?TreeLeafQ :> 1,

	tree_?TreeQ :> Total @ Map[
		treeBaseWidth,
		Replace[TreeChildren[tree], None -> {}]
	],

	Item[
		_,
		width : _?IntegerQ : 1,
		___?OptionQ
	] :> width,

	other_ :> Raise[
		DiagramError,
		"Unsupported tree base width argument form: ``",
		InputForm[other]
	]
}]

(*========================================================*)

Options[MultiBlockStackDiagram] = {
	ChartLegends -> None
}

SetFallthroughError[MultiBlockStackDiagram]

(*
	Output Elements:

	* "Diagram" — a Diagram[..] object representation that renders visually in
		the FrontEnd. May include chart legends if applicable.
	* "Graphic" — guaranteed to be a Graphics[..] expression, suitable for
		composition with other graphics, e.g. via Show.
	* "Regions" — an Association of the named regions present in the graphic.
*)
MultiBlockStackDiagram[
	stacks_List,
	connections_List,
	gap : _?NumberQ : 0.5,
	outputElems : _?OutputElementsQ : Automatic,
	OptionsPattern[]
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating MultiBlockStackDiagram"
] @ Module[{
	stackDiagrams,
	allRegions,
	connectionsGraphics,
	graphic,
	diagram
},
	stackDiagrams = Map[
		stack |-> RaiseConfirm @ BlockStackDiagram[stack, {"Graphics", "Regions"}],
		stacks
	];

	RaiseAssert[MatchQ[stackDiagrams, {{_Graphics, _?AssociationQ} ...}]];

	{stackDiagrams, allRegions} = Transpose @ FoldPairList[
		{state, element} |-> ConfirmReplace[{state, element},
			{xOffset_?NumberQ, {diagram_Graphics, regions_?AssociationQ}} :> Module[{
				width = ConfirmReplace[AbsoluteOptions[diagram, PlotRange], {
					{PlotRange -> {{0., x_}, {0., _}}} :> x,
					other_ :> Raise[
						DiagramError,
						"Unable to calculate plot width from unexpected absolute PlotRange value: ``",
						InputForm[other]
					]
				}]
			},
				{
					{
						Translate[
							ConfirmReplace[
								diagram,
								Graphics[commands_, ___] :> commands
							],
							{xOffset, 0}
						],
						Map[
							rect |-> AbsoluteTranslate[rect, {xOffset, 0}],
							regions
						]
					},
					xOffset + width + gap
				}
			]
		],
		0,
		stackDiagrams
	];

	RaiseAssert[MatchQ[stackDiagrams, {___Translate}]];
	RaiseAssert[MatchQ[allRegions, {___?AssociationQ}]];

	(* TODO: Test and TID for this check. *)
	(* Use addRegion to force a check for unique DiaID. *)
	allRegions = Block[{$regions = <||>},
		Map[
			regions |-> KeyValueMap[addRegion, regions],
			allRegions
		];

		$regions
	];

	RaiseAssert[AssociationQ[allRegions]];

	(*--------------------------------*)
	(* Compute the connection arrows  *)
	(*--------------------------------*)

	connectionsGraphics = Map[
		connection |-> AnnotationToGraphics[connection, allRegions],
		connections
	];

	(*--------------------------------*)
	(* Construct the output elements  *)
	(*--------------------------------*)

	graphic = Graphics[
		Join[stackDiagrams, connectionsGraphics],
		PlotRangePadding -> 0
	];

	diagram = Diagram[<|
		"Graphics" -> graphic,
		"Regions" -> allRegions
	|>];

	(* FIXME: Handle ChartLegends differently, by making these a "typed"
		property of the Diagram[..] object, and adding them in the caller
		instead of here. *)
	diagram = ConfirmReplace[OptionValue[ChartLegends], {
		None -> diagram,
		wrapper_Function :> wrapper[diagram],
		other_ :> Raise[
			DiagramError,
			"Unsupported ChartLegends option value: ``",
			InputForm[other]
		]
	}];

	ConstructOutputElements[
		outputElems,
		"Diagram",
		{
			"Diagram" :> diagram,
			"Graphics" :> graphic,
			"Regions" :> allRegions
		}
	]
]