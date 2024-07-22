Package["Diagrams`BlockStack`"]

PackageUse[Diagrams -> {
	DiagramError,
	BlockStackDiagram,
	TreeStackDiagram,
	Errors -> {
		SetFallthroughError, Raise, Handle, WrapRaised, ConfirmReplace
	}
}]

PackageExport[{
	(* Note: Exported for testing purposes. *)
	treeToLayers
}]

(*========================================================*)

SetFallthroughError[BlockStackDiagram]

BlockStackDiagram[
	rows0_List
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating BlockStackDiagram"
] @ Module[{
	rows = rows0
},
	Graphics[#, PlotRangePadding -> 0]& @ FoldPairList[
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

			graphic = Translate[
				blockStackDiagramRow[rowElements, rowHeight],
				{0, yOffset}
			];

			{
				graphic,
				yOffset + rowHeight
			}
		],
		0,
		rows
	]
]

(*====================================*)

SetFallthroughError[blockStackDiagramRow]

blockStackDiagramRow[
	row_List,
	rowSize_?NumberQ
] := Module[{
	defaultBackground = GrayLevel[0.8]
},
	FoldPairList[
		{xOffset, elem} |-> Module[{expr, incr},
			{expr, incr} = ConfirmReplace[elem, {
				(* TODO: What about unrecognized styleOpts? *)
				Item[
					label_,
					width : _?NumberQ : 1,
					styleOpts___?OptionQ
				] :> (
					{
						renderBlock[
							label,
							rowSize,
							width,
							Lookup[
								{styleOpts},
								Background,
								defaultBackground
							],
							xOffset,
							styleOpts
						],
						width
					}
				),
				label_ :> (
					{
						renderBlock[
							label,
							rowSize,
							(* Default width *)
							1,
							defaultBackground,
							xOffset
						],
						(* Default width *)
						1
					}
				)
			}];
			{expr, xOffset + rowSize * incr}
		],
		0,
		row
	]
]

(*====================================*)

SetFallthroughError[renderBlock]

renderBlock[
	content_,
	rowSize_?NumberQ,
	width_?NumberQ,
	color0_,
	xOffset_?NumberQ,
	styleOpts___
] := Module[{
	position = {xOffset + (rowSize * width)/2, rowSize/2},
	color = Replace[color0, Automatic :> RandomColor[Hue[_, 1, 0.7]]]
},
	{
		color,
		EdgeForm[{Thickness[0.005], Gray}],
		Rectangle[{xOffset, 0}, {xOffset + rowSize * width, rowSize}],
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

TreeStackDiagram[tree_Tree] := Module[{},
	BlockStackDiagram @ Map[
		layer |-> {1, layer},
		treeToLayers[tree]
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