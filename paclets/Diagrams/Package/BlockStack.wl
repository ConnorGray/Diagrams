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
	processTree
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
		processTree[tree]
	]
]

(*===================================*)

SetFallthroughError[processTree]

processTree[tree_?TreeQ] := Module[{
	lowerLayers,
	baseWidth = treeBaseWidth[tree],
	treeData
},
	If[TreeLeafQ[tree] || TreeChildren[tree] === {},
		(* Return the bottom layer. *)
		Return[{
			{TreeData[tree]}
		}, Module]
	];

	lowerLayers = Map[
		processTree,
		TreeChildren[tree]
	];

	(* TODO: Error if lower layers lengths (i.e. depths) are not consistent. *)

	(* Join elements across layers in the processed children. *)
	lowerLayers = ConfirmReplace[
		lowerLayers,
		{layersSeq___} :> Join[layersSeq, 2]
	];

	If[TreeData[tree] =!= Null,
		(* TODO:
			Handle `TreeData` result that is itself an
				Item[label, ___?OptionQ].
			Error if the Item tries to specify a width? *)
		Append[
			lowerLayers,
			{Item[TreeData[tree], treeBaseWidth[tree]]}
		],
		lowerLayers
	]
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