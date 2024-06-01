Package["Diagrams`BlockStack`"]

PackageUse[Diagrams -> {
	DiagramError,
	BlockStackDiagram,
	Errors -> {
		SetFallthroughError, Raise, Handle, WrapRaised, ConfirmReplace
	}
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
	Graphics @ FoldPairList[
		{yOffset, row} |-> Module[{
			rowHeight,
			rowElements,
			graphic
		},
			{rowHeight, rowElements} = ConfirmReplace[row, {
				{_?NumberQ, _List} :> row,
				other :> Raise[
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
							xOffset
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