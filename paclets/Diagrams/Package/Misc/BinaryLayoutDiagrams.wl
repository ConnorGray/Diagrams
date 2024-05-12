Package["Diagrams`Misc`BinaryLayoutDiagrams`"]

PackageExport[{
	(* TODO:
		Move to more general package. *)
	DiagramError,

	StringEncodingDiagram,
	BinaryLayoutDiagram,

	(*----------*)
	(* Concepts *)
	(*----------*)
	DiaCharacter,
	DiaCodepoint,
	DiaByte,
	DiaBit
}]

PackageUse[Diagrams -> {
	Errors -> {CreateErrorType, Raise, ConfirmReplace, SetFallthroughError}
}]

(*========================================================*)

CreateErrorType[DiagramError, {}]

(*========================================================*)

$tileSize = 80

SetFallthroughError[StringEncodingDiagram]

StringEncodingDiagram[
	text_?StringQ,
	layers : {___}
] := Module[{
	encoding = "UTF-8",
	handle,
	directives = {}
},
	handle["Characters"] := Module[{},
		Map[
			char |-> DiaCharacter[
				char,
				(* Calculate width in bytes *)
				Length @ ToCharacterCode[char, encoding]
			],
			Characters[text]
		]
	];

	handle["Codepoints"] := Module[{},
		Map[
			codepoint |-> DiaCodepoint[
				codepoint,
				(* Calculate width in bytes *)
				Length @ ToCharacterCode[
					FromCharacterCode[codepoint, "Unicode"],
					encoding
				]
			],
			ToCharacterCode[text, "Unicode"]
		]
	];

	handle["Bytes"] := Module[{},
		Map[
			DiaByte,
			ToCharacterCode[text, encoding]
		]
	];

	handle["Bits"] := Module[{},
		Flatten @ Map[
			byte |-> Map[
				DiaBit,
				IntegerDigits[byte, 2, 8]
			],
			ToCharacterCode[text, encoding]
		]
	];

	(*	handle[Delimiter] := {
		AbsoluteThickness[8],
		Dashed,
		Line[{{0, 5}, {StringLength[text] * 10, 5}}]
	};*)
	handle[Delimiter] := Delimiter;

	handle[Labeled[row_, label_]] := Labeled[handle[row], label];

	handle[other_] := (
		Raise[DiagramError, "Bad layer name: ``", other];
	);

	(*	directives = Flatten[MapIndexed[
		{arg, index} |-> Translate[
			handle[arg],
			{0, 10 * index[[1]]}
		],
		layers
	]];\[LineSeparator]
	Graphics[directives, BaseStyle -> {FontSize -> 25}]*)

	Graphics @ BinaryLayoutDiagram @ Map[handle, layers]
]

(*========================================================*)

SetFallthroughError[encodedTile]

encodedTile[
	content_,
	width_?NumberQ,
	color0_,
	xOffset_?IntegerQ,
	styleOpts___
] :=
Module[{
	color = Replace[color0, Automatic :> RandomColor[Hue[_, 1, 0.7]]]
},
	{
		color,
		EdgeForm[{Thickness[0.005], Gray}],
		Rectangle[{xOffset, 0}, {xOffset + $tileSize * width, $tileSize}],
		ColorNegate[color],
		Text[
			Style[content, styleOpts, Bold, FontFamily -> "PT Mono"],
			{xOffset + ($tileSize * width)/2, $tileSize/2}
		]
	}
];

(*========================================================*)

SetFallthroughError[binaryLayoutDiagramRow]

binaryLayoutDiagramRow[
	row:{Except[_?ListQ]...}
] := Module[{
},
	FoldPairList[
		{xOffset, elem} |-> Module[{expr, incr},
			{expr, incr} = ConfirmReplace[elem, {
				DiaBit[value:(0|1)] :> (
					{
						encodedTile["", 1/8, GrayLevel[Clip[value, {0.15,0.95}]], xOffset, FontSize -> 8],
						10
					}
				),
				DiaByte[value_?IntegerQ] /; NonNegative[value] && value <= 255 :> (
					{
						encodedTile[value, 1, Brown, xOffset, FontSize -> 32],
						$tileSize
					}
				),
				DiaCodepoint[
					value_?IntegerQ,
					width : _?IntegerQ : 1
				] :> (
					{
						encodedTile[value, width, Darker[Blue], xOffset, FontSize -> 32],
						width * $tileSize
					}
				),
				DiaCharacter[
					char_?StringQ,
					width : _?IntegerQ : 1
				] :> (
					{
						encodedTile[char, width, Blue, xOffset, FontSize -> 32],
						width * $tileSize
					}
				)
			}];
			{expr, xOffset + incr}
		],
		0,
		row
	]
]

SetFallthroughError[BinaryLayoutDiagram]

BinaryLayoutDiagram[
	rows:{(_List | Delimiter | _Labeled)...}
] := Module[{},
	FoldPairList[
		{yOffset, row0} |-> Module[{
			row = row0,
			label = None,
			rowHeight,
			graphic
		},
			row = ConfirmReplace[row, {
				Labeled[r_, l_] :> (
					label = l;
					r
				),
				r_ :> r
			}];

			(* FIXME:
				Horrible hack to guess height of row *)
			rowHeight = ConfirmReplace[First[row, row], {
				_DiaBit -> $tileSize / 8,
				_DiaByte | _DiaCharacter | _DiaCodepoint -> $tileSize,
				Delimiter -> $tileSize / 4
			}];

			graphic = ConfirmReplace[row, {
				Delimiter :> {
					Thickness[0.02],
					RGBColor[0.36,0.65,0.88],
					InfiniteLine[{{0, yOffset + 10}, {1, yOffset + 10}}]
				},
				_ :> Translate[binaryLayoutDiagramRow[row], {0, yOffset}]
			}];

			If[label =!= None,
				graphic = {
					graphic,
					Translate[label, {-$tileSize, yOffset + rowHeight/2}]
				}
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