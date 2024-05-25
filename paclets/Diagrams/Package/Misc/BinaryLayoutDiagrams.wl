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
	DiaString,
	DiaCharacter,
	DiaCodepoint,
	DiaByte,
	DiaBit
}]

PackageUse[Diagrams -> {
	Errors -> {
		CreateErrorType, Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirmMatch
	}
}]

(*========================================================*)

CreateErrorType[DiagramError, {}]

(*========================================================*)

$tileSize = 80

Options[StringEncodingDiagram] = {
	CharacterEncoding -> "UTF-8"
}

SetFallthroughError[StringEncodingDiagram]

StringEncodingDiagram[
	text_?StringQ,
	layers : {___},
	OptionsPattern[]
] := Handle[_Failure] @ Module[{
	encoding = RaiseConfirmMatch[
		OptionValue[CharacterEncoding],
		_?StringQ
	],
	handle,
	directives = {},
	(* TODO:
		Improve this heuristic to be more accurate when the StringLength
		is not the same as the number of vertical columns shown in the
		final graphic. *)
	fontMultiplier =  0.015 / StringLength[text]
},
	handle["String"] := Module[{},
		{DiaString[
			text,
			(* Calculate width in bytes *)
			Length @ ToCharacterCode[text, encoding]
		]}
	];

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

	handle[Labeled[row_, label_]] := (
		Labeled[
			handle[row],
			Style[label, FontSize -> 20]
		]
	);

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

	Graphics @ BinaryLayoutDiagram[
		Map[handle, layers],
		fontMultiplier
	]
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
	row:{Except[_?ListQ]...},
	fontMultiplier : _ : 1
] := Module[{
},
	FoldPairList[
		{xOffset, elem} |-> Module[{expr, incr},
			{expr, incr} = ConfirmReplace[elem, {
				DiaBit[value:(0|1)] :> (
					{
						encodedTile[
							"",
							1/8,
							GrayLevel[Clip[value, {0.15,0.95}]],
							xOffset,
							FontSize -> Scaled[fontMultiplier * 8]
						],
						10
					}
				),
				DiaByte[value_] :> (
					{
						encodedTile[
							value,
							1,
							Brown,
							xOffset,
							FontSize -> Scaled[fontMultiplier * 24]
						],
						$tileSize
					}
				),
				DiaCodepoint[
					value_?IntegerQ,
					width : _?IntegerQ : 1
				] :> (
					{
						encodedTile[
							(* "U+" <> ToUpperCase @ IntegerString[value, 16], *)
							value,
							width,
							Darker[Blue],
							xOffset,
							FontSize -> Scaled[fontMultiplier * 24]
						],
						width * $tileSize
					}
				),
				DiaCharacter[
					char_?StringQ,
					width : _?IntegerQ : 1
				] :> (
					{
						encodedTile[
							char,
							width,
							Blue,
							xOffset,
							FontSize -> Scaled[fontMultiplier * 32]
						],
						width * $tileSize
					}
				),
				DiaString[
					text_?StringQ,
					width_?IntegerQ
				] :> (
					{
						encodedTile[
							text,
							width,
							GrayLevel[0.95],
							xOffset,
							FontSize -> Scaled[fontMultiplier * 32]
						],
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
	rows:{(_List | Delimiter | _Labeled)...},
	fontMultiplier : _ : 1
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
				_DiaByte | _DiaCharacter | _DiaCodepoint | _DiaString -> (
					$tileSize
				),
				Delimiter -> $tileSize / 4
			}];

			graphic = ConfirmReplace[row, {
				Delimiter :> {
					Thickness[0.02],
					RGBColor[0.36,0.65,0.88],
					InfiniteLine[{{0, yOffset + 10}, {1, yOffset + 10}}]
				},
				_ :> (
					Translate[
						binaryLayoutDiagramRow[row, fontMultiplier],
						{0, yOffset}
					]
				)
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