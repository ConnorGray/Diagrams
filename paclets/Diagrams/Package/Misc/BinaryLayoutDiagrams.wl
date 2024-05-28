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
	DiaGrapheme,
	DiaCharacter,
	DiaCodepoint,
	DiaByte,
	DiaBit,

	(*---------------*)
	(* Configuration *)
	(*---------------*)
	$ColorScheme,

	(*-----------*)
	(* Utilities *)
	(*-----------*)
	ToCharacterCode2
}]

PackageUse[Diagrams -> {
	DiagramGraphicsImage,
	Errors -> {
		CreateErrorType, Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirm, RaiseConfirmMatch
	},
	Library -> {$LibraryFunctions},
	Render -> {
		SizedText
	},
	Utils -> {GraphemeClusters}
}]

$ColorScheme = <|
	"Byte" -> Brown,
	"Codepoint" -> Darker[Blue],
	"Character" -> Blue,
	"Grapheme" -> Blend[{Green, GrayLevel[0.3]}, 0.8],
	"String" -> GrayLevel[0.95]
|>;

(*========================================================*)

CreateErrorType[DiagramError, {}]

(*========================================================*)

Options[DiaString] = {
	(* NOTE:
		The Wolfram Front End incorrectly renders certain words, for example
		the Kannada word for "Hello" (ನಮಸ್ಕಾರ), by apparently mishandling how
		glyphs combine. If set to True, this option will change the "String"
		layer to render using a fallback renderer (Skia).
	*)
	"UseFallbackTextRenderer" -> False
}
(*========================================================*)

$tileSize = 80

Options[StringEncodingDiagram] = Join[
	{
		CharacterEncoding -> "UTF-8",
		ImageSize -> Automatic
	},
	Options[DiaString]
]

SetFallthroughError[StringEncodingDiagram]

StringEncodingDiagram[
	text_?StringQ,
	layers : {___},
	opts:OptionsPattern[]
] := Handle[_Failure] @ Module[{
	encoding = RaiseConfirmMatch[
		OptionValue[CharacterEncoding],
		_?StringQ
	],
	imageSize = Replace[
		OptionValue[ImageSize],
		(* Automatic ImageSize sets the height to be a multiple of the number
			of layers. *)
		Automatic -> {
			Automatic,
			45 * Length[DeleteCases[layers, "Bits"]]
		}
	],
	handle,
	directives = {},
	fontMultiplier
},
	(* TODO:
		Improve this heuristic to be more accurate when the StringLength
		is not the same as the number of vertical columns shown in the
		final graphic. *)
	fontMultiplier = If[MemberQ[layers, "Bytes"],
		0.015 / Length[ToCharacterCode2[text, encoding]]
		,
		0.015 / StringLength[text]
	];

	handle["String"] := Module[{},
		{DiaString[
			text,
			(* Calculate width in bytes *)
			Length @ ToCharacterCode2[text, encoding],
			FilterRules[{opts}, Options[DiaString]]
		]}
	];

	handle["Graphemes"] := Module[{},
		Map[
			grapheme |-> DiaGrapheme[
				grapheme,
				(* Calculate width in bytes. *)
				Length @ ToCharacterCode2[grapheme, encoding]
			],
			GraphemeClusters[text]
		]
	];

	handle["Characters"] := Module[{},
		Map[
			char |-> DiaCharacter[
				char,
				(* Calculate width in bytes *)
				Length @ ToCharacterCode2[char, encoding]
			],
			Characters[text]
		]
	];

	handle["Codepoints"] := Module[{},
		Map[
			codepoint |-> DiaCodepoint[
				codepoint,
				(* Calculate width in bytes *)
				Length @ ToCharacterCode2[
					FromCharacterCode[codepoint, "Unicode"],
					encoding
				]
			],
			ToCharacterCode2[text, "Unicode"]
		]
	];

	handle["Bytes"] := Module[{},
		Map[
			DiaByte,
			ToCharacterCode2[text, encoding]
		]
	];

	handle["Bits"] := Module[{},
		Flatten @ Map[
			byte |-> Map[
				DiaBit,
				IntegerDigits[byte, 2, 8]
			],
			ToCharacterCode2[text, encoding]
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

	Graphics[
		BinaryLayoutDiagram[
			Map[handle, layers],
			fontMultiplier
		],
		ImageSize -> imageSize,
		PlotRangePadding -> 0
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
] := Module[{
	position = {xOffset + ($tileSize * width)/2, $tileSize/2},
	color = Replace[color0, Automatic :> RandomColor[Hue[_, 1, 0.7]]]
},
	{
		color,
		EdgeForm[{Thickness[0.005], Gray}],
		Rectangle[{xOffset, 0}, {xOffset + $tileSize * width, $tileSize}],
		ColorNegate[color],
		Replace[content, {
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
							$ColorScheme["Byte"],
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
							$ColorScheme["Codepoint"],
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
							$ColorScheme["Character"],
							xOffset,
							FontSize -> Scaled[fontMultiplier * 32]
						],
						width * $tileSize
					}
				),
				DiaGrapheme[
					grapheme_?StringQ,
					width : _?IntegerQ
				] :> (
					{
						encodedTile[
							grapheme,
							width,
							$ColorScheme["Grapheme"],
							xOffset,
							FontSize -> Scaled[fontMultiplier * 32]
						],
						width * $tileSize
					}
				),
				DiaString[
					text_,
					width_?IntegerQ,
					stringOpts:OptionsPattern[]
				] :> Module[{
					label = text,
					useFallbackTextRenderer = RaiseConfirmMatch[
						OptionValue["UseFallbackTextRenderer"],
						_?BooleanQ
					],
					image
				},
					If[useFallbackTextRenderer,
						image = RaiseConfirm @ DiagramGraphicsImage @ Graphics[{
							SizedText[
								text,
								(* FIXME: Less arbitrary size? *)
								Rectangle[{0, 0}, {500, 500}]
							]
						}];

						RaiseConfirmMatch[image, _?ImageQ];

						(* BUG:
							For some reason ImageCrop[image] doesn't
							behave as expected. *)
						label = ImagePad[image, -BorderDimensions[image]];

						label = Inset[
							label,
							{0, 0},
							Center,
							(* TODO:
								This divide by 4 is an unprinciple choice
								that happens to look like about the right
								size on my machine. It may totally fail in
								cases I haven't tested. Think more deeply
								about the dimensions involved here and
								pick and document a better calculation. *)
							{$tileSize * width / 4, 0.8 * $tileSize}
						];
					];

					{
						encodedTile[
							label,
							width,
							$ColorScheme["String"],
							xOffset,
							FontSize -> Scaled[fontMultiplier * 32]
						],
						width * $tileSize
					}
				]
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
				_DiaBit -> (
					$tileSize / 8
				),
				_DiaByte
				| _DiaCodepoint
				| _DiaCharacter
				| _DiaGrapheme
				| _DiaString -> (
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

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)

SetFallthroughError[ToCharacterCode2]

(*
	This function is a workaround for the fact that WL doesn't provide support
	for UTF-16 encoding in ToCharacterCode.
*)
ToCharacterCode2[
	text_?StringQ,
	encoding_?StringQ
] := Module[{
	libFunc
},
	If[MemberQ[$CharacterEncodings, encoding] || encoding == "Unicode",
		Return @ ToCharacterCode[text, encoding];
	];

	libFunc = $LibraryFunctions["encode_string"];

	RaiseConfirmMatch[libFunc, _LibraryFunction];

	Replace[libFunc[text, encoding], {
		data_?NumericArrayQ :> Normal[data]
	}]
]