Package["Diagrams`Misc`BinaryLayoutDiagrams`"]

PackageExport[{
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
	DiagramError,
	DiagramGraphicsImage,
	BlockStackDiagram,
	Errors -> {
		CreateErrorType, Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirm, RaiseConfirm2, RaiseConfirmMatch, WrapRaised
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
		ImageSize -> Automatic,
		ChartLegends -> None
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
	fontMultiplier,
	graphic
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
		Raise[DiagramError, "Bad layer name: ``", InputForm[other]];
	);

	(*--------------------------------------*)
	(* Produce the Graphics for the diagram *)
	(*--------------------------------------*)

	(*	directives = Flatten[MapIndexed[
		{arg, index} |-> Translate[
			handle[arg],
			{0, 10 * index[[1]]}
		],
		layers
	]];\[LineSeparator]
	Graphics[directives, BaseStyle -> {FontSize -> 25}]*)

	graphic = Show[
		RaiseConfirm2 @ BinaryLayoutDiagram[
			Map[handle, layers],
			fontMultiplier
		],
		ImageSize -> imageSize,
		PlotRangePadding -> 0
	];

	(*--------------------------------*)
	(* Handle the ChartLegends option *)
	(*--------------------------------*)

	ConfirmReplace[OptionValue[ChartLegends], {
		None :> graphic,
		Automatic :> Module[{
			legendLayers = Reverse @ DeleteCases[layers, "Bits"],
			colors,
			legend
		},
			colors = Map[
				layer |-> ConfirmReplace[layer, {
					"String" :> $ColorScheme["String"],
					"Graphemes" :> $ColorScheme["Grapheme"],
					"Characters" :> $ColorScheme["Character"],
					"Codepoints" :> $ColorScheme["Codepoint"],
					"Bytes" :> $ColorScheme["Byte"],
					other_ :> Raise[
						DiagramError,
						"Unsupported legend layer specification: ``",
						InputForm[other]
					]
				}],
				legendLayers
			];

			legend = SwatchLegend[
				colors,
				legendLayers,
				LegendMarkerSize -> 20,
				LegendMargins -> 0
			];

			Labeled[graphic, legend, Right]
		],
		other_ :> Raise[
			DiagramError,
			"Unsupported ChartLegends option value: ``",
			InputForm[other]
		]
	}]
]

(*========================================================*)

SetFallthroughError[binaryLayoutDiagramRow]

binaryLayoutDiagramRow[
	row:{Except[_?ListQ]...},
	fontMultiplier : _ : 1
] := Module[{
},
	Map[
		elem |-> Module[{expr},
			expr = ConfirmReplace[elem, {
				DiaBit[value:(0|1)] :> (
					Item[
						"",
						Background -> GrayLevel[Clip[value, {0.15,0.95}]],
						FontSize -> Scaled[fontMultiplier * 8]
					]
				),
				DiaByte[value_] :> (
					Item[
						value,
						1,
						Background -> $ColorScheme["Byte"],
						FontSize -> Scaled[fontMultiplier * 24]
					]
				),
				DiaCodepoint[
					value_?IntegerQ,
					width : _?IntegerQ : 1
				] :> (
					Item[
						(* "U+" <> ToUpperCase @ IntegerString[value, 16], *)
						value,
						width,
						Background -> $ColorScheme["Codepoint"],
						FontSize -> Scaled[fontMultiplier * 24]
					]
				),
				DiaCharacter[
					char_?StringQ,
					width : _?IntegerQ : 1
				] :> (
					Item[
						char,
						width,
						Background -> $ColorScheme["Character"],
						FontSize -> Scaled[fontMultiplier * 32]
					]
				),
				DiaGrapheme[
					grapheme_?StringQ,
					width : _?IntegerQ
				] :> (
					Item[
						grapheme,
						width,
						Background -> $ColorScheme["Grapheme"],
						FontSize -> Scaled[fontMultiplier * 32]
					]
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

					Item[
						label,
						width,
						Background -> $ColorScheme["String"],
						FontSize -> Scaled[fontMultiplier * 32]
					]
				]
			}];

			expr
		],
		row
	]
]

SetFallthroughError[BinaryLayoutDiagram]

BinaryLayoutDiagram[
	rows:{(_List | Delimiter | _Labeled)...},
	fontMultiplier : _ : 1
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating BinaryLayoutDiagram"
] @ Module[{
	blockRows
},
	blockRows = Map[
		row0 |-> Module[{
			row = row0,
			label = None,
			rowHeight,
			graphic
		},
			row = ConfirmReplace[row, {
				Labeled[r_, l_] :> (
					Raise[
						DiagramError,
						"Labeled[..] is no longer supported as a row specification"
					]
					(* label = l;
					r *)
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

			items = ConfirmReplace[row, {
				(* Delimiter :> {
					Thickness[0.02],
					RGBColor[0.36,0.65,0.88],
					InfiniteLine[{{0, yOffset + 10}, {1, yOffset + 10}}]
				}, *)
				Delimiter :> Raise[
					DiagramError,
					"Delimiter is no longer supported as a row specification"
				],
				_ :> (
					binaryLayoutDiagramRow[row, fontMultiplier]
				)
			}];

			(* FIXME: Label handling *)
			(* If[label =!= None,
				graphic = {
					graphic,
					Translate[label, {-$tileSize, yOffset + rowHeight/2}]
				}
			]; *)

			{rowHeight, items}
		],
		rows
	];

	BlockStackDiagram[blockRows]
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