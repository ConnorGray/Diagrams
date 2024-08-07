Package["Diagrams`Misc`BinaryLayoutDiagrams`"]

PackageExport[{
	StringEncodingDiagram,
	CharacterSetDiagram,
	BinaryLayoutDiagram,
	MemoryLayoutDiagram,

	(*----------*)
	(* Concepts *)
	(*----------*)
	DiaString,
	DiaGrapheme,
	DiaCharacter,
	DiaCodepoint,
	DiaByte,
	DiaBit,

	DiaStruct,

	(*------------*)
	(* Helpers    *)
	(*------------*)
	TreeForType,

	(*---------------*)
	(* Configuration *)
	(*---------------*)
	$ColorScheme
}]

PackageUse[Diagrams -> {
	DiagramError,
	DiagramGraphicsImage,
	BlockStackDiagram,
	TreeStackDiagram,
	Errors -> {
		Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirm, RaiseConfirm2, RaiseConfirmMatch, WrapRaised,
		RaiseAssert
	},
	Library -> {$LibraryFunctions},
	Render -> {
		SizedText
	},
	Utils -> {
		ToCharacterCode2, GraphemeClusters, UnicodeData,
		OutputElementsQ, ConstructOutputElements
	}
}]

$ColorScheme = <|
	"Byte" -> Brown,
	"Codepoint" -> Darker[Blue],
	"Character" -> Blue,
	"Grapheme" -> Blend[{Green, GrayLevel[0.3]}, 0.8],
	"String" -> GrayLevel[0.95],

	"Integer" -> LightOrange,
	"Pointer" -> LightPurple,

	"CodepointUnassigned" -> LightGray,
	"CodepointSameAsUnicode" -> LightGreen,
	"CodepointDifferentFromUnicode" -> LightBlue
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

Options[StringEncodingDiagram] = Join[
	{
		CharacterEncoding -> "__unused",
		ImageSize -> Automatic,
		ChartLegends -> None
	},
	Options[DiaString]
]

SetFallthroughError[StringEncodingDiagram]

StringEncodingDiagram[
	text_?StringQ,
	layers : {___},
	encoding0 : (_?StringQ | Automatic) : Automatic,
	opts:OptionsPattern[]
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating StringEncodingDiagram"
] @ Module[{
	encoding = Replace[encoding0, Automatic -> "UTF-8"],
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
	RaiseConfirmMatch[
		OptionValue[CharacterEncoding],
		"__unused",
		"CharacterEncoding option has been deprecated. Use 2nd argument instead."
	];

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
			legend = layersSwatchLegend[layers]
		},
			Labeled[graphic, legend, Right]
		],
		other_ :> Raise[
			DiagramError,
			"Unsupported ChartLegends option value: ``",
			InputForm[other]
		]
	}]
]

(*====================================*)

Options[CharacterSetDiagram] = {
	ChartLegends -> None
}

SetFallthroughError[CharacterSetDiagram]

(* TODO: Rename to CodePageDiagram? *)
CharacterSetDiagram[
	charSet0 : _?StringQ | Association[(_?IntegerQ -> _?IntegerQ)..],
	visualization_?StringQ,
	optsSeq:OptionsPattern[]
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating CharacterSetDiagram"
] @ Module[{
	charSet = Replace[
		charSet0,
		_?StringQ :> Association @ RaiseConfirmMatch[
			UnicodeData["MappedCharacterSetCodepoints", charSet0],
			{ (_?IntegerQ -> _?IntegerQ).. }
		]
	]
},
	ConfirmReplace[visualization, {
		"8BitEncodingUnicodeCorrespondance" :> (
			show8BitCharacterSetUnicodeCorrespondance[
				charSet,
				(* Pass the name of the character set, if we have it. *)
				Replace[charSet0, Except[_?StringQ] :> Sequence[]],
				optsSeq
			]
		),
		_ :> Raise[
			DiagramError,
			"Unrecognized character set visualization specification: ``",
			InputForm[visualization]
		]
	}]
]

(*====================================*)

SetFallthroughError[show8BitCharacterSetUnicodeCorrespondance];

show8BitCharacterSetUnicodeCorrespondance[
	codepointsMap_?AssociationQ,
	characterSetName : _?StringQ : None,
	OptionsPattern[CharacterSetDiagram]
] := Module[{
	items,
	unicodeCodepoint,
	graphic
},
	RaiseAssert[Max[Keys[codepointsMap]] <= 255];

	items = Table[
		(* Possible states:
			* Codepoint is unassigned in original character set
			* Codepoint assignment in original character set has same
				codepoint value as in Unicode.
			* Codepoint assignment in original character set
				has different codepoint value in Unicode.
		*)
		(
			If[KeyExistsQ[codepointsMap, codepoint],
				unicodeCodepoint = Lookup[codepointsMap, codepoint];

				ConfirmReplace[unicodeCodepoint, {
					codepoint :> Item[
						FromCharacterCode[unicodeCodepoint],
						Background -> $ColorScheme["CodepointSameAsUnicode"]
					],
					_ :> Item[
						FromCharacterCode[unicodeCodepoint],
						Background -> $ColorScheme["CodepointDifferentFromUnicode"]
					]
				}]
				,
				(* Codepoint is unassigned in original encoding *)
				Item[
					Text @ Style[
						"<" <> IntegerString[codepoint, 16, 2] <> ">",
						8
					],
					Background -> $ColorScheme["CodepointUnassigned"]
				]
			]
		),
		{codepoint, 0, 255}
	];

	graphic = BlockStackDiagram[
		Reverse @ Map[
			row |-> {1, row},
			Partition[items, UpTo[16]]
		]
	];

	(*--------------------------------*)
	(* Include row and column labels  *)
	(*--------------------------------*)

	graphic = Show[graphic, Graphics[{
		Table[
			Inset[
				Style[Row[{
					"" <> ToUpperCase@IntegerString[i, 16],
					Style["x ", Italic]
				}], 14],
				{0, 16 - i - 0.5},
				Right
			],
			{i, 0, 15, 1}
		],
		Table[
			Inset[
				Style[Row[{
					Style["x", Italic],
					ToUpperCase@IntegerString[i, 16],
				}], 14],
				{i + 0.5, 16},
				Bottom
			],
			{i, 0, 15, 1}
		]
	}]];

	(*--------------------------------*)
	(* Handle the ChartLegends option *)
	(*--------------------------------*)

	graphic = ConfirmReplace[OptionValue[ChartLegends], {
		None :> graphic,
		Automatic :> Module[{
			legend
		},
			legend = SwatchLegend[
				{
					$ColorScheme["CodepointSameAsUnicode"],
					$ColorScheme["CodepointDifferentFromUnicode"],
					$ColorScheme["CodepointUnassigned"]
				},
				{
					"Same value as Unicode",
					"Different value from Unicode",
					"Unassigned"
				},
				LegendMarkerSize -> 20
			];

			Labeled[
				graphic,
				{legend, Style[characterSetName, "Text", 18]},
				{Right, Bottom}
			]
		],
		other_ :> Raise[
			DiagramError,
			"Unsupported ChartLegends option value: ``",
			InputForm[other]
		]
	}];

	graphic
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
							{width / 4, 0.8}
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

(*====================================*)

Options[BinaryLayoutDiagram] = {
	ChartLegends -> None
}

SetFallthroughError[BinaryLayoutDiagram]

BinaryLayoutDiagram[
	rows:{(_List | Delimiter | _Labeled)...},
	fontMultiplier : _ : 0.0045,
	OptionsPattern[]
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating BinaryLayoutDiagram"
] @ Module[{
	blockRows,
	graphic
},
	blockRows = Map[
		row0 |-> Module[{
			row = row0,
			label = None,
			rowHeight
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
					1 / 8
				),
				_DiaByte
				| _DiaCodepoint
				| _DiaCharacter
				| _DiaGrapheme
				| _DiaString -> (
					1
				),
				Delimiter -> 1 / 4
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

	graphic = BlockStackDiagram[blockRows];

	(*--------------------------------*)
	(* Handle the ChartLegends option *)
	(*--------------------------------*)

	ConfirmReplace[OptionValue[ChartLegends], {
		None :> graphic,
		layers:{___?StringQ} :> Module[{
			legend = layersSwatchLegend[layers]
		},
			Labeled[graphic, legend, Right]
		],
		other_ :> Raise[
			DiagramError,
			"Unsupported ChartLegends option value: ``",
			InputForm[other]
		]
	}]
]

(*====================================*)

SetFallthroughError[layersSwatchLegend]

layersSwatchLegend[layers: {___?StringQ}] := Module[{
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

	legend
]

(*========================================================*)

MemoryLayoutDiagram[
	type_
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating MemoryLayoutDiagram"
] @ Module[{
	tree
},
	tree = TreeForType[type];

	TreeStackDiagram[tree]
]

(*====================================*)

SetFallthroughError[TreeForType]

$indirectionLayers := Raise[DiagramError, "Invalid unscoped access of $indirectionLayers"]
$indirectionDepth := Raise[DiagramError, "Invalid unscoped access of $indirectionDepth"]

(*
	For users who want to customize the tree layout at a lower level before
	passing it to TreeStackDiagram.
*)
TreeForType[
	type_,
	outputElems : _?OutputElementsQ : Automatic
] := WrapRaised[
	DiagramError,
	"Error processing TreeForType: ``",
	InputForm[type]
] @ Block[{
	(* FIXME:
		Hard-coded max of 10 layers of indirection.
		Good luck if you need more.
	*)
	$indirectionLayers = AssociationMap[{} &, Range[10]],
	$indirectionDepth = 0
}, Module[{
	tree
},
	tree = treeForType[type];

	RaiseAssert[$indirectionDepth === 0];

	$indirectionLayers //= DeleteCases[{}];

	(* TODO: Return or render this information? *)
	$indirectionLayers = Values[$indirectionLayers];

	ConstructOutputElements[
		outputElems,
		"Graphics",
		{
			"Graphics" :> tree,
			"IndirectionLayers" :> $indirectionLayers
		}
	]
]]

(*------------------------------------*)

SetFallthroughError[treeForType]

(* FIXME: Support TypeSpecifier[..] type specifications. *)
treeForType[type_] := ConfirmReplace[type, {
	DiaStruct[structName_?StringQ, fields_?AssociationQ] :> (
		RaiseConfirmMatch[fields, Association[(_?StringQ -> _)...]];

		Tree[
			structName,
			KeyValueMap[
				{fieldName, fieldType} |-> (
					(* Prepend the field name to the label for the field type
						tree. Take care to preserve any styling options in
						Item[..] type labels. *)
					ConfirmReplace[treeForType[fieldType], {
						Tree[Item[label_, opts___], children_] :> (
							Tree[
								Item[
									fieldName <> ": " <> ToString[label],
									opts
								],
								children
							]
						),
						Tree[fieldTypeLabel_, children_] :> (
							Tree[
								fieldName <> ": " <> ToString[fieldTypeLabel],
								children
							]
						)
					}]
				),
				fields
			]
		]
	),

	"UInt8" | "Int8" |
	"UInt16" | "Int16" |
	"UInt32" | "Int32" |
	"UInt64" | "Int64" :> (
		Tree[
			Item[type, Background -> $ColorScheme["Integer"]],
			Table[
				Item["", Background -> $ColorScheme["Byte"]],
				sizeOf[type]
			]
		]
	),

	"PrimitiveArray"[arrayElemType_, count_?IntegerQ] :> (
		Tree[
			ToString["PrimitiveArray"["..", count]],
			Table[treeForType[arrayElemType], count]
		]
	),

	"Pointer"[
		pointee_,
		(* TODO: Support "SelfReferential"[..] as a location. *)
		location : ("Heap" | "Stack" | "Generic") : "Generic"
	] :> Block[{
		$indirectionDepth = $indirectionDepth + 1
	},
		AppendTo[
			$indirectionLayers[$indirectionDepth],
			treeForType[pointee]
		];

		Tree[
			Item[
				"Pointer[" <> ToString[pointee] <> "]",
				Background -> $ColorScheme["Pointer"]
			],
			Table[Item["", Background -> $ColorScheme["Byte"]], $pointerSize]
		]
	],

	(* TID:240721/1: Test unrecognized type errors *)
	other_ :> Raise[
		DiagramError,
		"Unrecognized type specification: ``",
		InputForm[other]
	]
}]

(*====================================*)

$pointerSize = 8

SetFallthroughError[sizeOf]

sizeOf[type_] := WrapRaised[
	DiagramError,
	"Error computing sizeOf: ``",
	InputForm[type]
] @ ConfirmReplace[type, {
	int:("UInt8" | "Int8")    :> 1,
	int:("UInt16" | "Int16")  :> 2,
	int:("UInt32" | "Int32")  :> 4,
	int:("UInt64" | "Int64")  :> 8,

	"Pointer"[pointee_] :> $pointerSize,

	"PrimitiveArray"[arrayElemType_, count_?IntegerQ] :> (
		count * sizeOf[arrayElemType]
	),

	other_ :> Raise[
		DiagramError,
		"Unrecognzied type specification: ``",
		InputForm[other]
	]
}]

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)
