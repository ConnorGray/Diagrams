Package["Diagrams`Misc`BinaryLayoutDiagrams`"]

PackageExport[{
	StringEncodingDiagram,
	CharacterSetDiagram,
	BinaryLayoutDiagram,
	MemoryLayoutDiagram,
	StackHeapDiagram,

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
	(* TODO: Replace with DiaStackFrame[.., <| "var1" -> ... |>] instead? *)
	DiaStackVariable,

	(*------------*)
	(* Helpers    *)
	(*------------*)
	TreeForType,

	IntegerTypeQ,

	(* NOTE: Public for testing purposes. *)
	stackVarToIndirectionColumns,
	typeToIndirectionColumns,

	(*---------------*)
	(* Configuration *)
	(*---------------*)
	$ColorScheme
}]

PackageUse[Diagrams -> {
	DiagramError,
	DiagramGraphicsImage,
	BlockStackDiagram,
	MultiBlockStackDiagram,
	TreeStackDiagram,
	DiaID,
	Errors -> {
		Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirm, RaiseConfirm2, RaiseConfirmMatch, WrapRaised,
		RaiseAssert, RaiseAssert2
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
		"Tree",
		{
			"Tree" :> tree,
			(* TODO: This is unused. Remove this logic from TreeForType. *)
			"IndirectionLayers" :> $indirectionLayers
		}
	]
]]

(*------------------------------------*)

SetFallthroughError[treeForType]

(* FIXME: Support TypeSpecifier[..] type specifications. *)
treeForType[type_] := ConfirmReplace[type, {
	(* Leave DiaID[_][_] wrappers untouched. *)
	id_DiaID[type1_] :> id[treeForType[type1]],

	DiaStruct[structName_?StringQ, fields_?AssociationQ] :> (
		RaiseConfirmMatch[fields, Association[(_?StringQ -> _)...]];

		Tree[
			structName,
			KeyValueMap[
				{fieldName, fieldType0} |-> Module[{
					fieldType = fieldType0,
					id = None
				},
					(* Temporarily remove the optional DiaID wrapper. *)
					fieldType //= Replace[id0_DiaID[type1_] :> (
						id = id0;
						type1
					)];

					(* Prepend the field name to the label for the field type
						tree. Take care to preserve any styling options in
						Item[..] type labels. *)
					fieldType = ConfirmReplace[treeForType[fieldType], {
						Tree[Item[label_?StringQ, opts___], children_] :> (
							Tree[
								Item[
									fieldName <> ": " <> label,
									opts
								],
								children
							]
						),
						Tree[fieldTypeLabel_?StringQ, children_] :> (
							Tree[
								fieldName <> ": " <> fieldTypeLabel,
								children
							]
						)
					}];

					If[id =!= None,
						fieldType = id[fieldType];
					];

					fieldType
				],
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
			(* FIXME: Maybe we shouldn't have this be part of TreeForType at
				all? *)
			pointee
			(* treeForType[pointee] *)
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

SetFallthroughError[StackHeapDiagram]

StackHeapDiagram[
	stackData_List,
	outputElems : _?OutputElementsQ : Automatic
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating StackHeapDiagram"
] @ Module[{
	stackElementsColumns,
	heapColumns
},
	stackElementsColumns = Map[
		stackElement |-> ConfirmReplace[stackElement, {
			stackVar:DiaStackVariable[__] :> (
				stackVarToIndirectionColumns[stackVar]
			),
			other_ :> Raise[
				DiagramError,
				"Unrecognized stack element specification: ``",
				InputForm[other]
			]
		}],
		stackData
	];

	(* Vertically join the columns from each "stack element layer" into
		a single mega column for each level of indirection. *)
	(* TODO: Maybe it would be easier of MultiBlockStackDiagram[..] took
		and argument structure of separate layers and did this joining
		itself.

		It could even support options controlling if the layers should be
		"smooshed" together or shown in separate horizontal "swimlanes". *)
	stackElementsColumns = Join[Sequence @@ stackElementsColumns, 2];

	RaiseAssert2[
		MatchQ[
			stackElementsColumns,
			{
				(* Nth column *)
				{
					(* Item in column *)
					{_?NumberQ, _List}...
				} ...
			}
		],
		<| "Stack" -> stackElementsColumns |>,
		"Unexpected processed stack elements structure"
	];

	(* Surround the items in the heap columns with visual indicator (vertical
		ellipsis) that they are not contiguous in memory. *)

	heapColumns = stackElementsColumns[[2 ;;]];

	heapColumns = Map[
		column |-> Riffle[
			column,
			(* Note: Show a vertial ellipsis, working around Packages parse
				error. *)
			{{1, {Item[FromCharacterCode[8942], Background -> White]}}},
			{1, 2 * Length[column] + 1, 2}
		],
		heapColumns
	];

	stackElementsColumns[[2 ;;]] = heapColumns;

	MultiBlockStackDiagram[
		stackElementsColumns,
		{},
		1.5,
		outputElems
	]
]

(*====================================*)

SetFallthroughError[stackVarToIndirectionColumns]

(*
	Input `type`: `DiaStruct[..]`

	(* TODO: Update this comment *)
	Output: {
		{1, {Item["name.field1", ...]}},
		{1, {Item["name.field2", ...]}},
	}
 *)
stackVarToIndirectionColumns[
	DiaStackVariable[varName_?StringQ, type_]
] := Module[{
	columns
},
	columns = typeToIndirectionColumns[type];

	RaiseAssert[MatchQ[columns, {__List}]];

	columns = MapIndexed[
		{column, pos} |-> Module[{
			name = Replace[pos, {
				{1} -> {varName},
				_ -> None
			}]
		},
			RaiseAssert[ListQ[column]];

			Flatten[#, 1]& @ Map[
				type1 |-> typeToItems[name, type1],
				column
			]
		],
		columns
	];

	columns
]

(*------------------------------------*)

SetFallthroughError[typeToIndirectionColumns]

typeToIndirectionColumns[type0_] := Module[{
	process,
	addIndirection,
	nextColumn,
	indirectionColumns
},
	(*----------------------*)
	(* Functions            *)
	(*----------------------*)

	SetFallthroughError[process];

	process[type1_] := ConfirmReplace[type1, {
		id_DiaID[type2_] :> (
			id[process[type2]]
		),

		DiaStruct[structName_?StringQ, structFields_?AssociationQ] :> Module[{},
			DiaStruct[
				structName,
				Association @ KeyValueMap[
					{fieldName, fieldType} |-> (
						fieldName -> process[fieldType]
					),
					structFields
				]
			]
		],

		"Pointer"[pointee_] :> Module[{
			destDiaID = addIndirection[pointee]
		},
			RaiseAssert[MatchQ[destDiaID, DiaID[_]]];

			"Pointer"[destDiaID]
		],

		int_?IntegerTypeQ :> int,

		other_ :> Raise[
			DiagramError,
			"Unsupported or malformed type: ``",
			InputForm[other]
		]
	}];

	(*----------------------*)

	SetFallthroughError[addIndirection];

	addIndirection[type_] := Module[{diaID},
		(* Construct a DiaID that points by ID value and with a Position-like
		   part specifiction to the pointee type. *)
		(* TODO: If access to this Position part does not end up being used,
			remove this and refactor this logic down to something simpler,
			like a FoldList. *)
		diaID = DiaID[{
			"Indirection",
			{Length[indirectionColumns] + 2, Length[nextColumn] + 1}
		}];

		AppendTo[nextColumn, type];

		diaID
	];

	(*----------------------*)
	(* Start work           *)
	(*----------------------*)

	indirectionColumns = {};
	nextColumn = {type0};

	(* Each iteration of this loop processes one level of indirection.
		`nextColumn` contains the types that were behind a pointer in the
		previous iteration of this loop. *)
	While[nextColumn =!= {}, Module[{
		saveNext = nextColumn
	},
		(* Reset `nextColumn`, to be populated with the types from the next
			level of indirection when we loop over the type from the previous
			level of indirection ("expanding the indirection frontier"). *)
		nextColumn = {};

		AppendTo[
			indirectionColumns,
			Map[process, saveNext]
		];
	]];

	indirectionColumns
]

(*------------------------------------*)

(* TODO: Rename to indirectType to items? *)
SetFallthroughError[typeToItems]

(*
	Returns one of:

		* Nothing
		* { {1, {Item[__]}} ... }
*)
typeToItems[
	namePath : {__?StringQ} | None,
	type_
] := ConfirmReplace[type, {
	id_DiaID[type1_] :> Module[{
		items = typeToItems[namePath, type1]
	},
		ConfirmReplace[items, {
			{ {size_, { column: _Item }} } :> { {size, { id[column] }} },
			(* TID:240724/1: DiaID on struct types *)
			{_, __} :> Raise[
				DiagramError,
				"Cannot apply DiaID[..] to type that spans multiple rows: ``",
				InputForm[type]
			],
			other_ :> Raise[
				DiagramError,
				"Unexpected typeToItems result: ``",
				InputForm[other]
			]
		}]
	],

	_?IntegerTypeQ :> {
		{1, {
			Item[
				withNamePath[namePath, ToString[type]],
				sizeOf[type],
				Background -> $ColorScheme["Integer"]
			]
		}}
	},

	DiaStruct[_?StringQ, fields_?AssociationQ] :> (
		Flatten[#, 1]& @ KeyValueMap[
			{fieldName, fieldType} |-> (
				typeToItems[Append[namePath, fieldName], fieldType]
			),
			fields
		]
	),

	"Pointer"[pointee_DiaID] :> {
		{1, {
			Item[
				(* withNamePath[namePath, "ptr to " <> ToString[pointee]], *)
				withNamePath[namePath, "ptr to ..."],
				sizeOf[type],
				Background -> $ColorScheme["Pointer"]
			]
		}}
	},

	other_ :> Raise[
		DiagramError,
		"Unsupported or unrecognized type: ``",
		InputForm[other]
	]
}]

(*------------------------------------*)

SetFallthroughError[withNamePath]

withNamePath[namePath: _, typeName: _?StringQ] :=
	ConfirmReplace[namePath, {
		None :> typeName,
		{__?StringQ} :> StringRiffle[namePath, "."] <> ": " <> typeName
	}]

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)

SetFallthroughError[IntegerTypeQ]

IntegerTypeQ[type_] := MatchQ[
	type,
	"UInt8" | "Int8" |
	"UInt16" | "Int16" |
	"UInt32" | "Int32" |
	"UInt64" | "Int64"
]
