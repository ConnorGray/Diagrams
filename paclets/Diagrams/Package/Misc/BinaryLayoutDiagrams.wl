Package["Diagrams`Misc`BinaryLayoutDiagrams`"]

PackageExport[{
	(* TODO:
		Move to more general package. *)
	DiagramError,

	StringEncodingDiagram,
	BinaryLayoutDiagram,

	(*-----------*)
	(* Utilities *)
	(*-----------*)
	EncodedTile,

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

SetFallthroughError[StringEncodingDiagram]

StringEncodingDiagram[
	text_?StringQ,
	layers : {___}
] := Module[{
	handle,
	directives = {},
	yOffset = 0
},
	handle["Characters"] := Module[{},
		DiaCharacter[#, 1] & /@ Characters[text]
	];

	handle["Codepoints"] := Module[{},
		Map[
			(* FIXME:
				Not guaranteed to fit in one byte *)
			DiaCodepoint,
			ToCharacterCode[text]
		]
	];

	handle["Bytes"] := Module[{},
		Map[
			DiaByte,
			ToCharacterCode[text, "UTF-8"]
		]
	];

	handle["Bits"] := Module[{},
		Flatten @ Map[
			(* FIXME:
				Codepoints are not guaranteed to fit in 8 bits *)
			codepoint |-> Map[
					DiaBit,
					IntegerDigits[codepoint, 2, 8]
				],
			ToCharacterCode[text]
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

SetFallthroughError[EncodedTile]

EncodedTile[content_, size_, color0_, offset_, styleOpts___] :=
Module[{
	color = Replace[color0, Automatic :> RandomColor[Hue[_, 1, 0.7]]]
},
	{
		color,
		EdgeForm[{Thickness[0.005], Gray}],
		Rectangle[{offset, 0}, {offset + size, size}],
		ColorNegate[color],
		Text[
			Style[content, styleOpts, Bold, FontFamily -> "PT Mono"],
			{offset + size/2, size/2}
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
		{yOffset, elem} |-> Module[{expr, incr},
			{expr, incr} = ConfirmReplace[elem, {
				DiaBit[value:(0|1)] :> (
					{EncodedTile["", 10, GrayLevel[Clip[value, {0.15,0.95}]], yOffset, FontSize -> 8], 10}
				),
				DiaByte[value_?IntegerQ] /; NonNegative[value] && value <= 255 :> (
					{EncodedTile[value, 80, Brown, yOffset, FontSize -> 32], 80}
				),
				DiaCodepoint[value_?IntegerQ] :> (
					{EncodedTile[value, 80, Darker[Blue], yOffset, FontSize -> 32], 80}
				),
				DiaCharacter[char_?StringQ, byteWidth_?IntegerQ] :> (
					{EncodedTile[char, 80, Blue, yOffset, FontSize -> 32], 80}
				)
			}];
			{expr, yOffset + incr}
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
				_DiaBit -> 10,
				_DiaByte -> 80,
				_DiaCharacter -> 80,
				_DiaCodepoint -> 80,
				Delimiter -> 20
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
					Translate[label, {-80, yOffset + rowHeight/2}]
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