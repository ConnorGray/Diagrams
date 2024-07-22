Package["Diagrams`Utils`"]

PackageExport[{
	RectangleWidth,
	RectangleHeight,
	RectangleSize,
	RectangleCenter,

	OutputElementsQ,
	ConstructOutputElements,

	ToCharacterCode2,
	GraphemeClusters,
	UnicodeData
}]

PackageUse[Diagrams -> {
	DiagramError,
	Errors -> {
		Raise, RaiseError, SetFallthroughError, Handle, WrapRaised,
		RaiseConfirmMatch, ConfirmReplace
	},
	Library -> {$LibraryFunctions}
}]

(*====================================*)

RectangleWidth[arg_] := Replace[arg, {
	Rectangle[{xMin_?NumberQ, _}, {xMax_?NumberQ, _}] :> Abs[xMax - xMin],
	_ :> RaiseError["unable to get width of rectangle: ``", arg]
}]

RectangleHeight[arg_] := Replace[arg, {
	Rectangle[{_, yMin_?NumberQ}, {_, yMax_?NumberQ}] :> Abs[yMax - yMin],
	_ :> RaiseError["unable to get height of rectangle: ``", arg]
}]

RectangleSize[arg_] := {RectangleWidth[arg], RectangleHeight[arg]}

RectangleCenter[arg_] := Replace[arg, {
	Rectangle[
		{xMin_?NumberQ, yMin_?NumberQ},
		{xMax_?NumberQ, yMax_?NumberQ}
	] :> {xMin + RectangleWidth[arg] / 2, yMin + RectangleHeight[arg] / 2},
	_ :> RaiseError["unable to get height of rectangle: ``", arg]
}]

(*========================================================*)
(* Developer UX                                           *)
(*========================================================*)

SetFallthroughError[OutputElementsQ]

OutputElementsQ[expr_] :=
	MatchQ[expr, _?StringQ | {___?StringQ} | Automatic]

(*====================================*)

SetFallthroughError[ConstructOutputElements]

ConstructOutputElements[
	outputElems: _?OutputElementsQ,
	default: _?StringQ,
	(* A list of rules or a function. *)
	getOutputElement0_
] := Module[{
	getOutputElement = getOutputElement0
},
	If[ListQ[getOutputElement0],
		getOutputElement = ({elem} |-> ConfirmReplace[
			elem,
			Append[
				getOutputElement0,
				other_ :> Raise[
					DiagramError,
					"Unrecognized output element requested: ``",
					InputForm[other]
				]
			]
		]);
	];

	(*--------------------------------*)

	ConfirmReplace[outputElems, {
		Automatic :> getOutputElement[default],
		element_?StringQ :> getOutputElement[element],
		elements:{___?StringQ} :> Map[getOutputElement, elements],
		other_ :> Raise[
			DiagramError,
			"Unrecognized output elements specification: ``",
			InputForm[other]
		]
	}]
]

(*========================================================*)
(* Strings                                                *)
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

(*====================================*)

SetFallthroughError[GraphemeClusters]

GraphemeClusters[text_?StringQ] := Module[{
	libFunc = $LibraryFunctions["grapheme_clusters"]
},
	libFunc[text]
]

(*====================================*)

SetFallthroughError[UnicodeData]

UnicodeData[] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error processing Unicode data"
] @ Module[{
	(* FIXME: Fix broken "\n" string literal in package format. *)
	newline = FromCharacterCode[10],
	data,
},
	(*--------------------------------------------------------*)
	(* Import from the file and divide it into the raw fields *)
	(*--------------------------------------------------------*)

	data = $LibraryFunctions["get_unicode_data"][];

	RaiseConfirmMatch[data, _?StringQ];

	data = StringSplit[data, newline];

	RaiseConfirmMatch[data, {Repeated[_?StringQ, {5, 100000}]}];

	data = Map[
		line |-> StringSplit[line, ";"],
		data
	];

	(*-----------------*)

	data
]

(*------------------------------------*)

UnicodeData["GeneralCategoryPropertyValues"] := Module[{},
	RaiseConfirmMatch[
		$LibraryFunctions["unicode_properties"][],
		{{_?StringQ, _?StringQ, _?StringQ}..}
	]
]

(*------------------------------------*)

UnicodeData[
	"MappedCharacterSetCodepoints",
	characterSet : _?StringQ | All
] := Module[{},
	RaiseConfirmMatch[
		$LibraryFunctions["get_mapped_character_set_codepoints"][characterSet],
		Alternatives[
			{ (_?IntegerQ -> _?IntegerQ) .. },
			{
				Repeated[
					_?StringQ -> { (_?IntegerQ -> _?IntegerQ) .. }
				]
			}
		]
	]
]
