Package["Diagrams`Utils`"]

PackageExport[{
	RectangleWidth,
	RectangleHeight,
	RectangleSize,
	RectangleCenter,

	GraphemeClusters,
	UnicodeData
}]

PackageUse[Diagrams -> {
	DiagramError,
	Errors -> {
		RaiseError, SetFallthroughError, Handle, WrapRaised, RaiseConfirmMatch
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

(*====================================*)
(* Strings                            *)
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
