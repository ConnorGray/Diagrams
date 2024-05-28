Package["Diagrams`Utils`"]

PackageExport[{
	RectangleWidth,
	RectangleHeight,
	RectangleSize,
	RectangleCenter,

	GraphemeClusters
}]

PackageUse[Diagrams -> {
	Errors -> {RaiseError, SetFallthroughError},
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
