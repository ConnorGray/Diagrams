(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]


(* TID:240724/1: DiaID on struct types *)
VerificationTest[
	StackHeapDiagram[{
		DiaStackVariable[
			"foo",
			DiaID["string"] @ DiaStruct["Point", <|
				"x" -> "Int64",
				"y" -> "Int64"
			|>]
		]
	}],
	Failure[DiagramError, <|
		"CausedBy" -> Failure[DiagramError, <|
			"MessageTemplate" -> "Cannot apply DiaID[..] to type that spans multiple rows: ``",
			"MessageParameters" -> {
				InputForm[
					DiaID["string"][
						DiaStruct["Point", <| "x" -> "Int64", "y" -> "Int64" |>]
					]
				]
			}
		|>],
		"MessageTemplate" -> "Error creating StackHeapDiagram",
		"MessageParameters" -> {}
	|>]
]

(* Test stack heap diagram without any indirections. *)
VerificationTest[
	StackHeapDiagram[{
		DiaStackVariable[
			"foo",
			DiaStruct["Point", <|
				"x" -> DiaID["x"] @ "Int64",
				"y" -> DiaID["y"] @ "Int64"
			|>]
		]
	}, "Regions"],
	<|
		DiaID["x"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["y"] -> Rectangle[{0, 1}, {8, 2}]
	|>
]