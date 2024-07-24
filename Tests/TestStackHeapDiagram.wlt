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
