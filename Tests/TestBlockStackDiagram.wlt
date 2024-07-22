(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

(*========================================================*)

VerificationTest[
	BlockStackDiagram[{
		{1, {
			DiaID["A"] @ "A",
			DiaID["B"] @ "B",
			DiaID["C"] @ "C"
		}}
	}, "Regions"],
	<|
		DiaID["A"] -> Rectangle[{0, 0}, {1, 1}],
		DiaID["B"] -> Rectangle[{1, 0}, {2, 1}],
		DiaID["C"] -> Rectangle[{2, 0}, {3, 1}]
	|>
]

(* TID:240721/5: Duplicate IDs in BlockStackDiagram *)
VerificationTest[
	BlockStackDiagram[{
		{1, {
			DiaID["A"] @ "A",
			DiaID["A"] @ "B"
		}}
	}, "Regions"],
	Failure[DiagramError, <|
		"CausedBy" -> Failure[DiagramError, <|
			"MessageTemplate" -> "Region value already defined for ID: ``, value: ``",
			"MessageParameters" -> {
				InputForm[DiaID["A"]],
				InputForm[Rectangle[{1, 0}, {2, 1}]]
			}
		|>],
		"MessageTemplate" -> "Error creating BlockStackDiagram",
		"MessageParameters" -> {}
	|>]
]