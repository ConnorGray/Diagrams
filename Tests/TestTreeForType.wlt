(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

Needs["Wolfram`ErrorTools`V1`"]

(*========================================================*)

(* TID:240721/1: Test unrecognized type errors *)
VerificationTest[
	Handle[_Failure] @ TreeForType["Fake64"],
	Failure[DiagramError, <|
		"CausedBy" -> Failure[DiagramError, <|
			"MessageTemplate" -> "Unrecognized type specification: ``",
			"MessageParameters" -> {InputForm["Fake64"]}
		|>],
		"MessageTemplate" -> "Error processing TreeForType: ``",
		"MessageParameters" -> {InputForm["Fake64"]}
	|>]
]
