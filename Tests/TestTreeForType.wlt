(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

AppendTo[$ContextPath, "Diagrams`Misc`BinaryLayoutDiagrams`Private`"]

Needs["Wolfram`ErrorTools`V1`"]

(*========================================================*)

VerificationTest[
	Context[treeForType],
	"Diagrams`Misc`BinaryLayoutDiagrams`Private`"
]

(* TID:240721/1: Test unrecognized type errors *)
VerificationTest[
	Handle[_Failure] @ treeForType["Fake64"],
	Failure[DiagramError, <|
		"MessageTemplate" -> "Unrecognized type specification: ``",
		"MessageParameters" -> {InputForm["Fake64"]}
	|>]
]
