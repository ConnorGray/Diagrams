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

VerificationTest[
	treeForType @ DiaStruct["Point", <|
		"x" -> "Int64",
		"y" -> "Int64"
	|>],
	Tree["Point", {
		Tree["x: Int64", {
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None]
		}],
		Tree["y: Int64", {
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None],
			Tree[Item["", Background -> RGBColor[0.6, 0.4, 0.2]], None]
		}]
	}]
]