Needs["Wolfram`ErrorTools`V1`"]

(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

AppendTo[$ContextPath, "Diagrams`Misc`BinaryLayoutDiagrams`Private`"]

(*========================================================*)

VerificationTest[
	Context[treeToLayers],
	"Diagrams`BlockStack`"
]

VerificationTest[
	treeToLayers[Tree["label", None]],
	{
		{"label"}
	}
]

VerificationTest[
	treeToLayers @ Tree[{1, 2, 3}],
	{
		{1, 2, 3}
	}
]

VerificationTest[
	treeToLayers @ Tree["list", {1, 2, 3}],
	{
		{1, 2, 3},
		{Item["list", 3]}
	}
]

VerificationTest[
	treeToLayers @ Tree[
		"SIMD[UInt16, 3]",
		{
			Tree["UInt16", {"byte", "byte"}],
			Tree["UInt16", {"byte", "byte"}],
			Tree["UInt16", {"byte", "byte"}]
		}
	],
	{
		{"byte", "byte", "byte", "byte", "byte", "byte"},
		{Item["UInt16", 2], Item["UInt16", 2], Item["UInt16", 2]},
		{Item["SIMD[UInt16, 3]", 6]}
	}
]

VerificationTest[
	treeToLayers @ Tree[Null, None],
	{
		{Null}
	}
]

VerificationTest[
	treeToLayers @ Tree[Null, {}],
	{
		{Null}
	}
]

(* TID:240721/2: treeToLayers handling of existing Item[..] tree data *)
VerificationTest[
	treeToLayers @ Tree[
		Item["Foo", Background -> Red],
		{1, 2, 3}
	],
	{
		{1, 2, 3},
		{Item["Foo", 3, Background -> Red]}
	}
]

(* TID:240721/3: treeToLayers handling of Item[..] with custom width *)
VerificationTest[
	Handle[_Failure] @ treeToLayers @ Tree[
		Item["Foo", 10, Background -> Red],
		{1, 2, 3}
	],
	Failure[DiagramError, <|
		"MessageTemplate" -> "Unsupported custom Item width in tree node data: ``",
		"MessageParameters" -> {
			InputForm[Item["Foo", 10, Background -> RGBColor[1, 0, 0]]]
		}
	|>]
]

VerificationTest[
	treeToLayers @ TreeForType @ DiaStruct["Point", <|
		"x" -> "Int64",
		"y" -> "Int64"
	|>],
	{
		{
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]],
			Item["", Background -> RGBColor[0.6, 0.4, 0.2]]
		},
		{
			Item["x: Int64", 8, Background -> RGBColor[1, 0.9, 0.8]],
			Item["y: Int64", 8, Background -> RGBColor[1, 0.9, 0.8]]
		},
		{Item["Point", 16]}
	}
]

(* TID:240721/4: treeToLayers handling of mixed-depth subtrees. *)
VerificationTest[
	treeToLayers @ Tree["Root", {
		"A",
		"B",
		Tree["C", {"D", "E"}]
	}],
	{
		{"A", "B", "D", "E"},
		{
			Item["\[VerticalEllipsis]", 1, Background -> GrayLevel[1]],
			Item["\[VerticalEllipsis]", 1, Background -> GrayLevel[1]],
			Item["C", 2]
		},
		{
			Item["Root", 4]
		}
	}
]

(*====================================*)
(* Test errors                        *)
(*====================================*)

(* FIXME: Start checking for this an enable this test. *)
(* VerificationTest[
	treeToLayers @ Tree[
		"InlineList[Int16, 3]",
		{
			Tree["storage", {
				Tree["Int16", {"byte", "byte"}],
				Tree["Int16", {"byte", "byte"}],
				Tree["Int16", {"byte", "byte"}]
			}],
			"size: Int"
		}
	],
	(* ERROR: Cannot have different TreeDepth children (?) *)
] *)