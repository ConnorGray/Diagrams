(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

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