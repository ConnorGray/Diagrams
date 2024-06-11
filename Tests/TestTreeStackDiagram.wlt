Needs["Packages`"]

packageObj = PackageLoad[
	PackageSpecifier[PacletSpecifier["Diagrams", "0.0.1"], "Diagrams"]
]

$ContextPath = DeleteDuplicates @ Join[
	$ContextPath,
	{
		packageObj["PackageContext"],
		"Diagrams`Concepts`",
		"Diagrams`Layout`",
		"Diagrams`Misc`BinaryLayoutDiagrams`",
		"Diagrams`Utils`",
		"Diagrams`BlockStack`"
	}
];

(*========================================================*)

VerificationTest[
	Context[processTree],
	"Diagrams`BlockStack`"
]

VerificationTest[
	processTree[Tree["label", None]],
	{
		{"label"}
	}
]

VerificationTest[
	processTree @ Tree[{1, 2, 3}],
	{
		{1, 2, 3}
	}
]

VerificationTest[
	processTree @ Tree["list", {1, 2, 3}],
	{
		{1, 2, 3},
		{Item["list", 3]}
	}
]

VerificationTest[
	processTree @ Tree[
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
	processTree @ Tree[Null, None],
	{
		{Null}
	}
]

VerificationTest[
	processTree @ Tree[Null, {}],
	{
		{Null}
	}
]

(*====================================*)
(* Test errors                        *)
(*====================================*)

(* FIXME: Start checking for this an enable this test. *)
(* VerificationTest[
	processTree @ Tree[
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