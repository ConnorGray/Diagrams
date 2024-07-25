(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

(*====================================*)
(* Test stackVarToIndirectionColumns  *)
(*====================================*)

VerificationTest[
	stackVarToIndirectionColumns @ DiaStackVariable["foo", "Int64"],
	{
		(* 1st column - Stack *)
		{
			{1, {Item["foo: Int64", 8, Background -> RGBColor[1, 0.9, 0.8]]}}
		}
	}
]

VerificationTest[
	stackVarToIndirectionColumns @ DiaStackVariable["foo", "Pointer"["Int64"]],
	{
		(* 1st column — Stack *)
		{
			{1, {Item["foo: ptr to ...", 8, Background -> RGBColor[0.94, 0.88, 0.94]]}}
		},

		(* 2nd column — 1st level of heap indirection. *)
		{
			{1, {Item["Int64", 8, Background -> RGBColor[1, 0.9, 0.8]]}}
		}
	}
]

(*====================================*)
(* Test typeToIndirectionColumns      *)
(*====================================*)

VerificationTest[
	typeToIndirectionColumns["Int64"],
	{
		(* 0th level of indirection. *)
		{"Int64"}
	}
]

VerificationTest[
	typeToIndirectionColumns @ "Pointer"["Int64"],
	{
		(* 1st level of indirection (no indirection). *)
		{"Pointer"[DiaID[{"Indirection", {2, 1}}]]},

		(* 2nd level of indirection. *)
		{"Int64"}
	}
]

VerificationTest[
	typeToIndirectionColumns @ DiaStruct["String", <|
		"size" -> "Int64",
		"len" -> "Int64",
		"ptr" -> "Pointer"["UInt8"]
	|>],
	{
		{
			DiaStruct["String", <|
				"size" -> "Int64",
				"len" -> "Int64",
				"ptr" -> "Pointer"[DiaID[{"Indirection", {2, 1}}]]
			|>]
		},
		{
			"UInt8"
		}
	}
]

VerificationTest[
	typeToIndirectionColumns @ "Pointer"[
		"Pointer"[
			"Pointer"[
				"UInt8"
			]
		]
	],
	{
		{
			"Pointer"[DiaID[{"Indirection", {2, 1}}]]
		},
		{
			"Pointer"[DiaID[{"Indirection", {3, 1}}]]
		},
		{
			"Pointer"[DiaID[{"Indirection", {4, 1}}]]
		},
		{
			"UInt8"
		}
	}
]

VerificationTest[
	typeToIndirectionColumns @ DiaStruct["Data", <|
		"ptr1" -> "Pointer"["UInt8"],
		"ptr2" -> "Pointer"["Pointer"["UInt16"]],
		"ptr3" -> "Pointer"[
			DiaStruct["Level1", <|
				"ptr" -> "Pointer"[
					DiaStruct["Level2", <|
						"ptr" -> "Pointer"["UInt32"]
					|>]
				]
			|>]
		]
	|>],
	{
		(* 0th level of indirection. *)
		{
			DiaStruct["Data", <|
				"ptr1" -> "Pointer"[DiaID[{"Indirection", {2, 1}}]],
				"ptr2" -> "Pointer"[DiaID[{"Indirection", {2, 2}}]],
				"ptr3" -> "Pointer"[DiaID[{"Indirection", {2, 3}}]]
			|>]
		},
		(* 1st level of indirection. *)
		{
			"UInt8",
			"Pointer"[DiaID[{"Indirection", {3, 1}}]],
			DiaStruct["Level1", <|
				"ptr" -> "Pointer"[DiaID[{"Indirection", {3, 2}}]]
			|>]
		},
		(* 2nd level of indirection. *)
		{
			"UInt16",
			DiaStruct["Level2", <|
				"ptr" -> "Pointer"[DiaID[{"Indirection", {4, 1}}]]
			|>]
		},
		(* 3rd level of indirection. *)
		{
			"UInt32"
		}
	}
]

(*====================================*)
(* Test StackHeapDiagram              *)
(*====================================*)

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
