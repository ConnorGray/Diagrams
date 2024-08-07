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
			{1, {DiaID["foo"] @ Item["foo: Int64", 8, Background -> RGBColor[1, 0.9, 0.8]]}}
		}
	}
]

VerificationTest[
	stackVarToIndirectionColumns @ DiaStackVariable["foo", "Pointer"["Int64"]],
	{
		(* 1st column — Stack *)
		{
			{1, {DiaID["foo"] @ Item["foo: ptr to ...", 8, Background -> RGBColor[0.94, 0.88, 0.94]]}}
		},

		(* 2nd column — 1st level of heap indirection. *)
		{
			{1, {DiaID["foo.*"] @ Item["Int64", 8, Background -> RGBColor[1, 0.9, 0.8]]}}
		}
	}
]

VerificationTest[
	stackVarToIndirectionColumns @ DiaStackVariable[
		"foo",
		"Pointer"["Pointer"["UInt8"]]
	],
	{
		(* 1st column — Stack *)
		{
			{1, {DiaID["foo"] @ Item["foo: ptr to ...", 8, Background -> RGBColor[0.94, 0.88, 0.94]]}}
		},

		(* 2nd column — 1st level of heap indirection. *)
		{
			{1, {DiaID["foo.*"] @ Item["ptr to ...", 8, Background -> RGBColor[0.94, 0.88, 0.94]]}}
		},

		(* 3rd column — 2nd level of heap indirection. *)
		{
			{1, {DiaID["foo.*.*"] @ Item["UInt8", 1, Background -> RGBColor[1, 0.9, 0.8]]}}
		}
	}
]

(*====================================*)
(* Test typeToIndirectionColumns      *)
(*====================================*)

VerificationTest[
	typeToIndirectionColumns[{}, "Int64"],
	{
		(* 0th level of indirection. *)
		{DiaID[""] @ "Int64"}
	}
]

VerificationTest[
	typeToIndirectionColumns[{}, "Pointer"["Int64"]],
	{
		(* 1st level of indirection (no indirection). *)
		{DiaID[""] @ "Pointer"[DiaID["*"]]},

		(* 2nd level of indirection. *)
		{DiaID["*"] @ "Int64"}
	}
]

VerificationTest[
	typeToIndirectionColumns[{}, DiaStruct["String", <|
		"size" -> "Int64",
		"len" -> "Int64",
		"ptr" -> "Pointer"["UInt8"]
	|>]],
	{
		{
			DiaStruct["String", <|
				"size" -> DiaID["size"] @ "Int64",
				"len" -> DiaID["len"] @ "Int64",
				"ptr" -> DiaID["ptr"] @ "Pointer"[DiaID["ptr.*"]]
			|>]
		},
		{
			DiaID["ptr.*"] @ "UInt8"
		}
	}
]

(* TID:240725/1: Don't double wrap DiaID around nested pointer. *)
VerificationTest[
	typeToIndirectionColumns[{}, "Pointer"[
		"Pointer"[
			"Pointer"[
				"UInt8"
			]
		]
	]],
	{
		{
			DiaID[""] @ "Pointer"[DiaID["*"]]
		},
		{
			DiaID["*"] @ "Pointer"[DiaID["*.*"]]
		},
		{
			DiaID["*.*"] @ "Pointer"[DiaID["*.*.*"]]
		},
		{
			DiaID["*.*.*"] @ "UInt8"
		}
	}
]

VerificationTest[
	typeToIndirectionColumns[{}, DiaStruct["Data", <|
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
	|>]],
	{
		(* 0th level of indirection. *)
		{
			DiaStruct["Data", <|
				"ptr1" -> DiaID["ptr1"] @ "Pointer"[DiaID["ptr1.*"]],
				"ptr2" -> DiaID["ptr2"] @ "Pointer"[DiaID["ptr2.*"]],
				"ptr3" -> DiaID["ptr3"] @ "Pointer"[DiaID["ptr3.*"]]
			|>]
		},
		(* 1st level of indirection. *)
		{
			DiaID["ptr1.*"] @ "UInt8",
			DiaID["ptr2.*"] @ "Pointer"[DiaID["ptr2.*.*"]],
			DiaID["ptr3.*"] @ DiaStruct["Level1", <|
				"ptr" -> DiaID["ptr3.*.ptr"] @ "Pointer"[DiaID["ptr3.*.ptr.*"]]
			|>]
		},
		(* 2nd level of indirection. *)
		{
			DiaID["ptr2.*.*"] @ "UInt16",
			DiaID["ptr3.*.ptr.*"] @ DiaStruct["Level2", <|
				"ptr" -> DiaID["ptr3.*.ptr.*.ptr"] @ "Pointer"[DiaID["ptr3.*.ptr.*.ptr.*"]]
			|>]
		},
		(* 3rd level of indirection. *)
		{
			DiaID["ptr3.*.ptr.*.ptr.*"] @ "UInt32"
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
				(* TODO: The returned type here includes automatically added
					DiaID wrappers. It would be better if we only showed a type
					identical to what the user passed in. *)
				InputForm[
					DiaID["foo"] @ DiaID["string"][
						DiaStruct["Point", <|
							"x" -> DiaID["foo.x"] @ "Int64",
							"y" -> DiaID["foo.y"] @ "Int64"
						|>]
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
		DiaID["foo.x"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["x"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["foo.y"] -> Rectangle[{0, 1}, {8, 2}],
		DiaID["y"] -> Rectangle[{0, 1}, {8, 2}]
	|>
]

(* Test stack heap diagram without any indirections and redundant
	DiaIDs. *)
(* TID:240724/2: Support multiple DiaID wrappers. *)
VerificationTest[
	StackHeapDiagram[{
		DiaStackVariable[
			"foo",
			DiaStruct["Point", <|
				"x" -> DiaID["x"] @ DiaID["x2"] @ DiaID["x3"] @ "Int64",
				"y" -> DiaID["y"] @ "Int64"
			|>]
		]
	}, "Regions"],
	<|
		DiaID["foo.x"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["x"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["x2"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["x3"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["foo.y"] -> Rectangle[{0, 1}, {8, 2}],
		DiaID["y"] -> Rectangle[{0, 1}, {8, 2}]
	|>
]

(* Test stack/heap diagram of nested "Pointer" type. *)
VerificationTest[
	StackHeapDiagram[{
		DiaStackVariable[
			"foo",
			"Pointer"["Pointer"["UInt8"]]
		]
	}, "Regions"],
	<|
		DiaID["foo"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["foo.*"] -> Rectangle[{9.5, 1}, {17.5, 2}],
		DiaID["foo.*.*"] -> Rectangle[{19., 1}, {20., 2}]
	|>
]

(* Test stack/heap diagram of multiple variables, each with indirection. *)
VerificationTest[
	StackHeapDiagram[{
		DiaStackVariable[
			"foo",
			"Pointer"["Pointer"["UInt8"]]
		],
		DiaStackVariable[
			"bar",
			"Pointer"["Pointer"["UInt8"]]
		]
	}, "Regions"],
	<|
		DiaID["foo"] -> Rectangle[{0, 0}, {8, 1}],
		DiaID["bar"] -> Rectangle[{0, 1}, {8, 2}],
		DiaID["foo.*"] -> Rectangle[{9.5, 1}, {17.5, 2}],
		DiaID["bar.*"] -> Rectangle[{9.5, 3}, {17.5, 4}],
		DiaID["foo.*.*"] -> Rectangle[{19., 1}, {20., 2}],
		DiaID["bar.*.*"] -> Rectangle[{19., 3}, {20., 4}]
	|>
]

Module[{visual, graphic, regions},
	{visual, graphic, regions} = StackHeapDiagram[
		{
			DiaStackVariable["x", "Pointer"["Int64"]]
		},
		{"Visual", "Graphics", "Regions"},
		ChartLegends -> Automatic
	];

	VerificationTest[MatchQ[visual, _Labeled]];
	VerificationTest[MatchQ[graphic, _Graphics]];
	VerificationTest[
		regions,
		<|
			DiaID["x"] -> Rectangle[{0, 0}, {8, 1}],
			DiaID["x.*"] -> Rectangle[{9.5, 1}, {17.5, 2}]
		|>
	]
]
