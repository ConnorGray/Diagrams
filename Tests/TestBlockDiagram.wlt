(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

(*========================================================*)

(* Test simple BlockDiagram of single root box. *)
Module[{diagram},
	diagram = BlockDiagram[
		DiaBox[
			"Process",
			Column[{
				DiaBox["Foo"],
				DiaBox["Bar"]
			}]
		],
		{}
	];

	VerificationTest[diagram, _Diagram, SameTest -> MatchQ];

	VerificationTest[Head[diagram], Diagram];

	VerificationTest[
		Keys[diagram[[1]]],
		{"Graphics"}
	];
]

(* TID:240908/1: BlockDiagram of box with Column of inner boxes. *)
Module[{diagram},
	diagram = BlockDiagram[
		{
			DiaBox[
				"Process",
				Column[{
					DiaBox["Foo"],
					DiaBox["Bar"]
				}]
			]
		},
		{},
		DiagramLayout -> {
			"EqualWidthRows",
			BoxPadding -> 48
		}
	];

	VerificationTest[Head[diagram], Diagram];

	VerificationTest[
		Keys[diagram[[1]]],
		{"Graphics"}
	];
]

(* TID:240908/2: BlockDiagram of box with Row of inner boxes. *)
Module[{diagram},
	diagram = BlockDiagram[
		DiaBox[
			"Parent",
			Row[{
				DiaBox["Foo"],
				DiaBox["Bar"]
			}]
		],
		{}
	];

	VerificationTest[Head[diagram], Diagram];

	VerificationTest[
		Keys[diagram[[1]]],
		{"Graphics"}
	];
]