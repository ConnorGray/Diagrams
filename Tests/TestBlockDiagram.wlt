(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

(*========================================================*)

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