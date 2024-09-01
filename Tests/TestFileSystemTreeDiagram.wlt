(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

(*====================================*)

VerificationTest[
	FileSystemTreeDiagram[
		Tree["Root", None],
		"ASCIIGraphics"
	],
	"Root"
]

VerificationTest[
	FileSystemTreeDiagram[
		Tree["Root", {"One", "Two", "Three"}],
		"ASCIIGraphics"
	],
	"\
Root
|-- One
|-- Two
\\-- Three"
]

VerificationTest[
	FileSystemTreeDiagram[
		RulesTree @ Rule["Project", {
			"README.md",
			"crates" -> {
				"project" -> {
						"Cargo.toml",
						"src" -> {
							"lib.rs"
						}
					},
				"project-wll"
			},
			"docs" -> {
				"Development.md",
				"Maintenence.md"
			}
		}],
		"ASCIIGraphics"
	],
	"\
Project
|-- README.md
|-- crates
|   |-- project
|   |   |-- Cargo.toml
|   |   \\-- src
|   |       \\-- lib.rs
|   \\-- project-wll
\\-- docs
    |-- Development.md
    \\-- Maintenence.md"
]

(* TID:240829/1: FileSystemTreeDiagram {"Custom", _} specification *)
VerificationTest[
	FileSystemTreeDiagram[
		Tree["Root", {
			"One", "Two",
			Tree["Three", {"Four"}],
			Tree["Five", {"Six"}]
		}],
		{"Custom", {"A ", "B ", "C ", "D "}}
	],
	{
		"Root",             "\n",
		"C ", "One",        "\n",
		"C ", "Two",        "\n",
		"C ", "Three",      "\n",
		"A ", "D ", "Four", "\n",
		"D ", "Five",       "\n",
		"B ", "D ", "Six",  "\n"
	}
]

(* TID:240829/2: FileSystemTreeDiagram LabelingFunction on ASCIIGraphics *)
VerificationTest[
	FileSystemTreeDiagram[
		Tree["Root", {
			"One", "Two",
			Tree["Three", {"Four"}],
			Tree["Five", {"Six"}]
		}],
		"ASCIIGraphics",
		LabelingFunction -> Function[{path, node},
			(* Label each node with its depth. *)
			" (" <> ToString[Length[path]] <> ")"
		]
	],
	"\
Root (0)
|-- One (1)
|-- Two (1)
|-- Three (1)
|   \\-- Four (2)
\\-- Five (1)
    \\-- Six (2)"
]

(* TID:240901/1: FileSystemTreeDiagram ItemDisplayFunction on ASCIIGraphics *)
(* Also tests composition of ItemDisplayFunction and LabelingFunction.  *)
VerificationTest[
	FileSystemTreeDiagram[
		Tree["Root", {
			"One",
			"Two",
			Tree["Three", {"Four"}],
			Tree["Five", {"Six"}]
		}],
		"ASCIIGraphics",
		ItemDisplayFunction -> Function[{node},
			Style[node, Bold]
		],
		LabelingFunction -> Function[{path, node},
			(*Label each node with its depth.*)
			" (" <> ToString[Length[path]] <> ")"
		]
	],
	Row[{
		Style["Root", Bold], " (0)", "\n",
		"|-- ", Style["One", Bold], " (1)", "\n",
		"|-- ", Style["Two", Bold], " (1)", "\n",
		"|-- ", Style["Three", Bold], " (1)", "\n",
		"|   ", "\\-- ", Style["Four", Bold], " (2)", "\n",
		"\\-- ", Style["Five", Bold], " (1)", "\n",
		"    ", "\\-- ", Style["Six", Bold], " (2)", "\n"
	}]
]