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