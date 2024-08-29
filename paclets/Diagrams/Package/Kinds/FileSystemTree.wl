Package["Diagrams`Kinds`FileSystemTree`"]

PackageUse[Diagrams -> {
	FileSystemTreeDiagram,

	DiagramError,

	Utils -> {OutputElementsQ, ConstructOutputElements, ForwardOptions},
	Errors -> {
		Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirm, RaiseConfirm2, RaiseConfirmMatch, WrapRaised,
		RaiseAssert2
	}
}]

(*========================================================*)

Options[FileSystemTreeDiagram] = {
	LabelingFunction -> None
}

SetFallthroughError[FileSystemTreeDiagram]

(*
	Output elements include:

	* "Graphics" — a Wolfram graphics expression
	* "TextGraphics" — a textual visual representation of the tree
	* "ASCIIGraphics" — a textual visual representation of the tree using only
	    characters in the ASCII character set.
 *)
FileSystemTreeDiagram[
	tree0: _?DirectoryQ | _?TreeQ,
	outputElems: _?OutputElementsQ | Automatic | {"Custom", _List} : Automatic,
	optsSeq:OptionsPattern[]
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating FileSystemTreeDiagram"
] @ Catch @ Module[{
	tree = ConfirmReplace[tree0, {
		tr: _?TreeQ :> tr,
		dir: _?DirectoryQ :> FileSystemTree[dir]
	}]
},
	(* Handle {"Custom", ..} output spec if present. *)
	(* TID:240829/1: FileSystemTreeDiagram {"Custom", _} specification *)
	Replace[outputElems, {
		{"Custom",
			specs: _List,
			labelFunc: _ : None
		} :> Throw @ fileTreeTextGraphicsHelper[
			tree,
			specs,
			ForwardOptions[optsSeq]
		]
	}];

	ConstructOutputElements[
		outputElems,
		"Graphics",
		{
			"Graphics" :> fileTreeGraphics[tree],
			"TextGraphics" :> fileTreeTextGraphics[
				tree,
				With[{
					(* Work around Package Format parsing brokenness. *)
					tBar = FromCharacterCode@FromDigits["251C", 16],
					vert = FromCharacterCode@FromDigits["2502", 16],
					hori = FromCharacterCode@FromDigits["2500", 16],
					corn = FromCharacterCode@FromDigits["2514", 16]
				},
					{
						hori <> "   ",
						"    ",
						tBar <> hori <> hori <> " ",
						corn <> hori <> hori <> " "
					}
				],
				ForwardOptions[optsSeq]
			],
			"ASCIIGraphics" :> fileTreeTextGraphics[
				tree,
				{"|   ", "    ", "|-- ", "\-- "},
				ForwardOptions[optsSeq]
			]
		}
	]
]

(*======================================*)

SetFallthroughError[fileTreeGraphics]

(*======================================*)

Options[fileTreeTextGraphics] = {
	LabelingFunction -> None
}

SetFallthroughError[fileTreeTextGraphics]

fileTreeTextGraphics[
	rootTree: _?TreeQ,
	{
		innerContinue: _?StringQ,
		innerLast: _?StringQ,
		leafContinue: _?StringQ,
		leafLast: _?StringQ
	},
	optsSeq:OptionsPattern[]
] := (
	StringTrim @ StringJoin @ fileTreeTextGraphicsHelper[
		rootTree,
		{innerContinue, innerLast, leafContinue, leafLast},
		ForwardOptions[optsSeq]
	]
)

(*======================================*)

Options[fileTreeTextGraphicsHelper] = {
	LabelingFunction -> None
}

SetFallthroughError[fileTreeTextGraphicsHelper]

fileTreeTextGraphicsHelper[
	rootTree: _?TreeQ,
	{
		innerContinue: _,
		innerLast: _,
		leafContinue: _,
		leafLast: _
	},
	OptionsPattern[]
] := Block[{
	output = {},
	labelFunc = OptionValue[LabelingFunction],
	(* { isLast: __?BooleanQ } *)
	(* Whether the element being processed at the given depth is the last of
		its siblings. *)
	path = {},
	(* Workaround Package Format wonkiness. *)
	newline = FromCharacterCode[10],
	(* Functions *)
	write,
	visit
},
	(*================================*)
	(* Definitions                    *)
	(*================================*)

	SetFallthroughError[write];

	write[fragment: _] := (
		AppendTo[output, fragment];
	);

	(*--------------------------------*)

	SetFallthroughError[visit];

	visit[str: _?StringQ] := (
		write[str];

		(* TID:240829/2: FileSystemTreeDiagram LabelingFunction on ASCIIGraphics *)
		If[labelFunc =!= None,
			write[
				WrapRaised[DiagramError, "Error in custom LabelingFunction"][
					labelFunc[path, str]
				]
			];
		];

		write[newline];
	);

	visit[tree: _?TreeQ] := Module[{
		children
	},
		visit[TreeData[tree]];

		children = Replace[TreeChildren[tree], {
			None | {} :> Return[Null, Module]
		}];

		MapIndexed[
			{child, pos} |-> Module[{
				isLastChild = pos[[1]] === Length[children]
			}, Block[{
				(* Append to `path` the state about whether the subtree child
					we're currently visiting is the last of its siblings. *)
				path = Append[path, isLastChild]
			},
				RaiseAssert2[MatchQ[path, {__?BooleanQ}]];

				Scan[
					(* If this isn't the last child at this depth in the
						path, write a continuation line. *)
					isLast |-> If[isLast,
						write[innerLast],    (* "    " *)
						write[innerContinue] (* "|   " *)
					],
					Most[path]
				];

				(* If this is the last child at this depth, show a slanted
					tick to visually indicate this is the end. Otherwise show
					a 'T' line to visually connect with the next `child`. *)
				If[isLastChild,
					write[leafLast],    (* "\-- " *)
					write[leafContinue] (* "|-- " *)
				];

				visit[child];
			]],
			children
		];
	];

	(*================================*)
	(* Kickoff                        *)
	(*================================*)

	visit[rootTree];

	output
]