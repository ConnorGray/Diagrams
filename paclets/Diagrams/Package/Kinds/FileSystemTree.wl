Package["Diagrams`Kinds`FileSystemTree`"]

PackageUse[Diagrams -> {
	FileSystemTreeDiagram,

	DiagramError,

	Utils -> {OutputElementsQ, ConstructOutputElements, ForwardOptions},
	Errors -> {
		Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirm, RaiseConfirmMatch, WrapRaised,
		RaiseAssert
	}
}]

(*========================================================*)

(* Workaround Package Format wonkiness. *)
$newline = FromCharacterCode[10];

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
	outputElems: Alternatives[
		_?OutputElementsQ,
		Automatic,
		{"Custom", _}
	] : Automatic,
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
	LabelingFunction -> None,
	ItemDisplayFunction -> Automatic
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
] := Module[{output},
	output = fileTreeTextGraphicsHelper[
		rootTree,
		{innerContinue, innerLast, leafContinue, leafLast},
		ForwardOptions[optsSeq]
	];

	ConfirmReplace[output, {
		(* If all elements are a string, return a string. *)
		{___?StringQ} :> StringTrim[StringJoin[output]],
		_List :> (
			(* Assert that some custom option was specified. E.g.
				an option like ItemDisplayFunction -> (Style[#, Bold]&) should
				be the only way to get non-String values in the output list. *)
			RaiseAssert[
				{optsSeq} =!= {},
				"Unexpected non-list-of-strings result when no custom styling options were specified: ``",
				InputForm[output]
			];

			(* NOTE:
				This is an odd case where an option can change the *type*
				of the result (from String to Row). This is pragmantic for
				keeping the visual display correct. *)
			Row[output]
		)
	}]
]

(*======================================*)

Options[fileTreeTextGraphicsHelper] = {
	ItemDisplayFunction -> Automatic,
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
	itemDisplayFunc = Replace[
		OptionValue[ItemDisplayFunction],
		Automatic -> Identity
	],
	(* { isLast: __?BooleanQ } *)
	(* Whether the element being processed at the given depth is the last of
		its siblings. *)
	path = {},
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
		(* TID:240901/1:
			FileSystemTreeDiagram ItemDisplayFunction on ASCIIGraphics *)
		write[itemDisplayFunc[str]];

		(* TID:240829/2: FileSystemTreeDiagram LabelingFunction on ASCIIGraphics *)
		If[labelFunc =!= None,
			write[
				WrapRaised[DiagramError, "Error in custom LabelingFunction"][
					labelFunc[path, str]
				]
			];
		];

		write[$newline];
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
				RaiseAssert[MatchQ[path, {__?BooleanQ}]];

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