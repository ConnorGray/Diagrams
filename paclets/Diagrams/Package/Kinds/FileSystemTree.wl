Package["Diagrams`Kinds`FileSystemTree`"]

PackageUse[Diagrams -> {
	FileSystemTreeDiagram,

	DiagramError,

	Utils -> {OutputElementsQ, ConstructOutputElements},
	Errors -> {
		Raise, Handle, ConfirmReplace, SetFallthroughError,
		RaiseConfirm, RaiseConfirm2, RaiseConfirmMatch, WrapRaised,
		RaiseAssert2
	}
}]

(*========================================================*)

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
	outputElems: _?OutputElementsQ | Automatic : Automatic
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating FileSystemTreeDiagram"
] @ Module[{
	tree = ConfirmReplace[tree0, {
		tr: _?TreeQ :> tr,
		dir: _?DirectoryQ :> FileSystemTree[dir]
	}]
},

	ConstructOutputElements[
		outputElems,
		"Graphics",
		{
			"Graphics" :> fileTreeGraphics[tree],
			"TextGraphics" :> fileTreeTextGraphics[
				tree,
				{"|   ", "    ", "|-- ", "\-- "}
			],
			"ASCIIGraphics" :> fileTreeTextGraphics[
				tree,
				{"|   ", "    ", "|-- ", "\-- "}
			]
		}
	]
]

(*======================================*)

SetFallthroughError[fileTreeGraphics]

(*======================================*)

SetFallthroughError[fileTreeTextGraphics]

fileTreeTextGraphics[
	rootTree: _?TreeQ,
	{
		innerContinue: _?StringQ,
		innerLast: _?StringQ,
		leafContinue: _?StringQ,
		leafLast: _?StringQ
	}
] := Block[{
	textOutput = "",
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

	write[str: _?StringQ] := (
		textOutput = StringJoin[textOutput, str]
	);

	(*--------------------------------*)

	SetFallthroughError[visit];

	visit[str: _?StringQ] := (
		write[str];
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

	StringTrim[textOutput]
]