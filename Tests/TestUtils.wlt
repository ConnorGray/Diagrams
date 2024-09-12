(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

Needs["Wolfram`ErrorTools`"]

(*========================================================*)
(* Layout Utilities                                       *)
(*========================================================*)

VerificationTest[
	RectangleAttachmentPoint[
		Rectangle[{0, 0}, {1, 1}],
		{Left, 0.5}
	],
	{0, 0.5}
]

VerificationTest[
	RectangleAttachmentPoint[
		Rectangle[{0, 0}, {1, 1}],
		{Top, 0.25}
	],
	{0.25, 1}
]

VerificationTest[
	RectangleAttachmentPoint[
		Rectangle[{0, 0}, {100, 50}],
		{Top, 0.5}
	],
	{50., 50}
]

(*========================================================*)
(* Developer UX                                           *)
(*========================================================*)

(*====================================*)
(* ForwardOptions                     *)
(*====================================*)

Module[{
	foo, bar
},
	(*-------*)
	(* Setup *)
	(*-------*)

	Options[foo] = {
		"A" -> 1,
		"B" -> 2
	};

	foo[arg_, optsSeq : OptionsPattern[]] := Module[{},
		bar[arg, ForwardOptions[optsSeq]]
	];

	Options[bar] = {
		"B" -> 3
	};

	bar[arg_, optsSeq : OptionsPattern[]] := Module[{},
		{arg, "bar opts"[optsSeq]}
	];

	(*-------*)
	(* Tests *)
	(*-------*)

	VerificationTest[
		foo[5, "A" -> 1],
		{5, "bar opts"[]}
	];

	VerificationTest[
		foo[5, "B" -> 2],
		{5, "bar opts"["B" -> 2]}
	];

	VerificationTest[
		foo[5, ForwardOptions["B" -> 10]],
		{5, "bar opts"["B" -> 10]}
	];

	VerificationTest[
		foo[5, "A" -> 7, ForwardOptions["B" -> 10]],
		{5, "bar opts"["B" -> 10]}
	];

	VerificationTest[
		foo[5, "A" -> 7, "B" -> 10, ForwardOptions["B" -> 11]],
		{5, "bar opts"["B" -> 10, "B" -> 11]}
	];
]

(*====================================*)
(* AbsoluteOptions2                   *)
(*====================================*)

Module[{
	foo, bar
},
	(*-------*)
	(* Setup *)
	(*-------*)

	Options[foo] = {
		"A" -> 1,
		"B" -> 2
	};

	(*-------*)
	(* Tests *)
	(*-------*)

	VerificationTest[
		AbsoluteOptions2[foo[]],
		{"A" -> 1, "B" -> 2}
	];

	VerificationTest[
		AbsoluteOptions2[foo["A" -> 999]],
		{"A" -> 999, "B" -> 2}
	];
]

(*====================================*)
(* ConfirmFileType                    *)
(*====================================*)

(* PRECOMMIT: Read and sanity check each case *)

(*------------------------------------*)
(* Cases /> Path does not exist       *)
(*------------------------------------*)

With[{
	path = "DoesNotExist.txt"
},
	(*----------------------------------------------------------*)
	(* Cases /> Path does not exist /> Check 6 allowed typePatt *)
	(*----------------------------------------------------------*)

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None],
		path
	];

	VerificationTest[
		failure = Handle[_Failure] @ ConfirmFileType[path, File],
		Failure[DiagramError, <|
			"Path" -> "DoesNotExist.txt",
			"FileType" -> None,
			"ExpectedFileType" -> File,
			"MessageTemplate" -> "Path does not exist when a file was expected: `1`",
			"MessageParameters" -> {"DoesNotExist.txt"}
		|>]
	];

	VerificationTest[
		ToString[failure],
		"Path does not exist when a file was expected: DoesNotExist.txt"
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, Directory],
		Failure[DiagramError, <|
			"Path" -> "DoesNotExist.txt",
			"FileType" -> None,
			"ExpectedFileType" -> Directory,
			"MessageTemplate" -> "Path does not exist when a directory was expected: `1`",
			"MessageParameters" -> {"DoesNotExist.txt"}
		|>]
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None | File],
		path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None | Directory],
		path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, File | Directory],
		Failure[DiagramError, <|
			"Path" -> "DoesNotExist.txt",
			"FileType" -> None,
			"ExpectedFileType" -> File | Directory,
			"MessageTemplate" -> "Path does not exist when a file or directory was expected: `1`",
			"MessageParameters" -> {"DoesNotExist.txt"}
		|>]
	];

	(*----------------------------------------------------------*)
	(* Cases /> Path does not exist /> Check custom error       *)
	(*----------------------------------------------------------*)

	VerificationTest[
		failure = Handle[_Failure] @ ConfirmFileType[
			path,
			File,
			"Well that's unexpected"
		],
		Failure[DiagramError, <|
			"Path" -> "DoesNotExist.txt",
			"FileType" -> None,
			"ExpectedFileType" -> File,
			"MessageTemplate" -> "Well that's unexpected: Path does not exist when a file was expected: `1`",
			"MessageParameters" -> {"DoesNotExist.txt"}
		|>]
	];

	VerificationTest[
		ToString[failure],
		"Well that's unexpected: Path does not exist when a file was expected: DoesNotExist.txt"
	];

	VerificationTest[
		failure = Handle[_Failure] @ ConfirmFileType[
			path,
			File,
			"Well that's unexpected (``) with template"
		],
		Failure[DiagramError, <|
			"Path" -> "DoesNotExist.txt",
			"FileType" -> None,
			"ExpectedFileType" -> File,
			"MessageTemplate" -> "Well that's unexpected (``) with template: Path does not exist when a file was expected: `1`",
			"MessageParameters" -> {"DoesNotExist.txt"}
		|>]
	];

	VerificationTest[
		ToString[failure],
		"Well that's unexpected (DoesNotExist.txt) with template: Path does not exist when a file was expected: DoesNotExist.txt"
	];
];

(*------------------------------------*)
(* Cases /> Path is a File            *)
(*------------------------------------*)

With[{
	path = CreateFile[]
},
	(*----------------------------------------------------------*)
	(* Cases /> Path does not exist /> Check 6 allowed typePatt *)
	(*----------------------------------------------------------*)

	VerificationTest[
		failure = Handle[_Failure] @ ConfirmFileType[path, None],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> File,
			"ExpectedFileType" -> None,
			"MessageTemplate" -> "Found file when path was expected to not exist: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		ToString[failure],
		"Found file when path was expected to not exist: " <> path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, File],
		path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, Directory],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> File,
			"ExpectedFileType" -> Directory,
			"MessageTemplate" -> "Found file at path where directory was expected: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None | File],
		path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None | Directory],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> File,
			"ExpectedFileType" -> None | Directory,
			"MessageTemplate" -> "Found file when directory or non-existent path was expected: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, File | Directory],
		path
	];

	(*----------------------------------------------------------*)
	(* Cases /> Path does not exist /> Check custom error       *)
	(*----------------------------------------------------------*)

	VerificationTest[
		failure = Handle[_Failure] @ ConfirmFileType[
			path,
			Directory,
			"Well that's unexpected"
		],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> File,
			"ExpectedFileType" -> Directory,
			"MessageTemplate" -> "Well that's unexpected: Found file at path where directory was expected: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		ToString[failure],
		"Well that's unexpected: Found file at path where directory was expected: "
			<> path
	];
];

(*------------------------------------*)
(* Cases /> Path is a Directory       *)
(*------------------------------------*)

With[{
	path = CreateDirectory[]
},
	(*----------------------------------------------------------*)
	(* Cases /> Path does not exist /> Check 6 allowed typePatt *)
	(*----------------------------------------------------------*)

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> Directory,
			"ExpectedFileType" -> None,
			"MessageTemplate" -> "Found directory when path was expected to not exist: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		failure = Handle[_Failure] @ ConfirmFileType[path, File],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> Directory,
			"ExpectedFileType" -> File,
			"MessageTemplate" -> "Found directory at path where normal file was expected: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		ToString[failure],
		"Found directory at path where normal file was expected: " <> path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, Directory],
		path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None | File],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> Directory,
			"ExpectedFileType" -> None | File,
			"MessageTemplate" -> "Found directory when normal file or non-existent path was expected: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, None | Directory],
		path
	];

	VerificationTest[
		Handle[_Failure] @ ConfirmFileType[path, File | Directory],
		path
	];

	(*----------------------------------------------------------*)
	(* Cases /> Path does not exist /> Check custom error       *)
	(*----------------------------------------------------------*)

	VerificationTest[
		failure = Handle[_Failure] @ ConfirmFileType[
			path, File, "Well that's unexpected"
		],
		Failure[DiagramError, <|
			"Path" -> path,
			"FileType" -> Directory,
			"ExpectedFileType" -> File,
			"MessageTemplate" -> "Well that's unexpected: Found directory at path where normal file was expected: `1`",
			"MessageParameters" -> {path}
		|>]
	];

	VerificationTest[
		ToString[failure],
		"Well that's unexpected: Found directory at path where normal file was expected: "
			<> path
	];
];

(*------------------------------------*)
(* Cases /> File type pattern invalid *)
(*------------------------------------*)

VerificationTest[
	Handle[_Failure] @ ConfirmFileType["DoesNotExist.txt", "SomeInvalidPattern"],
	Failure[DiagramError, <|
		"MessageTemplate" -> "Invalid type pattern specified: `` (or unhandled file type: ``)",
		"MessageParameters" -> {
			InputForm["SomeInvalidPattern"],
			InputForm[None]
		}
	|>]
]