Package["Diagrams`Utils`"]

PackageExport[{
	(* Layout Utilities *)
	RectangleWidth,
	RectangleHeight,
	RectangleSize,
	RectangleCenter,
	RectangleAttachmentPoint,
	RectangleBoundingBox,

	(* Developer UX *)
	OutputElementsQ,
	ConstructOutputElements,
	ForwardOptions,
	AbsoluteOptions2,
	ConfirmFileType,

	(* FrontEnd Operations *)
	NotebookImportCell,
	CopyToClipboard2,

	(* Strings *)
	ToCharacterCode2,
	GraphemeClusters,
	UnicodeData
}]

PackageUse[Diagrams -> {
	DiagramError,
	Errors -> {
		Raise, RaiseError, SetFallthroughError, Handle, WrapRaised,
		RaiseConfirmMatch, ConfirmReplace, RaiseAssert
	},
	Library -> {$LibraryFunctions}
}]

(*========================================================*)
(* Layout Utilities                                       *)
(*========================================================*)

RectangleWidth[arg_] := Replace[arg, {
	Rectangle[{xMin_?NumberQ, _}, {xMax_?NumberQ, _}] :> Abs[xMax - xMin],
	_ :> RaiseError["unable to get width of rectangle: ``", arg]
}]

RectangleHeight[arg_] := Replace[arg, {
	Rectangle[{_, yMin_?NumberQ}, {_, yMax_?NumberQ}] :> Abs[yMax - yMin],
	_ :> RaiseError["unable to get height of rectangle: ``", arg]
}]

RectangleSize[arg_] := {RectangleWidth[arg], RectangleHeight[arg]}

RectangleCenter[arg_] := Replace[arg, {
	Rectangle[
		{xMin_?NumberQ, yMin_?NumberQ},
		{xMax_?NumberQ, yMax_?NumberQ}
	] :> {xMin + RectangleWidth[arg] / 2, yMin + RectangleHeight[arg] / 2},
	_ :> RaiseError["unable to get height of rectangle: ``", arg]
}]

(*====================================*)

SetFallthroughError[RectangleAttachmentPoint]

RectangleAttachmentPoint[
	rect:Rectangle[
		{borderLeft_, borderBottom_},
		{borderRight_, borderTop_}
	],
	attachment_
] := Module[{
	point,
	x, y
},
	point = ConfirmReplace[attachment, {
		{
			side : (Left | Right | Top | Bottom),
			(* Linear interpolation factor. *)
			lerpFactor_?NumberQ
		} :> (
			x = ConfirmReplace[side, {
				Left :> borderLeft,
				Right :> borderRight,
				Top | Bottom :> borderLeft + lerpFactor * RectangleWidth[rect]
			}];

			y = ConfirmReplace[side, {
				Left | Right :> borderBottom + lerpFactor * RectangleHeight[rect],
				Top :> borderTop,
				Bottom :> borderBottom
			}];

			{x, y}
		),
		other_ :> Raise[
			DiagramError,
			"Unsupported attachment specification: ``",
			other
		]
	}];

	RaiseAssert[MatchQ[point, {_?NumberQ, _?NumberQ}]];

	point
]

(*====================================*)

SetFallthroughError[RectangleBoundingBox]

RectangleBoundingBox[rect_Rectangle] := rect;

RectangleBoundingBox[rects:{___Rectangle}] := Module[{
	minX, minY, maxX, maxY
},
	RaiseConfirmMatch[
		rects,
		{Rectangle[{_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ}]...}
	];

	minX = Min @ Part[rects, All, 1, 1];
	minY = Min @ Part[rects, All, 1, 2];
	maxX = Max @ Part[rects, All, 2, 1];
	maxY = Max @ Part[rects, All, 2, 2];

	Rectangle[{minX, minY}, {maxX, maxY}]
]

(*========================================================*)
(* Developer UX                                           *)
(*========================================================*)

SetFallthroughError[OutputElementsQ]

OutputElementsQ[expr_] :=
	MatchQ[expr, _?StringQ | {___?StringQ} | Automatic]

(*====================================*)

SetFallthroughError[ConstructOutputElements]

ConstructOutputElements[
	outputElems: _?OutputElementsQ,
	default: _?StringQ,
	(* A list of rules or a function. *)
	getOutputElement0_
] := Module[{
	getOutputElement = getOutputElement0
},
	If[ListQ[getOutputElement0],
		getOutputElement = ({elem} |-> ConfirmReplace[
			elem,
			Append[
				getOutputElement0,
				other_ :> Raise[
					DiagramError,
					"Unrecognized output element requested: ``",
					InputForm[other]
				]
			]
		]);
	];

	(*--------------------------------*)

	ConfirmReplace[outputElems, {
		Automatic :> getOutputElement[default],
		element_?StringQ :> getOutputElement[element],
		elements:{___?StringQ} :> Map[getOutputElement, elements],
		other_ :> Raise[
			DiagramError,
			"Unrecognized output elements specification: ``",
			InputForm[other]
		]
	}]
]

(*====================================*)

(*
	ForwardOptions can be used to pass down a sequence of options, passing only
	the subset of options accepted by the callee, discarding those options that
	were only relevant in the context of the caller function.

	ForwardOptions must be the last argument to a function to work.
*)
ClearAll[ForwardOptions];

ForwardOptions /: head_Symbol[
	args___,
	ForwardOptions[opts___?OptionQ]
] := (
	head[
		args,
		Sequence @@ Flatten@FilterRules[{opts}, Options[head]]
	]
)

(*====================================*)

SetFallthroughError[AbsoluteOptions2]

AbsoluteOptions2[expr:(head_Symbol[___])] := Module[{
	defaultOpts = Options[head]
},
	DeleteDuplicatesBy[
		(* {___Rule} *)
		Join[
			Options[expr],
			Options[head]
		],
		(* Delete by duplicate Rule LHS (i.e. the option name). *)
		First
	]
]

(*====================================*)

(* TODO: Support this option. *)
(* Options[ConfirmFileType] = {
	(* Whether to include the file path in any returned error messages.
		Disabling this shortens the messages, and may be a good choice
		when choosing to display the "Path" field in a different way. *)
	"IncludeFilePath" -> True
} *)

SetFallthroughError[ConfirmFileType]

(*
	Confirm that the specified file path conforms to the specified file type.

	This is a versatile tool for generating precise error messages indicating
	e.g. when a file was expected to exist but did not, when a path was either
	expected to be a normal file or not exist but in fact is a directory, and
	more.

	The second argument must be one of the following patterns verbatim:

	* None
	* File
	* Directory
	* None | File
	* None | Directory
	* File | Directory
*)
ConfirmFileType[
	(path: _?StringQ) | File[path: _?StringQ],
	typePatt: _,
	errorPrefix: _?StringQ : None
] := Module[{
	fileType = FileType[path],
	template,
	metadata
},
	If[MatchQ[fileType, typePatt],
		Return[path, Module];
	];

	(* The actual file type did not match the expected file spec. *)
	template = ConfirmReplace[{fileType, typePatt}, {
		{None, File} :>
			"Path does not exist when a file was expected",
		{None, Directory} :>
			"Path does not exist when a directory was expected",
		{None, Verbatim[File | Directory]} :>
			"Path does not exist when a file or directory was expected",

		{File, None} :>
			(* "File unexpectedly exists at path: ``", *)
			"Found file when path was expected to not exist",
		{File, Directory} :>
			"Found file at path where directory was expected",
		{File, Verbatim[None | Directory]} :>
			"Found file when directory or non-existent path was expected",

		{Directory, None} :>
			"Found directory when path was expected to not exist",
		{Directory, File} :>
			"Found directory at path where normal file was expected",
		{Directory, Verbatim[None | File]} :>
			"Found directory when normal file or non-existent path was expected",

		(* TODO: Should be a LogicError *)
		other_ :> Raise[
			DiagramError,
			"Invalid type pattern specified: `` (or unhandled file type: ``)",
			InputForm[typePatt],
			InputForm[fileType]
		]
	}];

	(* Note: Use `1` explicitly so that a custom errorPrefix can use `` without
		affecting the template parameter this resolves to. *)
	template = template <> ": `1`";

	If[StringQ[errorPrefix],
		template = errorPrefix <> ": " <> template;
	];

	Raise[
		DiagramError,
		<|
			"Path" -> path,
			"FileType" -> fileType,
			"ExpectedFileType" -> typePatt
		|>,
		template,
		path
	]
]

(*========================================================*)
(* FrontEnd Operations                                    *)
(*========================================================*)

SetFallthroughError[NotebookImportCell]

NotebookImportCell[cell_CellObject] :=
	NotebookImportCell[NotebookRead[cell]]

NotebookImportCell[cell_CellObject, form_?StringQ] :=
	NotebookImportCell[NotebookRead[cell], form]

NotebookImportCell[cell_Cell, form_?StringQ] := Module[{
	result
},
	result = RaiseConfirmMatch[
		First[
			UsingFrontEnd @ NotebookImport[
				Notebook[{cell}],
				_ -> form
			],
			Missing["NotAvailable"]
		],
		_?StringQ | _?MissingQ
	];

	result
]

(*====================================*)

SetFallthroughError[CopyToClipboard2]

(*
	Fix CopyToClipboard["1—3"] (where thats an emdash, \[LongDash]) resulting
	in pasting "1\[LongDash]3" into a different text editing program.

	Similar issues arise when using things like Unicode box drawing characters.
*)
CopyToClipboard2[text: _?StringQ] := Module[{
	file
},
	(* Save UTF-8 encoded bytes to a text file. *)
	file = Export[CreateFile[] <> ".txt", text];

	(* Use macOS builtin utility pbcopy to do the copy to the clipboard. *)
	RunProcess[
		{
			"zsh",
			"-c",
			"cat " <> file <> " | pbcopy"
		},
		ProcessEnvironment -> Prepend[
			GetEnvironment[],
			(* Ensure `pbcopy` decodes the piped incoming bytes as UTF-8. *)
			"LANG" -> "en_US.UTF-8"
		]
	];

	text
]

(*========================================================*)
(* Strings                                                *)
(*========================================================*)

SetFallthroughError[ToCharacterCode2]

(*
	This function is a workaround for the fact that WL doesn't provide support
	for UTF-16 encoding in ToCharacterCode.
*)
ToCharacterCode2[
	text_?StringQ,
	encoding_?StringQ
] := Module[{
	libFunc
},
	If[MemberQ[$CharacterEncodings, encoding] || encoding == "Unicode",
		Return @ ToCharacterCode[text, encoding];
	];

	libFunc = $LibraryFunctions["encode_string"];

	RaiseConfirmMatch[libFunc, _LibraryFunction];

	Replace[libFunc[text, encoding], {
		data_?NumericArrayQ :> Normal[data]
	}]
]

(*====================================*)

SetFallthroughError[GraphemeClusters]

GraphemeClusters[text_?StringQ] := Module[{
	libFunc = $LibraryFunctions["grapheme_clusters"]
},
	libFunc[text]
]

(*====================================*)

SetFallthroughError[UnicodeData]

UnicodeData[] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error processing Unicode data"
] @ Module[{
	(* FIXME: Fix broken "\n" string literal in package format. *)
	newline = FromCharacterCode[10],
	data,
},
	(*--------------------------------------------------------*)
	(* Import from the file and divide it into the raw fields *)
	(*--------------------------------------------------------*)

	data = $LibraryFunctions["get_unicode_data"][];

	RaiseConfirmMatch[data, _?StringQ];

	data = StringSplit[data, newline];

	RaiseConfirmMatch[data, {Repeated[_?StringQ, {5, 100000}]}];

	data = Map[
		line |-> StringSplit[line, ";"],
		data
	];

	(*-----------------*)

	data
]

(*------------------------------------*)

UnicodeData["GeneralCategoryPropertyValues"] := Module[{},
	RaiseConfirmMatch[
		$LibraryFunctions["unicode_properties"][],
		{{_?StringQ, _?StringQ, _?StringQ}..}
	]
]

(*------------------------------------*)

UnicodeData[
	"MappedCharacterSetCodepoints",
	characterSet : _?StringQ | All
] := Module[{},
	RaiseConfirmMatch[
		$LibraryFunctions["get_mapped_character_set_codepoints"][characterSet],
		Alternatives[
			{ (_?IntegerQ -> _?IntegerQ) .. },
			{
				Repeated[
					_?StringQ -> { (_?IntegerQ -> _?IntegerQ) .. }
				]
			}
		]
	]
]
