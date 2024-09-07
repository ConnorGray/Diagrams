Package["Diagrams`Errors`"]

PackageExport[{
	RaiseError, RaiseAssert, RaiseConfirm, RaiseConfirmMatch,

	$RaiseErrorTag,
	$ExitOnExceptionPreHandler,

	(*================================*)
	(* Reexported                     *)
	(*================================*)
	CreateErrorType,
	Raise,
	Handle,
	SetFallthroughError,
	ConfirmReplace,
	RaiseConfirm2,
	RaiseConfirmMatch,
	RaiseAssert2,
	WrapRaised
}]

RaiseError::usage  = "RaiseError[formatStr, args___] throws a Failure object indicating an error encountered during the build process.";
RaiseAssert::usage = "RaiseAssert[cond, formatStr, args___] throws a Failure object indicating a failed assertion encountered during the build process.";
RaiseConfirm

$RaiseErrorTag

$ExitOnExceptionPreHandler

PackageUse[Diagrams -> {Diagrams, DiagramError}]

Needs["Wolfram`ErrorTools`"]

CreateErrorType     = Symbol["Wolfram`ErrorTools`CreateErrorType"]
Raise               = Symbol["Wolfram`ErrorTools`Raise"]
Handle              = Symbol["Wolfram`ErrorTools`Handle"]
SetFallthroughError = Symbol["Wolfram`ErrorTools`SetFallthroughError"]
ConfirmReplace      = Symbol["Wolfram`ErrorTools`ConfirmReplace"]
RaiseAssert2        = Symbol["Wolfram`ErrorTools`RaiseAssert"]
RaiseConfirm2       = Symbol["Wolfram`ErrorTools`RaiseConfirm"]
RaiseConfirmMatch   = Symbol["Wolfram`ErrorTools`RaiseConfirmMatch"]
WrapRaised          = Symbol["Wolfram`ErrorTools`WrapRaised"]

CreateErrorType[DiagramError, {}]

(**********************************************************)

$ExitOnExceptionPreHandler = Function[
	expr,
	Module[{result},
		result = Catch[expr, _, "UncaughtException"];
		If[Head[result] === "UncaughtException",
			Print["Terminating program due to uncaught exception."];
			Exit[];
		]
	],
	HoldFirst
];

(**********************************************************)

$RaiseErrorTag

(* Generate a message and an exception. *)
RaiseError[formatStr_?StringQ, args___] := (
	Message[
		Diagrams::error,
		(* Note: Use '@@' to avoid behavior described in bug #240412. *)
		ToString[StringForm @@ {formatStr, args}]
	];

	Throw[
		Failure["DiagramsError", <|
			"MessageTemplate" -> formatStr,
			"MessageParameters" -> {args}
		|>],
		$RaiseErrorTag
	]
)

RaiseError[args___] := Throw[
	Failure["DiagramsError", <|
		"MessageTemplate" -> ToString[StringForm[
			"Unknown error occurred: ``",
			StringJoin[Map[ToString, {args}]]
		]]
	|>],
	$RaiseErrorTag
]

(**********************************************************)

Attributes[RaiseConfirm] = {HoldFirst}

RaiseConfirm[expr_] := Module[{result},
	result = expr;

	If[FailureQ[result] || MissingQ[result],
		RaiseError["RaiseConfirm error evaluating ``: ``", HoldForm[expr], result];
	];

	result
];

(**********************************************************)

Attributes[RaiseAssert] = {HoldFirst}

RaiseAssert[
	cond_,
	formatStr : _?StringQ,
	args___
] := If[!TrueQ[cond],
	Message[
		Diagrams::assertfail,
		(* Note: Use '@@' to avoid behavior described in bug #240412. *)
		ToString[StringForm @@ {formatStr, args}]
	];

	Throw[
		Failure["DiagramsError", <|
			"MessageTemplate" -> "RaiseAssert[..] failed: " <> formatStr,
			"MessageParameters" -> {args}
		|>],
		$RaiseErrorTag
	]
]

RaiseAssert[cond_] :=
	RaiseAssert[
		cond,
		"RaiseAssert[..] of expression failed: ``",
		(* HoldForm so that the error shows the unevaluated asserted expression. *)
		HoldForm @ InputForm @ cond
	]

RaiseAssert[args___] := Throw[
	Failure["DiagramsError", <|
		"MessageTemplate" -> ToString[StringForm[
			"Malformed RaiseAssert[..] call: ``",
			StringJoin[Map[ToString, {args}]]
		]]
	|>],
	$RaiseErrorTag
]

(*========================================================*)
