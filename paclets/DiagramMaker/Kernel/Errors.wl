BeginPackage["DiagramMaker`Errors`"]

RaiseError::usage  = "RaiseError[formatStr, args___] throws a Failure object indicating an error encountered during the build process.";
RaiseAssert::usage = "RaiseAssert[cond, formatStr, args___] throws a Failure object indicating a failed assertion encountered during the build process.";
RaiseConfirm
RaiseConfirmMatch::usage = "RaiseConfirmMatch[expr, form, formatStr, args___] returns expr if form matches expr, and raises an error otherwise."

$RaiseErrorTag

$ExitOnExceptionPreHandler

Begin["`Private`"]

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
		DiagramMaker`DiagramMaker::error,
		(* Note: Use '@@' to avoid behavior described in bug #240412. *)
		ToString[StringForm @@ {formatStr, args}]
	];

	Throw[
		Failure["DiagramMakerError", <|
			"MessageTemplate" -> formatStr,
			"MessageParameters" -> {args}
		|>],
		$RaiseErrorTag
	]
)

RaiseError[args___] := Throw[
	Failure["DiagramMakerError", <|
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
		DiagramMaker`DiagramMaker::assertfail,
		(* Note: Use '@@' to avoid behavior described in bug #240412. *)
		ToString[StringForm @@ {formatStr, args}]
	];

	Throw[
		Failure["DiagramMakerError", <|
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
	Failure["DiagramMakerError", <|
		"MessageTemplate" -> ToString[StringForm[
			"Malformed RaiseAssert[..] call: ``",
			StringJoin[Map[ToString, {args}]]
		]]
	|>],
	$RaiseErrorTag
]

(**********************************************************)

Attributes[RaiseConfirmMatch] = {HoldFirst}

RaiseConfirmMatch[
	expr0_,
	patt_,
	formatStr : _?StringQ : Automatic,
	args___
] := Module[{
	expr = expr0
},
	If[MatchQ[expr, patt],
		expr
		,
		Replace[formatStr, {
			Automatic :> RaiseError[
				"RaiseConfirmMatch: pattern '``' does not match result: ``",
				patt,
				expr
			],
			_?StringQ :> RaiseError[formatStr, args],
			_ :> RaiseError["unreachable RaiseConfirmMatch condition"]
		}];
	]
]


End[]

EndPackage[]