Package["Diagrams`Errors`"]

PackageExport[{
	RaiseError, RaiseAssert, RaiseConfirm, RaiseConfirmMatch,

	$ExitOnExceptionPreHandler,

	(*================================*)
	(* Reexported                     *)
	(*================================*)
	CreateErrorType,
	Raise,
	Handle,
	SetFallthroughError,
	ConfirmReplace,
	RaiseConfirmMatch,
	WrapRaised
}]

RaiseError::usage  = "RaiseError[formatStr, args___] throws a Failure object indicating an error encountered during the build process.";

$ExitOnExceptionPreHandler

PackageUse[Diagrams -> {Diagrams, DiagramError}]

Needs["Wolfram`ErrorTools`"]

CreateErrorType     = Symbol["Wolfram`ErrorTools`CreateErrorType"]
Raise               = Symbol["Wolfram`ErrorTools`Raise"]
Handle              = Symbol["Wolfram`ErrorTools`Handle"]
SetFallthroughError = Symbol["Wolfram`ErrorTools`SetFallthroughError"]
ConfirmReplace      = Symbol["Wolfram`ErrorTools`ConfirmReplace"]
RaiseAssert         = Symbol["Wolfram`ErrorTools`RaiseAssert"]
RaiseConfirm        = Symbol["Wolfram`ErrorTools`RaiseConfirm"]
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

(*========================================================*)

SetFallthroughError[RaiseError]

(* Generate a message and an exception. *)
RaiseError[formatStr_?StringQ, args___] := (
	Raise[DiagramError, formatStr, args]
)

(*========================================================*)
