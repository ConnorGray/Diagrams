Package["Diagrams`ProgramDiagram`"]

PackageUse[Diagrams -> {
	DiagramError,
	ProgramDiagram,
	Library -> {$LibraryFunctions},
	Errors -> {
		SetFallthroughError, Raise, Handle, WrapRaised, ConfirmReplace,
		RaiseAssert, RaiseConfirm2
	}
}]

(*========================================================*)

SetFallthroughError[ProgramDiagram]

ProgramDiagram[] := $LibraryFunctions["mlir_thing"][]