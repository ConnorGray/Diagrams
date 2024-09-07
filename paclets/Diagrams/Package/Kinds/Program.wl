Package["Diagrams`Kinds`Program`"]

PackageUse[Diagrams -> {
	DiagramError,
	ProgramDiagram,
	Library -> {$LibraryFunctions},
	Errors -> {
		SetFallthroughError, Raise, Handle, WrapRaised, ConfirmReplace,
		RaiseAssert
	}
}]

(*========================================================*)

SetFallthroughError[ProgramDiagram]

ProgramDiagram[] := $LibraryFunctions["mlir_thing"][]