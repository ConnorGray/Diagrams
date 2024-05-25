Package["Diagrams`Library`"]

PackageExport[{
	$LibraryFunctions
}]

$LibraryFunctions::usage = "Association of library functions provided by the Diagrams compiled library"

(*========================================================*)

$LibraryFunctions = LibraryFunctionLoad[
	"libdiagram_maker_wll",
	"load_diagram_maker_functions",
	LinkObject,
	LinkObject
][];

Assert[MatchQ[$LibraryFunctions, <| (_?StringQ -> _)... |>]];



