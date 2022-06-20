BeginPackage["DiagramMaker`Layouts`"]

$DiagramLayouts = {
	"Row",
	"Rows",
	"Graph",
	"EqualWidthRows"
}

DoRowLayout
DoRowsLayout
DoGraphLayout
DoEqualWidthRowsLayout

Begin["`Private`"]

Needs["DiagramMaker`Layouts`Row`"]
Needs["DiagramMaker`Layouts`Rows`"]
Needs["DiagramMaker`Layouts`Graph`"]
Needs["DiagramMaker`Layouts`EqualWidthRows`"]


End[]
EndPackage[]