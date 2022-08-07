Package["Diagrams`Layouts`"]

PackageModule["Row"]
PackageModule["Rows"]
PackageModule["Graph"]
PackageModule["EqualWidthRows"]

PackageExport[{
	$DiagramLayouts,

	DoRowLayout,
	DoRowsLayout,
	DoGraphLayout,
	DoEqualWidthRowsLayout
}]

$DiagramLayouts = {
	"Row",
	"Rows",
	"Graph",
	"EqualWidthRows"
}
