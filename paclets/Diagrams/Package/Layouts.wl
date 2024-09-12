Package["Diagrams`Layouts`"]

PackageModule["Row"]
PackageModule["Rows"]
PackageModule["Graph"]
PackageModule["EqualWidthRows"]

PackageModule["Manual"]

PackageExport[{
	$DiagramLayouts,

	DoRowLayout,
	DoRowsLayout,
	DoGraphLayout,
	DoEqualWidthRowsLayout,

	(* Special case, where layout is manual. *)
	DoManualLayout
}]

$DiagramLayouts = {
	"Row",
	"Rows",
	"Graph",
	"EqualWidthRows"
}
