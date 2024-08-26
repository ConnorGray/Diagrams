BeginPackage["ConnorGray`Diagrams`Loader`"]

Needs["Packages`"]

packageObj = PackageLoad[
	PackageSpecifier[
		PacletSpecifier["Diagrams", "0.0.1"],
		"Diagrams"
	]
]

EndPackage[]


$ContextPath = DeleteDuplicates @ Join[
	{
		packageObj["PackageContext"],
		"Diagrams`Concepts`",
		"Diagrams`Layout`",
		"Diagrams`Utils`",
		"Diagrams`Kinds`BlockStack`",
		"Diagrams`Kinds`BinaryLayout`"
	},
	$ContextPath
];

WithCleanup[
	Unprotect[$Packages]
	,
	AppendTo[$Packages, "Diagrams`"]
	,
	Protect[$Packages]
];
