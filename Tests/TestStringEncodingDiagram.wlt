Needs["Wolfram`ErrorTools`V1`"]

(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

Module[{
	diagram = StringEncodingDiagram[
		"foo",
		{"Bytes", "Codepoints", "Characters"},
		ChartLegends -> Automatic
	]
},
	VerificationTest[
		diagram,
		Diagram[_?AssociationQ, ___?OptionQ],
		SameTest -> MatchQ
	];

	VerificationTest[
		Values @ Options[diagram, ChartLegends],
		{Placed[
			SwatchLegend[
				{Blue, Darker[Blue], Brown},
				{"Characters", "Codepoints", "Bytes"},
				LegendMarkerSize -> 20,
				LegendMargins -> 0
			],
			Right
		]}
	];
]
