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

Module[{
	diagram = StringEncodingDiagram["\[Alpha]", {
		"Bits", "Bytes",
		"Codepoints", "Characters",
		"String"
	}]
},
	VerificationTest[
		diagram,
		Diagram[_?AssociationQ, ___?OptionQ],
		SameTest -> MatchQ
	];

	VerificationTest[
		diagram[[1, "Regions"]],
		<|
			DiaID["Bit.1"] -> Rectangle[{0, 0}, {1/8, 1/8}],
			DiaID["Bit.2"] -> Rectangle[{1/8, 0}, {1/4, 1/8}],
			DiaID["Bit.3"] -> Rectangle[{1/4, 0}, {3/8, 1/8}],
			DiaID["Bit.4"] -> Rectangle[{3/8, 0}, {1/2, 1/8}],
			DiaID["Bit.5"] -> Rectangle[{1/2, 0}, {5/8, 1/8}],
			DiaID["Bit.6"] -> Rectangle[{5/8, 0}, {3/4, 1/8}],
			DiaID["Bit.7"] -> Rectangle[{3/4, 0}, {7/8, 1/8}],
			DiaID["Bit.8"] -> Rectangle[{7/8, 0}, {1, 1/8}],
			DiaID["Bit.9"] -> Rectangle[{1, 0}, {9/8, 1/8}],
			DiaID["Bit.10"] -> Rectangle[{9/8, 0}, {5/4, 1/8}],
			DiaID["Bit.11"] -> Rectangle[{5/4, 0}, {11/8, 1/8}],
			DiaID["Bit.12"] -> Rectangle[{11/8, 0}, {3/2, 1/8}],
			DiaID["Bit.13"] -> Rectangle[{3/2, 0}, {13/8, 1/8}],
			DiaID["Bit.14"] -> Rectangle[{13/8, 0}, {7/4, 1/8}],
			DiaID["Bit.15"] -> Rectangle[{7/4, 0}, {15/8, 1/8}],
			DiaID["Bit.16"] -> Rectangle[{15/8, 0}, {2, 1/8}],
			DiaID["Byte.1"] -> Rectangle[{0, 1/8}, {1, 9/8}],
			DiaID["Byte.2"] -> Rectangle[{1, 1/8}, {2, 9/8}],
			DiaID["Codepoint.1"] -> Rectangle[{0, 9/8}, {2, 17/8}],
			DiaID["Character.1"] -> Rectangle[{0, 17/8}, {2, 25/8}],
			DiaID["String"] -> Rectangle[{0, 25/8}, {2, 33/8}]
		|>
	];
]
