BlockDiagram[
	DiaBox /@ {"Animal", "Dog", "Cat", "Charlie", "Tux", "Tux Jr."},
	{
		DiaArrow["Dog" -> "Animal",  "dogs"],
		DiaArrow["Cat" -> "Animal",  "cats"],
		DiaArrow["Charlie" -> "Cat", "cat1"],
		DiaArrow["Tux" -> "Cat",     "cat2"],
		DiaArrow["Tux Jr." -> "Cat", "cat3"]
	},
	DiagramLayout -> "EqualWidthRows"
]