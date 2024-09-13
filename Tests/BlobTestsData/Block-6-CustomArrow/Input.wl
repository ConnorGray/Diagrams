DiaMyArrow /: MakeDiagramPrimitives[
	DiaMyArrow[lhs_ -> rhs_, id_]
] := DiaArrow[lhs -> rhs, id, {Red, Dashed}]

customArrow = BlockDiagram[
	{DiaBox["foo"], DiaBox["bar"]},
	{DiaMyArrow["foo" -> "bar", "a"]},
	DiagramLayout -> "Row"
]