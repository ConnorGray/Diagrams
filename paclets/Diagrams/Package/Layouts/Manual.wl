Package["Diagrams`Layouts`Manual`"]

PackageUse[Diagrams -> {
	Diagram, DiaBox, DiaArrow,
	DiagramElementContent, DiagramElementId,
	PlacedDiagram,
	DiagramError,
	Layouts -> DoManualLayout,
	Layout -> {
		$Margin,
		PlacedBox,
		LayoutUtils -> {
			MakePlacedBox, PlaceArrowsBasedOnBoxes
		}
	},
	Errors -> {Raise, RaiseAssert, SetFallthroughError, ConfirmReplace}
}]

(*========================================================*)

SetFallthroughError[DoManualLayout]

DoManualLayout[
	boxes0: _List,
	arrows0: _List
] := Module[{
	boxes = boxes0,
	arrows = arrows0,
	singleBox,
	boxID,
	placedBox,
	placedArrows,
},
	singleBox = ConfirmReplace[boxes, {
		{single: _DiaBox} :> single,
		other_ :> Raise[
			DiagramError,
			"Unable to do manual layout of diagram with more than one root element."
		]
	}];

	boxID = DiagramElementId[singleBox];

	placedBox = MakePlacedBox[singleBox];

	placedArrows = PlaceArrowsBasedOnBoxes[
		arrows,
		<| boxID -> placedBox |>
	];

	PlacedDiagram[
		<| boxID -> placedBox |>,
		placedArrows
	]
]