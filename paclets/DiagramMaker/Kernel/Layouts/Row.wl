BeginPackage["DiagramMaker`Layouts`Row`"]

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Utils`"]
Needs["DiagramMaker`Layout`"]
Needs["DiagramMaker`Layout`Utils`"]
Needs["DiagramMaker`Layouts`"]

(* Layout all diagram boxes in a single row. *)
DoRowLayout[
	Diagram[
		boxes:{___DiaBox},
		arrows:{___DiaArrow},
		___?OptionQ
	]
] := Module[{
	xOffset,
	placedBoxes,
	placedArrows
},
	xOffset = 0.0;
	placedBoxes = <||>;
	placedArrows = {};

	(*-------------*)
	(* Place boxes *)
	(*-------------*)

	Scan[
		Replace[{
			box_DiaBox :> Module[{
				id = DiaElementId[box],
				text = DiaElementText[box],
				textRect, borderRect,
				placedBox
			},
				{textRect, borderRect} = makeBoxRectangles[text, {xOffset, 0}];

				placedBox = PlacedBox[box, textRect, borderRect];

				xOffset += RectangleWidth[placedBox[[2]]] + $margin;

				AssociateTo[placedBoxes, id -> placedBox];
			],
			other_ :> RaiseError["unexpected diagram box structure: ``", other]
		}],
		boxes
	];

	(*--------------*)
	(* Place arrows *)
	(*--------------*)

	placedArrows = placeArrowsBasedOnBoxes[arrows, placedBoxes];

	RaiseAssert[Length[placedArrows] === Length[arrows]];
	RaiseAssert[Length[placedBoxes] === Length[boxes]];

	PlacedDiagram[
		placedBoxes,
		placedArrows
	]
]


End[]
EndPackage[]