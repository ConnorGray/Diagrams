BeginPackage["DiagramMaker`Render`"]

DefaultTheme

SizedText::usage = "SizedText[s, rect]"

Begin["`Private`"]

Needs["DiagramMaker`"]
Needs["DiagramMaker`Errors`"]
Needs["DiagramMaker`Layout`"]

rgbColor[r_Integer, g_Integer, b_Integer] := RGBColor @@ ({r, g, b} / 255)

(* https://coolors.co/palette/072ac8-1e96fc-a2d6f9-fcf300-ffc600 *)
$DefaultTheme = <|
	"BoxBackground" -> rgbColor[162, 214, 249],
	"BoxBorder" ->  rgbColor[30, 150, 252],
	"BoxTextColor" -> rgbColor[7, 42, 200],
	"ArrowStroke" -> rgbColor[255, 198, 0]
	(* "ArrowStroke" -> rgbColor[252, 243, 0] *)
|>

(*--------------------------------------------------------*)

RenderPlacedDiagramToGraphics[
	PlacedDiagram[
		boxes:<| (_?StringQ -> _PlacedBox) ... |>,
		arrows:{___PlacedArrow}
	],
	theme0 : (_?AssociationQ | Automatic) : Automatic
] := Module[{
	graphics = {},
	theme = Replace[theme0, Automatic :> $DefaultTheme]
},
	(*------------*)
	(* Draw boxes *)
	(*------------*)

	Scan[
		Replace[{
			PlacedBox[
				DiaBox[id_?StringQ],
				textRect_Rectangle,
				borderRect_Rectangle
			] :> (
				(* Draw the background and border first. *)
				AppendTo[graphics, {
					(* FaceForm[Lookup[theme, "BoxBackground", RaiseError["FIXME"]]], *)
					Lookup[theme, "BoxBackground", RaiseError["FIXME"]],
					EdgeForm[Lookup[theme, "BoxBorder", RaiseError["FIXME"]]],
					AbsoluteThickness[4.0],
					borderRect
				}];

				(* Finally draw the text. *)
				AppendTo[graphics, {
					Lookup[theme, "BoxTextColor", RaiseError["FIXME"]],
					SizedText[id, textRect]
				}];
			),
			other_ :> RaiseError["unexpected diagram placed box structure: ``", other]
		}],
		boxes
	];

	(*-------------*)
	(* Draw arrows *)
	(*-------------*)

	Scan[
		Replace[{
			PlacedArrow[
				DiaArrow[___],
				startPoint:{_?NumberQ, _?NumberQ},
				endPoint:{_?NumberQ, _?NumberQ}
			] :> Module[{},
				AppendTo[graphics, {
					Lookup[theme, "ArrowStroke", RaiseError["FIXME"]],
					Arrow[{startPoint, endPoint}]
				}];
			],
			other_ :> RaiseError["unexpected diagram placed arrow structure: ``", other]
		}],
		arrows
	];

	graphics
]

End[]
EndPackage[]