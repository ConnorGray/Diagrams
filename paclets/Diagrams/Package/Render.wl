Package["Diagrams`Render`"]

PackageExport[{
	$DefaultTheme, DefaultTheme, SizedText, DiagramElementDirectives
}]

PackageUse[Diagrams -> {
	PlacedDiagram, RenderPlacedDiagramToGraphics, DiaBox, DiaArrow,
	DiagramElementContent,
	Layout -> {$DebugDiagramLayout, PlacedBox, PlacedArrow, Utils -> Bounded},
	Utils -> {RectangleSize},
	Errors -> {RaiseError, RaiseConfirm, RaiseConfirmMatch}
}]

SizedText::usage = "SizedText[s, rect]"


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
		Optional[_?StringQ, None],
		boxes:<| (_?StringQ -> _PlacedBox) ... |>,
		arrows:{___PlacedArrow}
	],
	theme0 : (_?AssociationQ | Automatic) : Automatic
] := Module[{
	graphics = {},
	theme = Replace[theme0, {
		Automatic :> $DefaultTheme,
		(* If custom theme rules were specified, use defaults from $DefaultTheme.
		   Rules in `theme0` will override those in $DefaultTheme. *)
		_?AssociationQ :> Join[$DefaultTheme, theme0]
	}]
},
	(*------------*)
	(* Draw boxes *)
	(*------------*)

	Scan[
		Replace[{
			PlacedBox[
				DiaBox[
					id_?StringQ,
					Optional[content0:Except[_?OptionQ], None],
					opts___?OptionQ
				],
				placedContent_?ListQ,
				contentRect_Rectangle,
				(* Border rect *)
				Rectangle[min_, max_]
			] :> Module[{
				contentGraphics,
				background = Lookup[
					{opts},
					Background,
					RaiseConfirm @ Lookup[theme, "BoxBackground"]
				]
			},
				(* Draw the background and border first. *)
				AppendTo[graphics, {
					(* FaceForm[RaiseConfirm @ Lookup[theme, "BoxBackground"]], *)
					background,
					EdgeForm[{
						RaiseConfirm @ Lookup[theme, "BoxBorder"],
						AbsoluteThickness[4.0]
					}],
					Rectangle[min, max, RoundingRadius -> 3]
				}];

				(* FIXME: Re-implement usage of the "BoxTextColor" theme property. *)
				contentGraphics = placedContent /. Bounded[c_?ListQ, _Rectangle] :> c;

				RaiseConfirmMatch[contentGraphics, _?ListQ];

				AppendTo[graphics, contentGraphics];

				If[TrueQ[$DebugDiagramLayout],
					AppendTo[graphics, {
						FaceForm[Transparent],
						EdgeForm[Directive[Dashed, Red]],
						contentRect
					}];
				];
			],
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
				arrow:DiaArrow[___],
				startPoint:{_?NumberQ, _?NumberQ},
				endPoint:{_?NumberQ, _?NumberQ}
			] :> Module[{},
				AppendTo[graphics, {
					RaiseConfirm @ Lookup[theme, "ArrowStroke"],
					AbsoluteThickness[4.0],
					Arrowheads[0.06],
					Replace[
						DiagramElementDirectives[arrow],
						(* If the directives are a list, splice them in so that
						   they can affect the Arrow[..] formatting. *)
						dirs_?ListQ :> Splice[dirs]
					],
					(* FIXME: Include arrow text in the diagram directly, instead
						of hiding it behind a tooltip. Doing this properly will
						require taking arrow labels into account during layout. *)
					Tooltip[
						Arrow[{startPoint, endPoint}],
						DiagramElementContent[arrow]
					]
				}];
			],
			other_ :> RaiseError["unexpected diagram placed arrow structure: ``", other]
		}],
		arrows
	];

	graphics
]

(*====================================*)
(* Helper functions                   *)
(*====================================*)

DiagramElementDirectives[DiaArrow[_, _, directive_]] := directive

DiagramElementDirectives[_DiaArrow] := {}

DiagramElementDirectives[args___] :=
	RaiseError["unexpected arguments to DiagramElementDirectives: ``", InputForm[{args}]]
