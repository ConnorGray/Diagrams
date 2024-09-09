Package["Diagrams`Render`"]

PackageExport[{
	$DefaultTheme, DefaultTheme, SizedText, DiagramElementDirectives
}]

PackageUse[Diagrams -> {
	DiagramError,
	PlacedDiagram, RenderPlacedDiagramToGraphics, DiaBox, DiaArrow,
	DiagramElementContent,
	Layout -> {
		$DebugDiagramLayout, PlacedBox, PlacedArrow,
		LayoutUtils -> Bounded
	},
	Utils -> {RectangleSize, RectangleCenter},
	Errors -> {
		Raise, RaiseError, RaiseAssert, RaiseConfirm, RaiseConfirmMatch,
		SetFallthroughError, ConfirmReplace
	}
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

$theme := Raise[DiagramError, "illegal unscoped access of $theme"]

(*--------------------------------------------------------*)

RenderPlacedDiagramToGraphics[
	PlacedDiagram[
		Optional[_?StringQ, None],
		boxes:<| (_?StringQ -> _PlacedBox) ... |>,
		arrows:{___PlacedArrow}
	],
	theme0 : (_?AssociationQ | Automatic) : Automatic
] := Block[{
	$theme = Replace[theme0, {
		Automatic :> $DefaultTheme,
		(* If custom theme rules were specified, use defaults from $DefaultTheme.
		   Rules in `theme0` will override those in $DefaultTheme. *)
		_?AssociationQ :> Join[$DefaultTheme, theme0]
	}]
}, Module[{
	graphics
},
	(*------------*)
	(* Draw boxes *)
	(*------------*)

	graphics = Map[
		placedContentToGraphics,
		Values[boxes]
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
					RaiseConfirm @ Lookup[$theme, "ArrowStroke"],
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

	(*--------------------------------*)
	(* Resolve SizedText              *)
	(*--------------------------------*)

	graphics = ReplaceAll[
		graphics,
		sizedText: _SizedText :> makeSizedTextInset[sizedText]
	];

	graphics
]]

(*------------------------------------*)

SetFallthroughError[placedContentToGraphics]

placedContentToGraphics[placedContent0: _] := ConfirmReplace[placedContent0, {
	PlacedBox[
		DiaBox[
			_?StringQ,
			Optional[unused: Except[_?OptionQ], None],
			opts: ___?OptionQ
		],
		placedContent1: _?ListQ,
		contentRect: _Rectangle,
		(* Border rect *)
		Rectangle[min: _, max: _]
	] :> Module[{
		graphics = {},
		contentGraphics,
		background = Lookup[
			{opts},
			Background,
			RaiseConfirm @ Lookup[$theme, "BoxBackground"]
		]
	},
		(* Draw the background and border first. *)
		AppendTo[graphics, {
			(* FaceForm[RaiseConfirm @ Lookup[theme, "BoxBackground"]], *)
			background,
			EdgeForm[{
				RaiseConfirm @ Lookup[$theme, "BoxBorder"],
				AbsoluteThickness[4.0]
			}],
			Rectangle[min, max, RoundingRadius -> 3]
		}];

		(* FIXME: Re-implement usage of the "BoxTextColor" theme property. *)
		contentGraphics = placedContentToGraphics[placedContent1];

		RaiseConfirmMatch[contentGraphics, _?ListQ];

		AppendTo[graphics, contentGraphics];

		If[TrueQ[$DebugDiagramLayout],
			AppendTo[graphics, {
				FaceForm[Transparent],
				EdgeForm[Directive[Dashed, Red]],
				contentRect
			}];
		];

		graphics
	],

	list: _List :> (
		Map[placedContentToGraphics, list]
	),

	(* This is lazily converted to more fundamental text primitives. *)
	text: SizedText[_?StringQ, _Rectangle] :> text,
	Bounded[{g: _Graphics}, rect_] :> {
		Inset[
			g,
			RectangleCenter[rect],
			Automatic,
			RectangleSize[rect]
		]
	},
	Bounded[innerContent: _?ListQ, _Rectangle] :> (
		Map[placedContentToGraphics, innerContent]
	),
	other: _ :> Raise[
		DiagramError,
		"unexpected form for placed content cannot be converted to Graphics directives: ``",
		InputForm[other]
	]
}]

(*====================================*)

SetFallthroughError[makeSizedTextInset]

makeSizedTextInset[sizedText: _SizedText] := Module[{
	str,
	rect, min, max,
	center,
	width, height,
	fontSize
},
	{str, rect, min, max} = Replace[sizedText, {
		SizedText[str_?StringQ, rect:Rectangle[min_, max_]] :> {str, rect, min, max},
		other_ :> RaiseError["unexpected SizedText specification: ``", sizedText]
	}];

	center = Mean[{min, max}];
	{width, height} = RectangleSize[rect];

	(* Note:
		Font size in Wolfram graphics can be specified in one of two
		ways:

			FontSize -> _?NumberQ
			FontSize -> Scaled[_?NumberQ]

		The units of the first specification are printers points,
		which are based on the physical size-in-inches of the users
		screen. When using the first specification, the
		size-in-inches of the rendered text will be the same no
		matter what the size-in-inches of the overall graphic is. If
		the user resizes the graphic larger or smaller, the text
		will not change size.

		The units of the second specification are scaled based on
		the plot range of the graphic: if the user resizes the
		graphic larger or smaller, the text will resize to be the
		same size relative to the overall graphic.

		Using the first specification makes manual positioning and
		layout of text in a graphic essentially impossible, because
		the text will appear to change size relative to the overall
		graphic if the user resizes it. E.g. at the extreme end, if
		the user makes the graphic very small, the text will still
		be the same physical size, and would likely spill over and
		cover up the other elements in the diagram graphic.

		Using the second specification is a bit better, because the
		text will be the same relative size if the graphic is made
		smaller or larger, meaning that it will appear to stay in
		the position that we placed it in no matter how the user
		resizes the graphic.

		However, using scaled coordinates leaves one problem
		unsolved: what should the actual value passed to Scaled[..]
		be?

		Scaled[..] takes a value that is a percentage of the plot
		range. For almost all graphics primitives, it is relatively
		straightforward to determine the scaled size from the
		absolute size in graphics system coordinates: you just
		divide the x and y coordinates that appear explicitly in the
		primitive specification by the width and height,
		respectively, of the overall graphic.

		However, the Text[..] primitive is unique, in that the size
		of the rendered text is not specified in any argument that
		Text accepts. Compare with e.g. Point or Line or Rectangle,
		which all take explicit positions as arguments, so their
		inherent size within graphics coordinate space is clear and
		unambiguous.

		The only way to adjust the size of text is to set the
		FontSize option, which, as mentioned, takes units of either
		printers points or scaled units. Determining the right
		scaling factor is made further difficult by the fact that
		the scaling factor doesn't scale the overall width of the
		complete rendered text string, it effectively scales the
		point size of the font to the plot range, which means that
		the final width of the text is based on the font metrics of
		each individual font.

		If we want our text `str` to fill the complete area of the
		`textRect` computed during layout, we need to know the font
		metrics of each letter in `str` to get the total width of
		the string in printers points, and then do
		1/totalPrintersPoints to get the optimal Scaled[..] factor.

		At the moment, I don't know a good way to get the precise
		font metrics, so the calculation below is an approximation.

		The code below uses:
			Inset[Graphics[{FontSize -> fontSize, Text[...]}]]

		instead of just {FontSize -> fontSize, Text[...]} so that
		the Scaled[..] coordinates only need to be computed in
		terms of the rendered size of `str`, not the rendered size
		of the overall diagram graphic.
	*)
	(* FIXME:
		Determine the correct font metrics to make this factor precise, and
		instead of using StringLength of the entire string (which is incorrect)
		if there are multiple lines, use the widest line. *)
	fontSize = Scaled[1.75 / StringLength[str]];

	(* FIXME:
		Compute this font size based on the overall size of
		the graphic. *)
	Inset[
		Graphics[{
			FontSize -> fontSize,
			Text[str, {width, height} / 2]
		},
			Frame -> False,
			PlotRange -> {{0, width}, {0, height}},
			PlotRangePadding -> 0,
			ImagePadding -> 0
		],
		center,
		Automatic,
		{width, height},
		Background -> If[TrueQ[$DebugDiagramLayout],
			Directive[Opacity[0.2], Red],
			Automatic
		]
	]
]

(*====================================*)
(* Helper functions                   *)
(*====================================*)

SetFallthroughError[DiagramElementDirectives]

DiagramElementDirectives[DiaArrow[_, _, directive_]] := directive

DiagramElementDirectives[_DiaArrow] := {}
