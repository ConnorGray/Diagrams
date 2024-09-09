Package["Diagrams`Kinds`Block`"]

PackageUse[Diagrams -> {
	Diagram,
	DiagramError,
	DiaID,
	DiaBox,
	DiaArrow,

	BlockDiagram,

	makeSizedTextInset,

	MakeDiagramPrimitives,
	LayoutDiagram,
	RenderPlacedDiagramToGraphics,
	DiagramGraphicsImage,

	DiagramLayout, DiagramTheme,

	Render -> SizedText,
	Layout -> $DebugDiagramLayout,
	Library -> $LibraryFunctions,
	Utils -> {
		OutputElementsQ, ConstructOutputElements, ForwardOptions,
		RectangleAttachmentPoint
	},
	Errors -> {
		Raise, Handle, SetFallthroughError, ConfirmReplace, RaiseAssert,
		WrapRaised, RaiseError, RaiseConfirm
	}
}]

(*========================================================*)

Options[BlockDiagram] = {
	DiagramLayout -> Automatic,
	DiagramTheme -> Automatic,
	Method -> Automatic,

	(* Graphics options *)
	Axes :> $DebugDiagramLayout
}

SetFallthroughError[BlockDiagram]

BlockDiagram[
	Optional[title: _?StringQ, None],
	boxes0: _?ListQ,
	arrows0: _?ListQ,
	OptionsPattern[]
] := Handle[_Failure] @ WrapRaised[
	DiagramError,
	"Error creating BlockDiagram"
] @ Module[{
	boxes = boxes0,
	arrows = arrows0
},
	boxes = Map[validatedMakeDiagramPrimitives, boxes];
	arrows = Map[validatedMakeDiagramPrimitives, arrows];

	(*-------------------------------------------*)
	(* Compute the rendered form of the diagram. *)
	(*-------------------------------------------*)

	graphics = ConfirmReplace[OptionValue[Method], {
		Automatic :> Module[{
			theme = RaiseConfirm @ OptionValue[DiagramTheme],
			placed,
			graphics,
			axes = Replace[Lookup[Options[diagram], Axes], {
				_?MissingQ | Automatic :> TrueQ[$DebugDiagramLayout],
				other_ :> other
			}]
		},
			placed = LayoutDiagram[boxes, arrows, OptionValue[DiagramLayout]];
			graphics = RenderPlacedDiagramToGraphics[placed, theme];

			RaiseAssert[ListQ[graphics]];

			graphics = ReplaceAll[graphics,
				sizedText: _SizedText :> makeSizedTextInset[sizedText]
			];

			graphics = Graphics[graphics, Axes -> axes];

			If[StringQ[title],
				Labeled[
					graphics,
					title,
					Top,
					Frame -> True,
					FrameMargins -> 10,
					Background -> LightGray,
					RoundingRadius -> 6,
					Spacings -> 4
				]
				,
				graphics
			]
		],
		"alpha-v2" :> Module[{placed, graphics},
			placed = LayoutDiagram[boxes, arrows, OptionValue[DiagramLayout]];
			graphics = RenderPlacedDiagramToGraphics[placed];

			DiagramGraphicsImage[Graphics[N @ Flatten @ graphics]]
		],
		"alpha-v1" :> Module[{result},
			result = $LibraryFunctions["diagram_image"][Unevaluated @ Diagram[boxes, arrows]];

			Replace[result, {
				bytes:{___?IntegerQ} :> ImageCrop @ ImportByteArray[ByteArray[bytes], "PNG"]
			}]
		]
	}];

	(*--------------------------------*)
	(* Return the finished diagram    *)
	(*--------------------------------*)

	Diagram[<|
		Replace[title, {
			None -> Nothing,
			_ :> ("Title" -> title)
		}],
		"Graphics" -> graphics
	|>]
]

(*------------------------------------*)

(*
	Validate that the return values of MakeDiagramPrimitives are in fact valid
	diagram primitives. Doing this validation is particularly important for UX
	because MakeDiagramPrimitives is intended to be overloaded by users, so we
	want to be proactive and helpful about validating the forms they return to
	us.
*)
(* TODO:
	If we're reducing forms that should reduce to DiaArrow, we should be
	validating that we didn't get a DiaBox[..], and vice versa. *)
validatedMakeDiagramPrimitives[args___] := Module[{result},
	result = MakeDiagramPrimitives[args];

	Replace[result, {
		DiaBox[id_?StringQ, Optional[content:Except[_?OptionQ], None], ___?OptionQ] :> result,
		(* FIXME: Validate these attachment specifications proactively. *)
		DiaArrow[lhs_ -> rhs_, _?StringQ, Optional[_, None], ___?OptionQ] :> result,
		_ :> RaiseError[
			"MakeDiagramPrimitives of `` returned invalid diagram primitive: ``",
			InputForm[args],
			InputForm[result]
		]
	}]
]