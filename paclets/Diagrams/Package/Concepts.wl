Package["Diagrams`Concepts`"]

PackageExport[{
	DiaFunctionPointer,
	DiaPaclet
}]

(* TODO:
	Move this into a separate package. These are essentially 'plugins' that
	extend the core diagrams package with new diagram types.
*)

(* TODO:
	What about function pointers within the same library? E.g. virtual methods
	are based on function pointers.
*)
DiaFunctionPointer::usage = "DiaFunctionPointer[libA -> libB, name] represents pointer from libA to the function name in libB."

DiaPaclet::usage = "DiaPaclet[paclet] represents a Wolfram paclet."


PackageUse[Diagrams -> {
	DiaBox, DiaArrow, AttachmentQ, MakeDiagramPrimitives,
	Render -> {SizedText},
	Errors -> {RaiseError, RaiseAssert, RaiseConfirmMatch}
}]


$functionPointerArrowheads = Arrowheads[{
	{-Automatic, Automatic, {Graphics @ {
		Transparent,
		EdgeForm[Directive[Thick, Gray]],
		Rectangle[{0.5,-0.5},{1.5,0.5}]
	},-0.5}},
	{Automatic, Automatic, {Graphics @ {
		LightGray,
		EdgeForm[Directive[Thick, Gray]],
		Rectangle[{0.5,-0.5},{1.5,0.5}]
	}, -0.5}}
}]

DiaFunctionPointer /: MakeDiagramPrimitives[
	pointer_DiaFunctionPointer
] := Module[{
	libA, libB, name
},
	(* TODO: Instead of StringQ here, create and use AttachmentQ, and provide a
		accessor function for extracting the element ID from an attachment spec. *)
	{libA, libB, name} = Replace[pointer, {
		DiaFunctionPointer[libA_ -> libB_, name_?StringQ] :> {
			libA,
			libB,
			name
		},
		_ :> RaiseError["unrecognized DiaFunctionPointer specification: ``", pointer]
	}];

	RaiseAssert[AttachmentQ[libA] && AttachmentQ[libB]];

	DiaArrow[libA -> libB, name, $functionPointerArrowheads]
]

(*====================================*)

DiaPaclet /: MakeDiagramPrimitives[
	diaPaclet_DiaPaclet
] := Module[{
	paclet,
	pacletName,
	pacletVersion
},
	paclet = Replace[diaPaclet, {
		DiaPaclet[paclet_] :> paclet,
		_ :> RaiseError["unrecognized DiaPaclet specification: ``", diaPaclet]
	}];

	{pacletName, pacletVersion} = Replace[paclet, {
		name_?StringQ :> {name, Missing["NotAvailable"]},
		pacletObj_?PacletObjectQ :> {
			RaiseConfirmMatch[pacletObj["Name"], _?StringQ],
			pacletObj["Version"]
		},
		_ :> RaiseError["unexpected DiaPaclet paclet specification: ``", paclet]
	}];

	DiaBox[
		pacletName,
		Column[{
			Text[pacletName],
			If[StringQ[pacletVersion],
				Text[RaiseConfirmMatch[paclet["Version"], _?StringQ]]
				,
				Nothing
			]
		}],
		Background -> Lighter[Blend[{Green, Blue}], 0.7]
	]
]
