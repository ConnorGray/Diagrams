Package["Diagrams`Concepts`"]

PackageExport[{
	$BoxConcepts,

	DiaFunctionPointer,
	DiaPaclet,
	DiaSoftwareProcess,
	DiaSoftwareExecutable,
	DiaDynamicLibrary,
	DiaFunction
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

DiaSoftwareProcess::usage = "DiaSoftwareProcess[id, content] represents a software OS process."
DiaSoftwareExecutable::usage = "DiaSoftwareExecutable[id, content] represents a software executable program."
DiaDynamicLibrary::usage = "DiaDynamicLibrary[id, content] represents a software dynamic library."
DiaFunction::usage = "DiaFunction[id, content] represents a named software function."

PackageUse[Diagrams -> {
	DiaBox, DiaArrow, AttachmentQ, MakeDiagramPrimitives,
	DiagramError,
	Render -> {SizedText},
	Utils -> AbsoluteOptions2,
	Errors -> {
		Raise, RaiseError, RaiseAssert, RaiseConfirmMatch, ConfirmReplace,
		SetFallthroughError
	}
}]

(*========================================================*)

$BoxConcepts = {
	DiaFunction,
	DiaPaclet,
	DiaSoftwareProcess,
	DiaSoftwareExecutable,
	DiaDynamicLibrary
}

(*====================================*)


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

Options[DiaPaclet] = {
	Background -> Lighter[Blend[{Green, Blue}], 0.7]
}

DiaPaclet /: MakeDiagramPrimitives[
	diaPaclet: _DiaPaclet
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
		AbsoluteOptions2[diaPaclet]
	]
]

(*====================================*)

Options[DiaSoftwareProcess] = {
	Background -> Blend[{Gray, Blue}, 0.7]
}

DiaSoftwareProcess /: MakeDiagramPrimitives[
	primitive: _DiaSoftwareProcess
] := (
	processStandardPrimitive[primitive]
)

(*====================================*)

Options[DiaSoftwareExecutable] = {
	Background -> Blend[{Gray, Blue}, 0.3]
}

DiaSoftwareExecutable /: MakeDiagramPrimitives[
	primitive: _DiaSoftwareExecutable
] := (
	processStandardPrimitive[primitive]
)

(*====================================*)

Options[DiaDynamicLibrary] = {
	Background -> Blend[{Gray, Purple}, 0.7]
}

DiaDynamicLibrary /: MakeDiagramPrimitives[
	primitive: _DiaDynamicLibrary
] := (
	processStandardPrimitive[primitive]
)

(*====================================*)

Options[DiaFunction] = {
	Background -> Blend[{Gray, Orange}, 0.7]
}

DiaFunction /: MakeDiagramPrimitives[
	primitive: _DiaFunction
] := (
	processStandardPrimitive[primitive]
)

(*========================================================*)
(* Helpers                                                *)
(*========================================================*)

SetFallthroughError[processStandardPrimitive]

processStandardPrimitive[
	primitive: (head_Symbol[args___])
] := Module[{
	id,
	content
},
	{id, content} = ConfirmReplace[primitive, {
		head[
			id: _?StringQ,
			___?OptionQ
		] :> {id, id},
		head[
			id: _?StringQ,
			content: Except[_?OptionQ],
			___?OptionQ
		] :> {id, content},
		other_ :> Raise[
			DiagramError,
			"Unexpected form for diagram primitive: ``",
			InputForm[other]
		]
	}];

	content = MakeDiagramPrimitives[content];

	DiaBox[
		id,
		content,
		Sequence @@ AbsoluteOptions2[primitive]
	]
]