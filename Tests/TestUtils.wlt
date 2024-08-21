(* TODO: Fix DiagramsLoader.wl and make this Needs *)
Get["Diagrams`"]

(*========================================================*)
(* Layout Utilities                                       *)
(*========================================================*)

VerificationTest[
	RectangleAttachmentPoint[
		Rectangle[{0, 0}, {1, 1}],
		{Left, 0.5}
	],
	{0, 0.5}
]

VerificationTest[
	RectangleAttachmentPoint[
		Rectangle[{0, 0}, {1, 1}],
		{Top, 0.25}
	],
	{0.25, 1}
]

VerificationTest[
	RectangleAttachmentPoint[
		Rectangle[{0, 0}, {100, 50}],
		{Top, 0.5}
	],
	{50., 50}
]

(*========================================================*)
(* Developer UX                                           *)
(*========================================================*)

(*====================================*)

Module[{
	foo, bar
},
	(*-------*)
	(* Setup *)
	(*-------*)

	Options[foo] = {
		"A" -> 1,
		"B" -> 2
	};

	foo[arg_, optsSeq : OptionsPattern[]] := Module[{},
		bar[arg, ForwardOptions[optsSeq]]
	];

	Options[bar] = {
		"B" -> 3
	};

	bar[arg_, optsSeq : OptionsPattern[]] := Module[{},
		{arg, "bar opts"[optsSeq]}
	];

	(*-------*)
	(* Tests *)
	(*-------*)

	VerificationTest[
		foo[5, "A" -> 1],
		{5, "bar opts"[]}
	];

	VerificationTest[
		foo[5, "B" -> 2],
		{5, "bar opts"["B" -> 2]}
	];

	VerificationTest[
		foo[5, ForwardOptions["B" -> 10]],
		{5, "bar opts"["B" -> 10]}
	];

	VerificationTest[
		foo[5, "A" -> 7, ForwardOptions["B" -> 10]],
		{5, "bar opts"["B" -> 10]}
	];

	VerificationTest[
		foo[5, "A" -> 7, "B" -> 10, ForwardOptions["B" -> 11]],
		{5, "bar opts"["B" -> 10, "B" -> 11]}
	];
]