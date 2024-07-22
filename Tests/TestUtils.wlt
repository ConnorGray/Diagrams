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