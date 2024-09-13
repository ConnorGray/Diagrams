BlockDiagram[
	(*"Wolfram Kernel Process",*)
	{
		DiaBox["./WolframKernel", Background -> LightRed],
		DiaBox["libbasic_types.dylib"],
		DiaBox["libfoo.dylib"]
	},
	{
		DiaFunctionPointer[
			{"./WolframKernel", Nearest} -> "libbasic_types.dylib",
			"square()"
		],
		DiaFunctionPointer[
			{"./WolframKernel", Nearest} -> "libbasic_types.dylib",
			"add2()"
		],
		DiaFunctionPointer[
			{"./WolframKernel", Nearest} -> "libfoo.dylib",
			"total_i64()"
		]
	},
	DiagramLayout -> {
		"EqualWidthRows",
		BoxPadding -> 48
	}
]