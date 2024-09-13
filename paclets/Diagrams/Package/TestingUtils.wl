Package["Diagrams`TestingUtils`"]

PackageExport[{
	RunBlobTests
}]

PackageUse[Diagrams -> {
	DiagramError,
	Errors -> {SetFallthroughError, ConfirmReplace, WrapRaised},
	Utils -> {ConfirmFileType}
}]

(*========================================================*)

SetFallthroughError[RunBlobTests]

(*
	Run "blob" tests from a data directory with the following format:

		TestsDataDir
		|-- Test1
		|   |-- Input.wl
		|   \-- ExpectedOutput.wl
		|-- Test2
		|   |-- Input.wl
		|   \-- ExpectedOutput.wl
		\-- ...

	This function is intended to enable the following workflow:

	* The expected output expression is a "blob" of some kind, not necessarily
	  with an easily readable diff.

	* When a test fails, the user will want to visualize and inspect the new and
	  old blob in a notebook of some kind.

	  If the user wishes, it should be trivial to accept the new blob as correct
	  and overwrite the ExpectedOutput.wl file with the new blob.

	This balances towards several advantages over store the test inputs and
	outputs in a testing notebook:

	* The test inputs are "plain text" and easily diffable.

	* The test outputs retain some amount of readability and diffability,
	  avoiding the pitfalls of diffing entire notebooks, where one change is
	  difficult to follow in diff format, but several changes at once becomes
	  quite difficult to follow

	* Merge conflicts are isolated and easy to fix in a way that results in a
	  well-formed expression.

	* The expected output can be easily updated by clicking the 'Save' button
	  in the UI, making it "cheap" for the programmer to change the expression
	  form of the data, visually sanity check that the results are equivalent,
	  and update the expected test output.

	TODO:
		This function will return a Success or Failure object containing a link to
		the generated temporary testing report notebook, which provides a UI for
		running and editing any of the tests inputs or expected outputs.
*)
RunBlobTests[rootDir: _?StringQ] := Module[{
	caseDirs,
	testResults,
	equalCells,
	unequalCells
},
	ConfirmFileType[
		rootDir, Directory,
		"Invalid blob testing directory specification"
	];

	caseDirs = Select[
		FileNames[All, rootDir],
		FileType[#] === Directory &
	];

	(*-------------------------*)
	(* Evaluate the test cases *)
	(*-------------------------*)

	testResults = Map[
		caseDir |-> WrapRaised[
			DiagramError,
			"Error evaluting blob test case in directory: ``",
			caseDir
		] @ Module[{
			inputPath = FileNameJoin[{caseDir, "Input.wl"}],
			expectedOutputPath = FileNameJoin[{caseDir, "ExpectedOutput.wl"}],
			inputString,
			input,
			expectedOutput
		},
			ConfirmFileType[inputPath, File];
			ConfirmFileType[expectedOutputPath, File];

			inputString = Import[inputPath, "String"];
			input = Get[inputPath];
			expectedOutput = Get[expectedOutputPath];

			<|
				"Name" -> FileNameTake[caseDir],
				"InputPath" -> inputPath,
				"ExpectedOutputPath" -> expectedOutputPath,
				"InputText" -> inputString,
				"EvaluatedInput" -> input,
				"ExpectedOutput" -> expectedOutput
			|>
		],
		caseDirs
	];

	(*-------------------------------*)
	(* Construct the result notebook *)
	(*-------------------------------*)

	equalCells = {};
	unequalCells = {};

	Scan[
		case |-> ConfirmReplace[case, {
			KeyValuePattern[{
				"Name" -> name: _?StringQ,
				"InputPath" -> inputPath: _?StringQ,
				"ExpectedOutputPath" -> expectedOutputPath: _?StringQ,
				"InputText" -> inputText: _?StringQ,
				"EvaluatedInput" -> evaluatedInput: _,
				"ExpectedOutput" -> expectedOutput: _
			}] :> Module[{
				passedQ = evaluatedInput === expectedOutput,
				outputSaveButton,
				cell
			},
				outputSaveButton = ToBoxes @ Button[
					"Save",
					(
						Export[expectedOutputPath, evaluatedInput];

						CurrentValue[
							ParentCell @ EvaluationCell[],
							CellFrameLabels
						] = None;
					)
				];

				cell = Cell @ CellGroupData[{
					Cell[TextData[name], "Section"],
					Cell[TextData[{
						StyleBox["Input Path:", Bold],
						" ",
						inputPath
					}], "Text"],
					Cell[TextData["Input Program"], "Subsubsubsection"],
					Cell[TextData[inputText], "Program"],
					Cell[
						TextData["Expected Output"],
						"Subsubsubsection",
						Background -> LightGreen
					],
					Cell[
						BoxData @ ToBoxes @ expectedOutput,
						"Output"
					],
					Cell[
						TextData["Actual Output"],
						"Subsubsubsection",
						Background -> If[passedQ, LightGreen, LightOrange]
					],
					Cell[
						BoxData @ ToBoxes @ evaluatedInput,
						"Output",
						CellFrameLabels -> {
							{None, If[passedQ, None, outputSaveButton]},
							{None, None}
						}
					]
				}, Open];

				If[passedQ,
					AppendTo[equalCells, cell];
					,
					AppendTo[unequalCells, cell];
				];
			]
		}],
		testResults
	];

	nb = Notebook @ {
		Cell["Test Results", "Title"],
		Cell @ CellGroupData[{
			Cell[
				TextData[{
					"Passed ",
					StyleBox[
						TemplateApply["(``)", Length[equalCells]],
						"Subtitle"
					]
				}],
				"Chapter"
			],
			Splice[equalCells]
		}, Closed],
		Cell @ CellGroupData[{
			Cell[
				TextData[{
					"Failed ",
					StyleBox[
						TemplateApply["(``)", Length[unequalCells]],
						"Subtitle"
					]
				}],
				"Chapter"
			],
			Splice[unequalCells]
		}, Open]
	};

	NotebookPut[
		nb,
		Editable -> False,
		Saveable -> False,
		(* Make the window 800pts wide, and as tall as the screen. *)
		WindowSize -> {800, Automatic},
		WindowMargins -> {{Automatic, Automatic}, {0, 0}}
	]
]

