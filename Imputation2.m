(* ::Package:: *)

(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Imputation *)
(* :Context: Imputation` *)
(* :Author: jonaprieto *)
(* :Date: 2016-08-06 *)

(* :Package Version: 0.2 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 jonaprieto *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["Imputation`"];
(* Exported symbols added here with SymbolName::usage *)
ImputeVersion::usage = "ImputateVersion";
TestAlgorithm::usage = "TestAlgorithm[]";
Impute::usage = "Impute[]";


ClassifyMethods::usage = "Setter/Getter ClassifyMethods";
ModelDimensions::usage = "The dimension of the last dataset processed.";
MissingRate::usage = "Setter/Getter the missing rate. Values between 0.01 and 0.49.";
SetInitValues::usage = "Set the defaults values for every single variable of the package.";
DataBefore::usage = "The last dataset processed without any change.";
DataMissing::usage = "The last dataset after make the dataset with some missing values.";
DataAfter::usage = "The last dataset after the imputation process.";
PrintData::usage = "A nice print function.";


Off[AbortAssert];
AbortAssert::trace = "Assertion failed ``.";
(*AbortAssert /: On[AbortAssert] := On[AbortAssert::trace];*)
(*AbortAssert /: Off[AbortAssert] := Off[AbortAssert::trace];*)
SetAttributes[AbortAssert, {HoldFirst}];
AbortAssert[test_, message__] :=
    Check[TrueQ[test] || Message[AbortAssert::trace, {HoldForm[test], message}],
      Abort[]];

Begin["`Private`"];

ImputeVersion = "Imputation Package v0.1.2";
Print[ImputeVersion];


$cObject = 0;
$cTask = "";
$FailCompleteSymbol = Missing["Failure"];
$FailCompleteSymbolPrint = "\!\(\* StyleBox[\"*\",\nFontSize->24,\nBackground->RGBColor[1, 0.5, 0.5]]\)";
$I = {};
$J = {};
$listMethods = {"NaiveBayes"};
$M = {};
$method = $methodByDefault;
$methodByDefault = "NaiveBayes";
$missingRate = 0.1;
$missingSymbol = Missing[];
$missingSymbolPrint = "\!\(\* StyleBox[\"?\",\nFontSize->18,\nBackground->RGBColor[1, 1, 0]]\)";
$missingX = {};
$n = 0;
$m = 0;
$numIncompleteRows = 0;
$numLaps = 2;
$numMissings = 0;
$oldX = {};
$orderCols = 1;
$orderRows = -1;
$R = {};
$rangeN = {};
$rangeM = {};
$s = $m - 1;
$X = {};
$Y = {};


Clear[SetInitValues]
SetInitValues[] := Module[{},
  $X = {};
  $oldX = {};
  $missingX = {};
  $n = 0; $m = 0;
  $rangeN = {}; $rangeM = {};
  $s = $m - 1;

  $M = {};
  $Y = {};
  $I = {};
  $J = {};
  $R = {};
  $numMissings = 0;
  $numIncompleteRows = 0;
  $cObject = 0;
  $cTask = "";
];
SetInitValues[];

Clear[ClassifyMethods];
ClassifyMethods[list_List] := ($listMethods = list);
ClassifyMethods[] := Return[$listMethods];

Clear[ModelDimensions];
ModelDimensions[] := Return[Dimensions[$X]];

Clear[MissingRate];
MissingRate[rate_Real] := ($missingRate = rate);
MissingRate[] := Return@$missingRate;

Clear[DataBefore, DataMissing, DataAfter];
DataBefore[] := Return@$oldX;
DataMissing[] := Return@$missingX;
DataAfter[] := Return@$X;

Clear[PrintData];
PrintData[ex_] := Module[{nX, n, m},
  {n, m} = Dimensions@ex;
  nX = ex;
  Table[
    nX[[i, j]] =
        If[ MissingQ[ex[[i, j]]], $missingSymbolPrint , nX[[i, j]]];
    , {i, 1, n}, {j, 1, m}];
  Print[TableForm@nX];
];

(*http://goo.gl/hMyhjI*)
Clear[Complement2];
Complement2[A_, B_] := DeleteCases[A, Alternatives @@ B];

Clear[Union2];
Union2[A_, B_] := Union[Flatten[{A, B}]];

(*http://goo.gl/5O7jhs*)
Clear[MakeArrange];
MakeArrange[{n_, m_}, p_] := Module[{base = PadLeft[ConstantArray[1, Round[n m p]], n m], cand},
  While[(cand = ArrayReshape[RandomSample@base, {n, m}];
  Max[Total[cand]] > n - 2 || Max[Total /@ cand] > m - 2)];
  Position[cand, 1]
];


Clear[SetMissings];
SetMissings[database_, per_] := (
  $X = database;
  If[ per > 0 && per < 1, $missingRate = per];
  SetMissings[]
);

SetMissings[] := Module[
  {cant, ms},
  $oldX = $X;

  With[{d = Dimensions@$X},
    AbortAssert[Length@d == 2, "SetMissings"];
    {$n, $m} = d;
    AbortAssert[$n > 2 && $m > 2, "SetMissings"];
  ];

  $s = $m - 1;
  AbortAssert[$s > 0 && $n > 1, "SetMissings"];
  AbortAssert[MissingRate[] < 1 && MissingRate[] > 0, "SetMissings"];
  $cTask = "Putting missing values in the dataset";
  cant = Ceiling[$n * $s * MissingRate[]];
  ms = MakeArrange[{$n, $s}, MissingRate[]];
  Table[
    $X[[pos[[1]], pos[[2]]]] = $missingSymbol, {pos, ms}];
  $numMissings = Length[ms];
  AbortAssert[$numMissings > 0, "SetMissings"];
  $missingX = $X;
];



Clear[TestAlgorithm];
TestAlgorithm[datasets_List, numIter_Integer] := Module[
  {oData, name, citer, outcome = <||>, matches, cDataset = 0, resPerMethod, oldJ},

  SetInitValues[];
  AbortAssert[ $missingRate > 0 && $missingRate < 0.5 ];

  $lastResult = "";

  $minResult = Infinity;
  $maxResult = -Infinity;

  PrintTemporary@Dynamic@Dataset[
    <|
      "No." -> ToString@cDataset <> "/" <> ToString@Length@datasets,
      "Dataset" -> name,
      "Missing Rate" -> MissingRate[],
      "No. Instances" -> $n,
      "No. Attributes" -> $m,
      "No. Missing Values" -> ($numMissings - 1),
      "Current Method" -> $method,
      "Current Iteration" -> ToString@citer <> "/" <> ToString@numIter,
      "Current Incomplete Row" -> ToString@$cObject <> "/" <> ToString@$numIncompleteRows,
      "Last result" -> ToString@$lastResult,
      "Min. result" -> $minResult,
      "Max. result" -> $maxResult 
    |>
  ];

  PrintTemporary@Dynamic@$cTask;

  cDataset = 1;
  Table[
    $cTask = "Importing dataset data";
    name = dataset["filename"];
    oData = Import@dataset["data"];
    With[{d = Dimensions@oData},
      AbortAssert[Length@d == 2, "TestAlgorithm"];
      {$n, $m} = d;
      AbortAssert[$n > 2 && $m > 2, "TestAlgorithm"];
    ];
    outcome[cDataset] = <|
      "Dataset" -> name,
      "Size" -> ToString@$n <> "x" <> ToString@$m
    |>;

    Table[
      $method = method;
      $minResult = Infinity;
      $maxResult = -Infinity;
      $cTask = "Starting... " <> ToString@$method;
      resPerMethod = 0.0;
      citer = 0;
      resPerMethod = Table[
        citer++;
        $X = oData;
        SetMissings[];
        StepOne[]; oldJ = $J;
        AbortAssert[$numLaps > 0, "TestAlgorithm"];
        StepTwo[$numLaps];
        $cTask = "Computing stats...";
        matches = checkMatches[oldJ];
        With[{res = N[Total@matches / $numMissings]},
          $lastResult = res;
          $minResult = Min[$minResult, res];
          $maxResult = Max[$maxResult, res];
          res
        ]
        , {numIter}];
      outcome[cDataset][ToString@method] = <|
      "min"-> $minResult, 
      "mean" -> NumberForm[Mean@resPerMethod, {3, 2}],
      "measures"-> resPerMethod,
      "max" ->  $maxResult
      |>;
      , {method,  {"NaiveBayes","RandomForest"} }];
    cDataset++;
    , {dataset, datasets}];
  Return@outcome;
];

Clear[checkMatches];
checkMatches[oldJ_] := Module[
  {matches},
  matches = Table[
    Length[oldJ[[i]]] - HammingDistance[$X[[i, oldJ[[i]]]], $oldX[[i, oldJ[[i]]]]]
    , {i, $n}];
  Return@matches;
];

Clear[Impute];
Impute[ex_] := Module[
  {mm, yy, jj, rr, ii, sis, mtt, nx, lenmiss, n, m},
  $X = ex;
  StepOne[];
  StepTwo[2];
  Return@$X;
];


Clear[StepOne];
StepOne[] := Module[
{n=0,m=0},

If[Length@Dimensions[$X] != 2, Print["Step One, invalid $X"]; Return[]];

{n, m} = Dimensions[$X];

Table[
	$V[k] = {};
	$OMS[k] = {};
,{k, 1, m}];

$MOS = {};

Table[
	$MAS[i] = {}; $NS[i] = {};
	Table[
		If[ MissingQ@$X[[i,k]],
			$MAS[i] = {$MAS[i], k};
			$OMS[k] = {$OMS[k], i};
		,
			$V[k] = {$V[k], $X[[i, k]]};
		];
	,{k, 1, m}];

	If[ Length@$MAS[i] >= 1, $MOS = {$MOS, i}];
	$Mlv[i,i] = 1;
	Table[
		$GM[i,j] = {}; $Mlv[i,j] = 1;
		Table[
			If[ MissingQ@$X[[i,k]],
				If[  MissingQ@$X[[j,k]],
					$Mlv[i,j] = $Mlv[i,j] / (Length[$V[k]]^2);
				,   $Mlv[i,j] = $Mlv[i,j] / Length[$V[k]];
				];
			, If[  MissingQ@$X[[j,k]], 
					$Mlv[i,j] = $Mlv[i,j] / Length[$V[k]];
					,
					If[ $X[[i,k]]!= $X[[j,k]],
						$Mlv[i,j] = 0;
						$GM[i,j] = {$GM[i,j], k};	
					];
		        ];
		    ];
		,{k,1 , m}];
		If[ Length@$GM[i,j] == 0,
			$NS[i] = {$NS[i], j};
		];
	,{j, i+1, n}];
,{i, 1, n}];

(* simplify every list to become a set *)
$MOS = Union@Flatten[$MOS, Infinity];
Table[
	$MAS[i] = Union@Flatten[$MAS[i], Infinity];
	$NS[i] = Union@Flatten[$NS[i], Infinity];
	Table[ $GM[i,j] = Union@Flatten[$GM[i,j], Infinity], {j,i+1,m}];	
,{i, 1, n}];

Table[
	$V[k] = Union@Flatten[$V[k], Infinity];
	$OMS[k] = Union@Flatten[$OMS[k], Infinity];
,{k, 1, m}];

];


Clear[StepTwo];
StepTwo[lap_] := Module[
{valMaxJ, maxJ, n,m},
If[ Length@Dimensions != 2, Print["Step Two. Invalid $X."]; Return[]];
{n,m} = Dimensions[$X];

$MOS = SortBy[$MOS, -$MAS[#] &];
Table[ 
	maxJ = -1; valMaxJ = -1;
	Table[
		If[ i != j && $Mlv[i,j] > valMaxJ,
			maxJ = j; valMaxJ = $Mlv[i,j];
		];
	, {j, 1, n}];
	
	If[ maxJ > 0,
		Table[
			$X[[i,k]] = $X[[maxJ,k]];
			$OMS[k] = DeleteCases[$OMS[k], i];
		,{k, 1, m}];
		$MAS[i] = {};
		$MOS = DeleteCases[$MOS, i];
		Table[
			$Mlv[i,j] = 1; $GM[i,j] = {};
			Table[
				If[ MissingQ@$X[[j,k]],
					$Mlv[i,j] = $Mlv[i,j] / Length[$V[k]];
				,   $Mlv[i,j] = 0;
					$GM[i,j] = {$GM[i,j], k};
				];
			, {k, 1, m}];
			$GM[i,j] = Union@Flatten[$GM[i,j], Infinity];
			If[ Length@$GM[i,j] == 0,
				$NS[i] = {$NS[i], j};
			];
		, {j, 1, n}];
		
		$NS[i] = Union@Flatten[$NS[i], Infinity];
	];
	
,{i, 1, n}];
  
];


Clear[MeanCompleter];
MeanCompleter[]:= Module[
{n,m, freq},
If[Length@Dimensions@$X ==0, Print["MeanCompleter. Invalid $X"]; Return[]];
{n,m} = Dimensions[$X];

Table[
	freq = SortBy[Tally@Table[$X[[i,k]], {i,1,n}], -#[[2]] &];
	Table[
		If[ MemberQ[$MAS[i], k],
			$X[[i,k]] = freq;
		];
	,{i, $MOS}];
	
,{k, 1,m}];
];


Clear[ClassifyReduceModel];
ClassifyReduceModel[i_Integer, j_Integer] := Module[
  {rowsRef1, colsRef1, ref1, rowsRef2, colsRef2, ref2,
    rowsRef3, colsRef3, ref3, answers},
  $cTask = "Reduce-model imputation ClassifyReduceModel[i,j] ";
  AbortAssert[Length@$R[[i]] >= 2, "ClassifyReduceModel"];

  answers = $X[[All, j]];

  If[ Length@Union@Complement2[answers, {i}],
    Return[First@answers];
  ];

  AbortAssert[MemberQ[$I[[j]], i], "ClassifyReduceModel"];

  rowsRef1 = Complement2[$rangeN, $I[[j]]];
  colsRef1 = Complement2[$R[[i]], {$m}];
  ref1 = $X[[rowsRef1, colsRef1]];

  (*PrintTemporary["ref1 ",Length /@ {rowsRef1, colsRef1}];*)

  AbortAssert[Not@MemberQ[$X[[i, colsRef1]], Missing[]], "ClassifyReduceModel"];
  AbortAssert[ Length@rowsRef1 > 0, "ClassifyReduceModel"];

  If[ Length@rowsRef1 == 1,
    With[{class = answers[[ rowsRef1[[1]] ]] },
      Return@class;
    ];
  ];
  If[ Length@Union@answers[[rowsRef1]] == 1,
    Return@answers[[ rowsRef1[[1]] ]];
  ];

  rowsRef2 = Select[rowsRef1, Intersection[$J[[#]], $R[[i]]] == {} &];
  colsRef2 = colsRef1;

  (*PrintTemporary["ref2 ",Length /@ {rowsRef2, colsRef2}];*)

  AbortAssert[Not@MemberQ[$X[[i, colsRef2]], Missing[]], "ClassifyReduceModel"];

  ref2 = $X[[rowsRef2, colsRef2]];

  If[ Length@rowsRef2 == 1,
    With[{class = answers[[ rowsRef2[[1]] ]] },
      Return@class;
    ];
  ];

  If[ Length@rowsRef2 > 0 && Length@Union@answers[[rowsRef2]] == 1,
    Return@answers[[ rowsRef2[[1]] ]];
  ];

  If[ Length@rowsRef2 >= 0 && Length@rowsRef2 <= 100,
    Return@ClassifyReducedModel[rowsRef2, colsRef2, answers[[rowsRef2]], $X[[i, colsRef2]]];
  ];

  If[ Length@rowsRef2 == 0,
    rowsRef2 = rowsRef1;
    colsRef2 = colsRef1;
    ref2 = ref1;
  ];

  rowsRef3 = RandomSample[rowsRef2, UpTo@100];
  colsRef3 = FindReduct[rowsRef2, colsRef2, j];
  ref3 = $X[[ rowsRef3, colsRef3 ]];

  (*PrintTemporary["ref3 ",Length /@ {rowsRef3, colsRef3}];*)
  (*PrintTemporary[];*)

  AbortAssert[Not@MemberQ[$X[[i, colsRef2]], Missing[]], "ClassifyReduceModel"];


  If[ Length@rowsRef3 == 1,
    With[{class = answers[[ rowsRef3[[1]] ]] },
      Return@class;
    ];
  ];

  If[ Length@Union@answers[[rowsRef3]] == 1,
    Return@answers[[ rowsRef3[[1]] ]];
  ];

  Return@ClassifyReducedModel[rowsRef3, colsRef3, answers[[rowsRef3]], $X[[i, colsRef3]] ];
];

Clear[ClassifyReducedModel];
ClassifyReducedModel[rows_, cols_, answers_, goal_] := Module[
  {model, c},
  model = $X[[rows, cols]] -> answers;

  AbortAssert[Length@rows > 1, "ClassifyReducedModel"];
  AbortAssert[Length@rows == Length@answers, "ClassifyReducedModel"];
  AbortAssert[Length@Union@answers > 1, "ClassifyReducedModel" ];
  AbortAssert[Length@goal == Length@cols, "ClassifyReducedModel"];

  c = Classify[model, Method -> $method, PerformanceGoal -> "Quality"];
  Return[c[goal]];
];

Clear[FindReduct];
FindReduct[rows_, cols_, j_] := Module[
  {reduct, ncols, nn, mm, t},
  nn = Length@rows; mm = Length@cols + 1;
  Clear["RS`*"];
  << RS`;
  RS`Universe@$X[[rows, cols ~ Join ~ {j} ]];
  RS`Attrs@Range[mm];
  RS`Conditions@Range[mm - 1];
  RS`Decisions@{mm};
  RS`Base@RS`Conditions[];
  Print["reduct..."];
  t = AbsoluteTiming[
    ncols = RS`QuickReduct[];
  ];
  Print["termino un reduct... ", First@t];

  Return@If[ Length@ncols > 0, cols[[ncols]], cols];
];


End[];

(*`Private`*)



EndPackage[];
