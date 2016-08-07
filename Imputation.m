(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Imputation *)
(* :Context: Imputation` *)
(* :Author: jonaprieto *)
(* :Date: 2016-08-06 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 jonaprieto *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["Imputation`", {"RS`"}]
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

ImputeVersion = "0.1.1";
PrintTemporary[ImputeVersion];

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

  PrintTemporary@Dynamic@Dataset[
    <|
      "Name of Dataset" -> name,
      "No." -> ToString@cDataset <> "/" <> ToString@Length@datasets,
      "No. Instances" -> $n,
      "No. Attributes" -> $m,
      "No. Missing Values" -> $numMissings,
      "Current Method" -> $method,
      "Current Iteration" -> ToString@citer <> "/" <> ToString@numIter,
      "Current Incomplete Row" -> ToString@$cObject <> "/" <> ToString@$numIncompleteRows
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
        matches = Table[
          Length[oldJ[[i]]] - HammingDistance[$X[[i, oldJ[[i]]]], oData[[i, oldJ[[i]]]]]
          , {i, $M}];
        N[Total[matches] / $numMissings]
        , {numIter}];
      outcome[cDataset][ToString@method] = NumberForm[Mean[resPerMethod], {3, 2}];
      , {method, $listMethods}];
    cDataset++;
    , {dataset, datasets}];

  Return@outcome;
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
StepOne[] := Module[{},
  $cTask = "Running ... StepOne";
  With[{d = Dimensions@$X},
    AbortAssert[Length@d == 2, "StepOne"];
    {$n, $m} = d;
    AbortAssert[$n > 2 && $m > 2, "StepOne"];
  ];
  $s = $m - 1;
  $J = Table[{}, {$n}];
  $R = Table[{$m}, {$n}];
  $I = Table[{}, {$s}];
  $numMissings = 0;
  $numIncompleteRows = 0;

  Table[
    Table[
      If[ MissingQ[$X[[i, j]]],
        $I[[j]] = {$I[[j]], {i}};
        $J[[i]] = {$J[[i]], {j}};
        $numMissings++,
        $R[[i]] = {$R[[i]], {j}};
      ];
      , {j, $s}];
    If[ Length@$J[[i]] > 0, $M = {$M, {i}} ];
    , {i, $n}];

  Table[
    $J[[i]] = Union@Flatten[$J[[i]], Infinity];
    $R[[i]] = Union@Flatten[$R[[i]], Infinity];
    , {i, $n}];
  Table[
    $I[[j]] = Flatten[$I[[j]], Infinity]
    , {j, $s}];

  $M = Union@Flatten[$M, Infinity];
  $numIncompleteRows = Length@$M;
  $Y = Complement2[$rangeN, $M];

  AbortAssert[Length@$M > 0, "StepOne"];
  Table[
    AbortAssert[Length@$I[[j]] < $n, "StepOne"];
    , {j, $s}];
  Table[
    AbortAssert[Length@$J[[i]] <= ($m - 2), "StepOne"];
    AbortAssert[Length@$R[[i]] >= 2, "StepOne"];
    , {i, $n}];
];


Clear[StepTwo];
StepTwo[lap_] := Module[{},
  AbortAssert[lap > 0, "StepTwo"];

  $cTask = "Imputation Phase... (" <> ToString[lap] <> ")";
  $cObject = 0;

  If[$numIncompleteRows == 0, Return[]];
  $M = SortBy[$M, $orderRows * Length@$J[[#]] &];
  Table[
    $cObject++;
    $J[[i]] = SortBy[$J[[i]], $orderCols * Length@$I[[#]] &];
    Table[
      AbortAssert[ Length@$R[[i]] > 1];
      $X[[i, j]] = BlackBox[i, j];
      If[ Not@MissingQ[$X[[i, j]]],
        $R[[i]] = $R[[i]] ~ Join ~ {j};
        $I[[j]] = Complement2[$I[[j]], {i}];
      ];
      , {j, $J[[i]]}];
    If[ Length@$R[[i]] == $m,
      $Y = $Y ~ Join ~ {i}; $J[[i]] = {},
      $J[[i]] = Complement2[$rangeM, $R[[i]]];
    ];
    , {i, $M}];

  $M = Complement2[$M, $Y];
  If[ Length@$M > 0 && lap > 1,
    StepTwo[lap - 1]
  ];
];


Clear[BlackBox]
BlackBox[i_, j_] := Module[
  {goal, classes, classes2, trainingset, goods, refined},
  (*$cTask = "Reduce-model imputation BlackBox[i,j] ";*)
  (*AbortAssert[Length@$R[[i]] >= 2];*)
  (*goal = $X[[ i, $R[[i]] ]];*)
  (*goods = Complement2[$rangeN, $I[[j]]];*)
  (*Assert[Length@goods >= 1];*)
  (*If[ Length@goods == 0,*)
    (*Return[ $FailCompleteSymbol];*)
  (*];*)

  (*classes = Union@$X[[goods, j]];*)
  (*If[ Length@classes == 1,*)
    (*Return[First@classes]*)
  (*];*)
  (*refined = Select[goods, Intersection[$J[[#]], $R[[i]]] == {} &];*)
  (*classes2 = Union@$X[[ refined, j]];*)

  (*Which[*)
    (*Length@refined == 0, domain = goods,*)
    (*Length@refined == 1, Return[First@classes],*)
    (*Length@classes2 == 1, Return[First@classes],*)
    (*Length@refined > Ceiling[$n * 0.1], domain = refined,*)
    (*True,*)
    (*Return@$FailCompleteSymbol*)
    (*domain = goods*)
  (*];*)
  (*If[ $method == "RWNB",*)
    (*Clear["RS`*"];*)
    (*<< RS`;*)
    (*Clear[RS`universe, RS`attributes];*)
    (*RS`universe = $X[[domain, $R[[i]] ~ Join ~ {j} ]];*)
    (*lr = Length[$R[[i]]];*)
    (*If[ lr <= 1, Return[$FailCompleteSymbol]];*)
    (*RS`attributes = Range[lr];*)
    (*RS`conditions = Range[lr - 1];*)
    (*RS`Base = RS`conditions;*)
    (*RS`decisions = {lr};*)
  (*];*)

  (*trainingset = $X[[domain, $R[[i]]]] -> $X[[domain, j]];*)
  (*Return[Classify[trainingset, goal, Method -> $method ]];*)
  Return@Missing[];
];

End[];

(*`Private`*)



EndPackage[];