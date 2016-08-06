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
$RSImputeVersion::usage = "0.1";

Begin["`Private`"]

Clear[complement]; (*http://goo.gl/hMyhjI*)
complement[A_, B_] := DeleteCases[A, Alternatives @@ B];


Clear[makearr]; (* http://goo.gl/5O7jhs *)
makearr[{n_, m_}, p_] := Module[{base = PadLeft[ConstantArray[1, Round[n m p]], n m], cand},
  While[(cand = ArrayReshape[RandomSample@base, {n, m}];
  Max[Total[cand]] > n - 2 || Max[Total /@ cand] > m - 2)];
  Position[cand, 1]
];


Clear[SetMissings];
SetMissings[database_, per_] := ($X = database; If[ per > 0 && per < 1, $MissingRate = per];SetMissings[]);
SetMissings[] := Module[
  {cant, ms},
  $oldX = $X;
  {$n, $m} = Dimensions@$X;
  $s = $m - 1;
  Assert[$s > 0 && $n > 1];
  Assert[$MissingRate < 1 && $MissingRate > 0];
  $currentTask = "Putting missing values in the dataset";
  cant = Ceiling[$n * $s * $MissingRate];
  ms = makearr[{$n, $s}, $MissingRate];
  Table[
    $X[[pos[[1]], pos[[2]]]] = $MissingSymbol, {pos, ms}];
  $numMissings = Length[ms];
  $MissingX = $X;
];



Clear[Instance];
Instance[numIter_, datasets_, perc_] := Module[
  {original, name, citer, outcome, matches, currentset = 0, resPerMethod, oldJ},
  SetInitValues[];
  outcome = <||>;

  $MissingRate = perc;
  If[ $MissingRate < 0 || $MissingRate > 0.5, Return[]];

  PrintTemporary@Dynamic@Dataset[
    <|
      "name_of_dataset" -> name,
      "No." -> ToString[currentset] <> "/" <> ToString[Length[datasets]],
      "num_instances" -> $n,
      "num_attributes" -> $m,
      "num_missings_values" -> $numMissings,
      "current_method" -> $method,
      "current_iteration" -> ToString[citer] <> "/" <> ToString[numIter],
      "current_incomplete_row" -> ToString[$currentobject] <> "/" <> ToString[$numIncompleteRows]
    |>
  ];

  PrintTemporary@Dynamic@$currentTask;
  currentset = 1;
  Table[
    $currentTask = "Importing dataset data";
    name = dataset["filename"];
    original = Import[dataset["data"]];
    {$n, $m} = Dimensions@original;
    outcome[currentset] = <|"Dataset" -> name, "Size" -> ToString[$n] <> "x" <> ToString[$m]
    |>;

    Table[
      $method = method;
      $currentTask = "Starting... " <> ToString@$method;
      Clear[resPerMethod];
      citer = 0;
      resPerMethod = Table[
        citer++;
        $X = original;
        SetMissings[];
        Step1[];
        oldJ = $J;
        Step2[2];
        $currentTask = "Computing stats...";
        matches = Table[
          Length[oldJ[[i]]] - HammingDistance[$X[[i, oldJ[[i]]]], original[[i, oldJ[[i]]]]]
          , {i, $M}];
        (*Print[matches];*)
        N[Total[matches] / $numMissings]
        , {numIter}];
      outcome[currentset][ToString@method] = NumberForm[Mean[resPerMethod], {3, 2}];
      , {method, $listMethods}];
    currentset++;
    , {dataset, datasets}];

  Return[outcome];
];

Clear[Impute];
Impute[ex_] := Module[
  {mm, yy, jj, rr, ii, sis, mtt, nx, lenmiss, n, m},
  $X = ex;
  Step1[];
  Step2[2];
  Return[$X];
];


Clear[PrintTable];
PrintTable[ex_] := Module[{nX, n, m},
  {n, m} = Dimensions[ex];
  nX = ex;
  Table[
    nX[[i, j]] =
        If[ MissingQ[ex[[i, j]]], $MissingSymbolPrint , nX[[i, j]]];
    , {i, 1, n}, {j, 1, m}];
  Return[nX];
];

Clear[SetValues];
SetValues[] := Module[{},
  $currentobject = 0;
  $currentTask = "";
  $FailCompleteSymbol = Missing["Failure"];
  $FailCompleteSymbolPrint = "\!\(\* StyleBox[\"*\",\nFontSize->24,\nBackground->RGBColor[1, 0.5, 0.5]]\)";
  $I = {};
  $J = {};
  $listMethods = {"NaiveBayes", "RandomForest"};
  $M = {};
  $methodByDefault = "RandomForest";
  $method = $methodByDefault;
  $MissingRate = 0.1;
  $MissingSymbol = Missing[];
  $MissingSymbolPrint = "\!\(\* StyleBox[\"?\",\nFontSize->18,\nBackground->RGBColor[1, 1, 0]]\)";
  $MissingX = {};
  $n = 0; $m = 0;
  $oldX = {};
  $orderCols = 1;
  $orderRows = -1;
  $R = {};
  $rangen = {}; $rangem = {};
  $s = $m - 1;
  $numIncompleteRows = 0;
  $numMissings = 0;
  $X = {}; (* at the end, this is the new version of dataset after Impute*)
  $Y = {};
];
SetValues[];


Clear[SetInitValues]
SetInitValues[] := Module[{},
  $X = {}; (* at the end, this is the new version of dataset after Impute*)
  $oldX = {};  (* saves the dataset original *)
  $MissingX = {}; (* saves the missing holes *)
  $n = 0; $m = 0; (* {n,m} = Dimensions[X] *)
  $rangen = {}; $rangem = {};
  $s = $m - 1;

  $M = {};
  $Y = {};
  $I = {};
  $J = {};
  $R = {};

  $numMissings = 0;
  $numIncompleteRows = 0;
  $currentobject = 0;
  $currentTask = "";
];


Clear[Step1];
Step1[] := Module[{},
  $currentTask = "Initialization... Step1.";
  {$n, $m} = Dimensions[$X];
  $s = $m - 1;
  $J = Table[{}, {$n}]; $R = Table[{$m}, {$n}];
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
      , {j, 1, $s}];
    If[ Length@$J[[i]] > 0, $M = {$M, {i}} ];
    , {i, 1, $n}];

  (* optional trick: flatten all unions *)
  Table[
    $J[[i]] = Union@Flatten[$J[[i]], Infinity];
    $R[[i]] = Union@Flatten[$R[[i]], Infinity];
    , {i, 1, $n}];
  Table[
    $I[[j]] = Flatten[$I[[j]], Infinity]
    , {j, 1, $s}];

  $M = Union@Flatten[$M, Infinity];
  $numIncompleteRows = Length@$M;
  $Y = complement[$rangen, $M];

  AbortAssert[Length@$M > 0];
  Table[
    AbortAssert[Length@$I[[j]] < $n];
    , {j, $m}];
  Table[
    AbortAssert[Length@$J[[i]] <= ($m - 2)];
    AbortAssert[Length@$R[[i]] >= 2];
    , {i, $n}];
];


Clear[RSImpute`Step2];
RSImpute`Step2[lap_] := Module[{},

  $currentTask = "Imputation Phase... (" <> ToString[lap] <> ")";
  $currentobject = 0; (* current_object*)
  AbortAssert[ lap > 0];
  If[ $numIncompleteRows == 0, Return[]];
  $M = SortBy[$M, $orderRows * Length@$J[[#]] &];
  Table[
    $currentobject++;
    $J[[i]] = SortBy[$J[[i]], $orderCols * Length@$I[[#]] &];
    Table[
      AbortAssert[ Length@$R[[i]] > 1];
      $X[[i, j]] = RSImpute`BlackBox[i, j];
      If[ Not@MissingQ[$X[[i, j]]],
        $R[[i]] = $R[[i]] ~ Join ~ {j};
        $I[[j]] = complement[$I[[j]], {i}];
      ];
      , {j, $J[[i]]}];
    If[ Length@$R[[i]] == $m,
      $Y = $Y ~ Join ~ {i}; $J[[i]] = {},
      $J[[i]] = complement[$rangem, $R[[i]]];
    ];
    , {i, $M}];

  $M = complement[$M, $Y];
  If[ Length@$M > 0 && lap > 0,
    RSImpute`Step2[lap - 1]
  ];
];


Clear[union2];
union2[A_, B_] := Union[Flatten[{A, B}]];


Clear[RSImpute`BlackBox]
RSImpute`BlackBox[i_, j_] := Module[
  {goal, classes, classes2, trainingset, goods, refined},
  $currentTask = "Reduce-model imputation BlackBox[i,j] ";
  AbortAssert[Length@$R[[i]] >= 2];
  goal = $X[[ i, $R[[i]] ]];
  goods = complement[$rangen, $I[[j]]];
  (*Assert[Length@goods >= 1];*)
  If[ Length@goods == 0,
    Return[ $FailCompleteSymbol];
  ]; (* en principio no deberia pasar nunca *)

  classes = Union@$X[[goods, j]];
  If[ Length@classes == 1,
    Return[First@classes]
  ];
  refined = Select[goods, Intersection[$J[[#]], $R[[i]]] == {} &];
  classes2 = Union@$X[[ refined, j]];
  (* hot-deck reduce-model *)
  Which[
    Length@refined == 0, domain = goods,
    Length@refined == 1, Return[First@classes],
    Length@classes2 == 1, Return[First@classes],
    Length@refined > Ceiling[$n * 0.1], domain = refined;
    ,
    True,
    Return[ $FailCompleteSymbol]
  (*domain = goods *)
  ];
  If[ $method == "RWNB",
    Clear["RS`*"];
    << RS`;
    Clear[RS`universe, RS`attributes];
    RS`universe = $X[[domain, $R[[i]] ~ Join ~ {j} ]];
    lr = Length[$R[[i]]];
    If[ lr <= 1, Return[$FailCompleteSymbol]];
    RS`attributes = Range[lr];
    RS`conditions = Range[lr - 1];
    RS`Base = RS`conditions;
    RS`decisions = {lr};
  ];

  trainingset = $X[[domain, $R[[i]]]] -> $X[[domain, j]];
  Return[
    Classify[trainingset, goal,
      Method -> $method
    ];
  ];

  End[] (* `Private` *)

  On[AbortAssert];
  AbortAssert::trace = "Assertion `` failed.";
  AbortAssert /: On[AbortAssert] := On[AbortAssert::trace];
  AbortAssert /: Off[AbortAssert] := Off[AbortAssert::trace];
  SetAttributes[AbortAssert, {HoldFirst}];
  AbortAssert[test_] :=
      Check[TrueQ[test] || Message[AbortAssert::trace, HoldForm[test]],
        Abort[]]

  EndPackage[]