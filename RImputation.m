(* ::Package:: *)

(* :Title: Imputation                                                         *)
(* :Context: Imputation`                                                      *)
(* :Author: Jonathan Prieto-Cubides                                           *)
(* :Date: 2016-11-07                                                          *)
(* :Package Version: 0.2                                                      *)
(* :Mathematica Version:                                                      *)
(* :Copyright: (c) 2016-2018 Jonathan Prieto-Cubides                          *)
(* :Keywords: Categorical, Classification, Imputation, Missing Values         *)
(* :Discussion: Dealing with Missing Data using a Selection Hybrid Algorithm  *)

BeginPackage["RImputation`"];

ImputeVersion::usage = "ImputateVersion";
ImputeVersion        = "Imputation Package v0.2";

ROUSTIDA::usage   = "ROUSTIDA algorithm implementation";
VTRIDA::usage     = "VTRIDA algorithm implemenation";
HSI::usage        = "HSI algorithm implementation";
Impute::usage     = "Impute function";
RunAlgorithm::usage   = "RunAlgorithm[]";
MeanCompleter::usage  = "Mean completer implementation";
checkMatches::usage   = "Check for the accuracy ratio";
SetMissings::usage    = "Set randomly missing data value in a dataset";
FillWith::usage       = "Handy method to fill a position with a set value";
UpdateData::usage     = "Update every container after changed i,k";

$oldU::usage = "original dataset";
$U::usage    = "dataset";
Preprocessing::usage = "";
$missingU::usage     = "dataset with random missing values";

$V::usage     = "Number of possible values that an attribute can take";
$MOS::usage   = "Missing object set in the dataset";
$MAS::usage   = "Missing attribute set per each row";
$NS::usage    = "Set of no distinguishible object with a row";
$GM::usage    = "Generalized descirnibility matrix";
$Mlv::usage   = "Value tolerance matrix";
$OMS::usage   = "object missing set per an attribute k";
$verboseOutcome::usage  = "output data container of RunAlgorithm method";
$missingRate::usage     = "Missing rate for the SetMissings method";

Off[AbortAssert];
AbortAssert::trace = "Assertion failed ``.";
AbortAssert /: On[AbortAssert]  := On[AbortAssert::trace];
AbortAssert /: Off[AbortAssert] := Off[AbortAssert::trace];
SetAttributes[AbortAssert, {HoldFirst}];
AbortAssert[test_, message__] :=
    Check[TrueQ[test] || Message[AbortAssert::trace, {HoldForm[test], message}],
      Abort[]];

(* -------------------------------------------------------------------------- *)

$FailCompleteSymbol = Missing["Failure"];
$FailCompleteSymbolPrint =
"\!\(\* StyleBox[\"*\",\nFontSize->24,\nBackground->RGBColor[1, 0.5, 0.5]]\)";

$oldU             = {};
$U                = {};
$missingU         = {};
$algorithm        = "ROUSTIDA";
$numberIterations = 20;
$missingRate      = 0.05;
$missingSymbol    = Missing[];
$missingSymbolPrint =
"\!\(\* StyleBox[\"?\",\nFontSize->18,\nBackground->RGBColor[1, 1, 0]]\)";


(* -------------------------------------------------------------------------- *)

Clear[SetInitValues]
SetInitValues[] := Module[{},
  $oldU               = {};
  $missingU           = {};
  $U                  = {};
  $numMissings        = 0;
  $numIncompleteRows  = 0;
  Clear[$MOS, $MAS, $V, $OMS, $GM, $Mlv, $NS];
];
SetInitValues[];

(* -------------------------------------------------------------------------- *)

Clear[PrintData];
PrintData[ex_] := Module[{nX, n, m},
  {n, m} = Dimensions@ex;
  nX = ex;
  Table[
    nX[[i, j]] = If[ MissingQ[ex[[i, j]]], $missingSymbolPrint , nX[[i, j]]];
    , {i, 1, n}, {j, 1, m}];
  Print[TableForm@nX];
];

(* -------------------------------------------------------------------------- *)

(*http://goo.gl/hMyhjI*)
Clear[Complement2];
Complement2[A_, B_] := DeleteCases[A, Alternatives @@ B];

(* -------------------------------------------------------------------------- *)

Clear[Union2];
Union2[A_, B_] := Union[Flatten[{A, B}]];

(* -------------------------------------------------------------------------- *)

(*http://goo.gl/5O7jhs*)
Clear[MakeArrange];
MakeArrange[{n_, m_}, p_] := Module[
{base = PadLeft[ConstantArray[1, Round[n m p]], n m], cand},
  While[(cand = ArrayReshape[RandomSample@base, {n, m}];
  Max[Total[cand]] > n - 2 || Max[Total /@ cand] > m - 2)];
  Position[cand, 1]
];

(* -------------------------------------------------------------------------- *)

Clear[SetMissings];
SetMissings[database_, per_] := (
  $U = database;
  If[ per > 0 && per < 1,
    $missingRate = per
  ];
  SetMissings[];
);

SetMissings[] := Module[
  {cant, ms, n, m},

  $oldU = $U;
  With[{d = Dimensions@$U},
    AbortAssert[Length@d == 2, "SetMissings"];
    {n, m} = d;
  ];

  AbortAssert[m-1 > 0 && n > 1, "SetMissings"];
  AbortAssert[$missingRate < 0.5 && $missingRate > 0,"Rate out of range"];

  cant  = Ceiling[n * (m-1) * $missingRate];
  ms    = MakeArrange[{n, m-1}, $missingRate];
  Table[
    $U[[pos[[1]], pos[[2]]]] = $missingSymbol
  , {pos, ms}];
  $numMissings = Length[ms];
  $missingU    = $U;
  $numMeanCompleter = 0;
  $numAlgo = 0;
  $numCorrectAlgo = 0;
];

(* -------------------------------------------------------------------------- *)

Clear[Preprocessing];
Preprocessing[] := Module[
  {n=0,m=0},

  If[Length@Dimensions[$U] != 2,
    Print["Step One, invalid $U"];
    Return[]
  ];

  {n, m} = Dimensions[$U];

  Clear[$MOS, $MAS, $OMS, $GM, $Mlv, $NS];

  $GM[i_,j_]  := $GM[j,i] /; i > j;
  $GM[_,_]    := {};

  $Mlv[i_,j_] := $Mlv[j,i] /; i > j;
  $Mlv[_,_]   := {};

  Table[ $OMS[k] = {},{k, 1, m}];

  $MOS = {};

  Table[
    $MAS[i] = {};
    $NS[i]  = {};
    Table[
      If[ MissingQ@$U[[i,k]],
        $MAS[i] = $MAS[i]~Join~{k};
        $OMS[k] = $OMS[k]~Join~{i};
      ];
    ,{k, 1, m}];
    If[ Length@$MAS[i] > 0,
      $MOS = $MOS~Join~{i}
    ];
    $Mlv[i,i] = 1;
    $GM[i,i]  = {};
  ,{i, 1, n}];

  Table[
    $Mlv[i,j] = 1;
    $GM[i,j]  = {};
    Table[
      If[ MissingQ@$U[[i,k]],
        If[ MissingQ@$U[[j,k]],
          $Mlv[i,j] = $Mlv[i,j] / ($V[k]^2);
        , $Mlv[i,j] = $Mlv[i,j] / $V[k];
        ];
     , If[ MissingQ@$U[[j,k]],
          $Mlv[i,j] = $Mlv[i,j] / $V[k];
       ,  If[ $U[[i,k]] != $U[[j,k]],
            $Mlv[i,j] = 0;
            $GM[i,j]  = $GM[i,j]~Join~{k};
         ,  $Mlv[i,j] = $Mlv[i,j]*1;
         ];
       ];
     ];
  ,{k, 1, m}];

  If[ Length@$GM[i,j] == 0,
    $NS[i]  = $NS[i]~Join~{j};
  ];

  ,{i, 1, n}, {j, i+1, n}];

];

(* -------------------------------------------------------------------------- *)
(* TODO update and changed variables *)

Clear[ROUSTIDA];
ROUSTIDA[] := Module[
  {condition, n,m, flag, changed, val},

  If[ Length@Dimensions@$U != 2,
    Print["Step Two. Invalid $U."];
    Return[]
  ];

  {n,m} = Dimensions[$U];
  flag  = False;

  Table[
    changed = False;
    Which[
      Length@$NS[i] == 1,
        With[{j = $NS[i][[1]]},
          changed = FillWith[i, k, $U[[j,k]]];
          flag    = Or[flag, changed];
        ];
    , Length@$NS[i] >= 2,
        condition = False; (* a flag for check inconsistences *)
        For[j0 = 1, j0 <= Length@$NS[i] && condition, j0++,
          For[j1 = j0+1, j1 <= Length@$NS[i] && condition, j1++,
            If[  !MissingQ@$U[[ $NS[i][[j0]], k]]
              && !MissingQ@$U[[ $NS[i][[j1]], k]]
              && $U[[ $NS[i][[j0]], k]] !=  $U[[ $NS[i][[j1]], k]],
                condition = True;
            ];
          ];
        ];

        If[ !condition,
          For[jj = 1, jj <= Length@$NS[i] && !changed, jj++,
            val     = $U[[ $NS[i][[jj]], k ]];
            changed = FillWith[i, k, $U[[ $NS[i][[jj]], k]] ];
            flag    = Or[flag, changed];
           ];
        ];
    ];

  ,{i, $MOS}, {k, $MAS[i]}];

  If[ flag,
    ROUSTIDA[];
  , MeanCompleter[];
  ];
];

(* -------------------------------------------------------------------------- *)

Clear[VTRIDA];
VTRIDA[] := Module[
  {valMaxJ, maxJ, n, m, condition, i, j, flag, changed},

  If[ Length@Dimensions@$U != 2,
    Print["Step Two. Invalid $U."];
    Return[];
  ];

  {n,m} = Dimensions[$U];
  flag  = False;

  Table[
    changed   = False;
    (* try to find the j such it has the largest $Mlv[i,j]$ *)
    maxJ      = -1;
    valMaxJ   = 0;  (* this assures that $Mlv[i,j] > 0 *)
    For[j = 1, j <= n && !condition, j++,
      If[ i != j,
        If[ valMaxJ < $Mlv[i,j],
            valMaxJ = $Mlv[i,j];
            maxJ    = j;
        ];
      ];
    ];

    If[ maxJ > 0, (* this condition is suffice for a valid j T(i,j)>0 *)
      Table[
        changed  = FillWith[i, k, $U[[i, maxJ]] ];
        flag     = Or[flag, changed];
      ,{k, $MAS[i]}];
    ];

  ,{i, $MOS}];

  If[ flag,
    VTRIDA[]
  , MeanCompleter[]
  ];
];

(* -------------------------------------------------------------------------- *)

Clear[HSI];
HSI[] := Module[
  {answers, rows, cols, rangeN, rangeM, base, model, clasifier, ans, goal, flag
    ,rowsAll, changed, classes},

  If[ Length@$MOS == 0,
    Return[];
  ];

  If[ Length@Dimensions@$U != 2,
    Print["Step Two. Invalid $U."];
    Return[]
  ];

  {n,m}   = Dimensions[$U];
  flag    = False;
  rangeN  = Range[n];
  rangeM  = Range[m];
  $MOS    = SortBy[$MOS, Length@$MAS[#] &];

  Table[

    $MAS[i] = SortBy[$MAS[i], Length@$OMS[#] &];
    changed = False;

    Table[
      If[ Length@$NS[i] == 1,
        With[{  j  = $NS[i][[1]]},
          changed  = FillWith[i, k, $U[[j, k]] ];
          flag     = Or[flag, changed];
        ];
      ];
    , {k, $MAS[i]}];

    Table[

      answers = DeleteCases[Union@$U[[All, k]], Missing[]];
      If[ Length@answers == 1,
        changed = FillWith[i, k, answers[[1]] ];
        flag    = Or[flag, changed];
      ];
      (* if there is not an obvious answer, next step *)
      If[!changed,

        base = Complement2[rangeM, $MAS[i]~Join~{m}];

        (* rows with values at k-col*)
        rowsAll = Complement2[rangeN, $OMS[k]];
        rows    = Select[rowsAll, Intersection[$MAS[#], base] == {}  &];

        If[ Length@rows == 1,
            changed = FillWith[i, k, $U[[ rows[[1]], k ]] ];
            flag    = Or[flag, changed];
        ];

        If[ !changed && Length@rows > 1,

          If[ Length@rows >= 0.10*n,
            rows = SortBy[rows, - $Mlv[i,#] &];
            rows = Take[rows, UpTo@Ceiling[0.05*n]];
          ];
          AbortAssert[Length@rows > 1, "ClassifyReducedModel"];

          (* cols with values in the i-row *)
          cols  = Complement2[rangeM, $MAS[i]];
          If[ Length@cols >= 0.8*m,
            cols = SortBy[cols, Length@$OMS[#] &];
            cols = Take[cols, UpTo@Ceiling[0.8*m]];
          ];
          model = $U[[rows, cols]] -> $U[[rows, k]];

          classes = Union@DeleteCases[$U[[rows, k]], Missing[]];

          (*Print["Model "];
          PrintData[$U[[rows, cols]]];
          Print["classes:"];
          Print[classes];*)

          If[ Length@clases == 1,
            change = FillWith[i, j, classes[[1]] ];
            flag = Or[flag, change];
          ];

          If[ !change && Length@classes > 1,
            clasifier = Classify[model,
                Method          -> "NaiveBayes"
              , PerformanceGoal -> "Quality"
            ];

            goal  = $U[[i, cols]];
            ans   = clasifier[goal];

            If[ MemberQ[answers, ans],
              changed  = FillWith[i, k, ans];
              flag     = Or[flag, changed];
            ];
          ];
        ];
      ];
    , {k, $MAS[i]}];
  , {i, $MOS}];

  If[ flag,
     HSI[]
  ,  MeanCompleter[]
  ];
];

(* -------------------------------------------------------------------------- *)

Clear[FillWith];
FillWith[i_Integer, k_Integer, val_] := Module[
  {},
  If[ !MissingQ@val,
    $U[[i, k]]  = val;
    If[ val == $oldU[[i,k]],
      $numCorrectAlgo++;
    ];
    If[ $numMissings > 0,
      --$numMissings
    ];
    $numAlgo++;
    UpdateData[i,k];
    Return[True];
  ];
  Return[False];
];

(* -------------------------------------------------------------------------- *)

(* This method supposes that $U[i,k] has been imputed *)
Clear[UpdateData];
UpdateData[i_Integer,k_Integer] := Module[
  {newPkij, oldMlv, oldPkij, n, m},

  {n,m}   = Dimensions[$U];
  $MAS[i] = DeleteCases[$MAS[i], k];

  If[ Length@$MAS[i] == 0,
    $MOS = DeleteCases[$MOS, i];
  ];

  Table[
    If[i != j,
      oldMlv  = $Mlv[i,j];
      oldPkij =
        If[MissingQ@$U[[j, k]],
          1 / $V[k]
        , 1 / $V[k]^2
      ];

      $Mlv[i, j] = 1;
      $GM[i, j]  = DeleteCases[$GM[i,j], k]; (* we first remove, and the check *)

      newPkij =
        If[ $U[[i,k]] == $U[[j, k]],
          If[ MissingQ@$U[[j,k]],
            1 / $V[k]
          , 1
          ]
        , If[ MissingQ@$U[[j,k]],
            1 / $V[k]
          , 0
          ]
      ];

      If[ $U[[i,k]]!=$U[[j,k]] && !MissingQ@$U[[i,k]] && !MissingQ@$U[[j,k]],
         $GM[i,j] = $GM[i,j]~Join~{k};
      ];

      AbortAssert[ oldPkij != 0, {"oldPkij ", oldPkij, i, j, k, " failed" }];

      $Mlv[i,j] = oldMlv * (1/ oldPkij) * newPkij;
      $Mlv[j,i] = $Mlv[i,j];
      $NS[i]    = DeleteCases[$NS[i], j];

      If[ Length@$GM[i,j] == 0,
        $NS[i] = $NS[i]~Join~{j};
      ];
    ];
    , {j, 1, n}];
 ];

(* -------------------------------------------------------------------------- *)

Clear[MeanCompleter];
MeanCompleter[]:= Module[
  {n,m, freq},

  If[Length@Dimensions@$U == 0,
    Print["MeanCompleter. Invalid $U"];
    Return[]
  ];

  {n,m} = Dimensions[$U];

  Table[
    freq = SortBy[Tally@Select[$U[[All, k]],Not@MissingQ[#] & ], Last][[-1,1]];
    Table[
      If[ MemberQ[$MAS[i], k],
        FillWith[i,k, freq];
        --$numAlgo;
        If[ $oldU[[i,k]] == freq,
          --$numCorrectAlgo
        ];
        $numMeanCompleter++;
      ];
    ,{i, $MOS}];
  ,{k, 1, m-1}];
];

(* -------------------------------------------------------------------------- *)
(*
  dataset = <| "name" -> datasetname, "data" -> filepath, "attr"-> filepath |>
  $missingRate = 0.05;
  $numberIterations = 20;
  $algorithm = "HSI";
  RunAlgorithm[dataset, 20, 0.05, "HSI"]
*)

Clear[RunAlgorithm];
RunAlgorithm[dataset_Association] :=
  RunAlgorithm[dataset,$numberIterations,$missingRate, $algorithm];

RunAlgorithm[dataset_Association, numIter_Integer, miss_,algo_String]:=
  RunAlgorithm[{dataset}, numIter, miss, algo]

RunAlgorithm[datasets_List, numIter_Integer:30, miss_, algo_String] := Module[
  {oData, name, citer, outcome = <||>, matches, cDataset = 0,
  res, oldJ, n=0, m=0, numMissing=0, attr, mean, stand, conf},

  SetInitValues[];
  $missingRate = miss;
  AbortAssert[ $missingRate > 0 && $missingRate < 0.5 ];

  $lastResult = "";

  $minResult = Infinity;
  $maxResult = -Infinity;

  PrintTemporary@Dynamic@Dataset[
    <|
      "Algo"               -> algo,
      "No."                -> ToString@(cDataset) <> "/" <> ToString@Length@datasets,
      "Dataset"            -> name,
      "Missing Rate"       -> $missingRate,
      "No. Instances"      -> n,
      "No. Attributes"     -> m,
      "No. Missing Values" -> ($numMissings - 1),
      "No. Algo imputed"   -> $numAlgo,
      "No. Algo correct"   -> $numCorrectAlgo,
      "No. Mean completer" -> $numMeanCompleter,
      "Current Iteration"  -> ToString@citer <> "/" <> ToString@numIter,
      "Last result"        -> ToString@$lastResult,
      "Min. result"        -> $minResult,
      "Max. result"        -> $maxResult
    |>
  ];

  Clear[$verboseOutcome];
  $verboseOutcome = Association@Table[i -> <||>,{i, 1, Length@datasets}];

  cDataset = 1;
  Table[
    name  = dataset["name"];
    oData = Import@dataset["data"];
    attr  = Import@dataset["attr"];
    With[
      {d = Dimensions@oData},
      AbortAssert[Length@d == 2, "RunAlgorithm"];
      {n, m} = d;
      AbortAssert[n > 2 && m > 2, "RunAlgorithm"];
    ];

    $minResult = Infinity;
    $maxResult = -Infinity;

    citer = 0; (* number of iteration currently *)
    res   = Table[
      citer++;
      $U = oData;
      AbortAssert[Length@attr == m,
        "attribute file invalid " <>ToString@m<>" vs "<>ToString@Length@attr
      ];
      Table[$V[k] = attr[[k, 2]], {k, 1, m}];

      SetMissings[];
      Preprocessing[];

      oldJ = Table[$MAS[i], {i, 1, n}];
      numMissing = $numMissings;

      Which[
        algo == "ROUSTIDA", ROUSTIDA[]
      , algo == "VTRIDA", VTRIDA[]
      , algo == "HSI",  HSI[]
      ];

      matches = checkMatches[oldJ];

      With[{stat = N[Total@matches /numMissing]},
        $lastResult = stat;
        $minResult = Min[$minResult, stat];
        $maxResult = Max[$maxResult, stat];
      ];
      $lastResult

    , {numIter}];

    mean  = Mean@res;
    stand = StandardDeviation@res;
    conf  = {mean - 2.01*(stand/Sqrt[numIter]), mean + 2.01*(stand/Sqrt[numIter])};

    Export[FileNameJoin[{dataset[["dir"]], algo<>"-"<>ToString[miss]<>".csv"}],
      { algo
      , name
      , $missingRate
      , numIter
      , res
      , {$minResult, $maxResult}
      , {mean, stand}
      , conf
      }
      , "CSV"];

      $verboseOutcome[[cDataset]] =
        <|  "dataset"     -> name
        ,   "size"        -> ToString@n <> "x" <> ToString@m
        ,   "algo"        -> algo
        ,   "name"        -> name
        ,   "numIter"     -> numIter
        ,   "res"         -> res
        ,   "(min, max)"  -> {$minResult, $maxResult}
        ,   "mean"        -> mean
        ,   "interval"    -> conf
        |>;

  cDataset++;
  , {dataset, datasets}];
  Return@$verboseOutcome;
];

(* -------------------------------------------------------------------------- *)

Clear[checkMatches];
checkMatches[oldJ_List] := Module[
  {matches, n, m},
  If[ Length@Dimensions@$U != 2,
    Print["checkMathes. Invalid $U"];
    Return[0]
  ];
  {n, m} = Dimensions@$U;
  matches = Table[
    Length@oldJ[[i]] - HammingDistance[$U[[i, oldJ[[i]]]], $oldU[[i, oldJ[[i]]]]]
  , {i, n}];
  Return@matches;
];

(* -------------------------------------------------------------------------- *)

EndPackage[];
