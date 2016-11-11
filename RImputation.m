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

$classifier       = "NaiveBayes";
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
  $numMeanCompleter   = 0;
  $numAlgo            = 0;
  $numCorrectAlgo     = 0;
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
  {n=0,m=0, symetric = True, flag, equals},

  If[Length@Dimensions[$U] != 2,
    Print["Step One, invalid $U"];
    Return[]
  ];

  {n, m} = Dimensions[$U];

  Clear[$MOS, $MAS, $OMS, $GM, $Mlv, $NS, $S, $R];

  $MAS[_] := {};
  $NS[_]  := {};
  $R[_]   := {};
  $OMS[_] := {};

  $GM[i_,j_]  := $GM[j,i] /; i > j;
  $GM[i_,j_]  := {} /; i == j;
  $GM[_,_]    := {};

  $Mlv[i_,j_] := $Mlv[j,i] /; i > j;
  $Mlv[i_,j_] := 1 /; i == j;
  $Mlv[_,_]   := 0;

  (*$S[i_, j_]  := True ;/ i == j;
  $S[_, _]    := False;*)

  Table[If[ i != j,
    $Mlv[i,j] = 1;
    symetric = True;
    Table[
      If[ MissingQ@$U[[i,k]],
        If[ MissingQ@$U[[j,k]],
          $Mlv[i,j] = $Mlv[i,j] / $V[k]^2;
        , $Mlv[i,j] = $Mlv[i,j] / $V[k];
        ];
     , (* $U[[i,k]] != "*" *)
      equals = SameQ[$U[[i,k]],$U[[j,k]]];
      symetric = And[symetric, equals];
       If[ MissingQ@$U[[j,k]],
          $Mlv[i,j]  = $Mlv[i,j] / $V[k];
       ,  If[ !equals,
            $Mlv[i,j] = 0;
            $GM[i,j]  = {$GM[i,j],k};
          ];
       ];
     ];
  ,{k, 1, m}];

  If[ symetric,
    (*$S[i,j] = True;*)
    $R[j]   = {$R[j],i};
  ];

  $GM[i,j] = Union@Flatten[$GM[i,j], Infinity];

  If[ Length@$GM[i,j] == 0,
    $NS[i]  = {$NS[i], j};
    $NS[j]  = {$NS[j], i};
  ];

  ],{i, 1, n}, {j, 1, n}];

  (* simplification of the data containers *)

  $MOS = {};
  Table[
    flag = False;
    Table[
      If[ MissingQ@$U[[i,k]],
        $MAS[i] = {$MAS[i],k};
        $OMS[k] = {$OMS[k],i};
        flag = True;
      ];
    ,{k, 1, m}];

    If[ flag, (* Length@$MAS[i] > 0, *)
      $MOS = {$MOS,i};
    ];

    $MAS[i] = Union@Flatten[$MAS[i], Infinity];
    $R[i]   = Union@Flatten[$R[i], Infinity];
    $NS[i]  = Union@Flatten[$NS[i], Infinity];
  ,{i, 1, n}];

  $MOS = Flatten[$MOS, Infinity];

  Table[
    $OMS[k] = Flatten[$OMS[k], Infinity];
  ,{k,1, m}];

];

(* -------------------------------------------------------------------------- *)

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
        condition = False;
        For[j0 = 1, j0 <= Length@$NS[i] && condition, j0++,
          For[j1 = j0+1, j1 <= Length@$NS[i] && condition, j1++,
            If[  !MissingQ@$U[[ $NS[i][[j0]], k]]
              && !MissingQ@$U[[ $NS[i][[j1]], k]]
              && !SameQ[ $U[[ $NS[i][[j0]], k]], $U[[ $NS[i][[j1]], k]] ],
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

Clear[HSI];
HSI[] := Module[
  {answers, rows, cols, rangeN, rangeM, base, model, clasifier, ans, goal, flag
    ,rowsAll, changed, classes, entro =0},

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
  $MOS    = SortBy[$MOS, Length@$MAS[#]*(Length@$R[#]+1) &];

  Table[

    $MAS[i] = SortBy[$MAS[i], Length@$OMS[#]&];
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

        base = Complement2[rangeM, $MAS[i]];
        (* rows with values at k-col*)
        rowsAll = Complement2[rangeN, $OMS[k]];
        rows    = Select[rowsAll, Intersection[$MAS[#], base] == {}  &];

        If[ Length@rows == 1,
            changed = FillWith[i, k, $U[[ rows[[1]], k ]] ];
            flag    = Or[flag, changed];
        ];

        If[ !changed && Length@rows > 1,

          If[ Length@rows >= $missingRate*n,
            rows = SortBy[rows, - $Mlv[i,#] &];
            rows = Take[rows, UpTo@Ceiling[ $missingRate*n ]];
          ];

          If[Length@rows > 1,
            (* cols with values in the i-row *)
            cols  = Complement2[rangeM, $MAS[i]];
            If[ Length@cols >= 0.8*m,
              cols = SortBy[cols, Length@$OMS[#] &];
              cols = Take[cols, UpTo@Ceiling[0.8*m]];
            ];
            model   = $U[[rows, cols]] -> $U[[rows, k]];
            classes = Union@DeleteCases[$U[[rows, k]], Missing[]];

            (*Print["Model "];
            PrintData[$U[[rows, cols]]];
            Print["classes:"];
            Print[classes];*)

            If[ Length@clases == 1,
              change = FillWith[i, j, classes[[1]] ];
              flag   = Or[flag, change];
            ];

            If[ !change && Length@classes > 1,
              clasifier = Classify[model,
                  Method          -> $classifier
                , PerformanceGoal -> "Quality"
              ];

              goal  = $U[[i, cols]];
              ans   = clasifier[goal];

              If[ MemberQ[answers, ans],
                changed  = FillWith[i, k, ans];
                flag     = Or[flag, changed];
                Print["entro", entro++];
              ];
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
    valMaxJ   = -1;  (* this assures that $Mlv[i,j] > 0 *)
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

Clear[FillWith];
FillWith[i_Integer, k_Integer, val_] := Module[
  {change = False},
  change = !SameQ[$U[[i,k]], val];
  If[ change,
    $U[[i, k]]  = val;
    UpdateData[i,k];
    If[ SameQ[ val, $oldU[[i,k]] ],
      $numCorrectAlgo++;
    ];
    If[ $numMissings > 0,
      --$numMissings
    ];
    $numAlgo++;
  ];
  Return[change];
];

(* -------------------------------------------------------------------------- *)

(* This method supposes that $U[i,k] has been imputed *)
Clear[UpdateData];
UpdateData[i_Integer,k_Integer] := Module[
  {newPkij, oldMlv, oldPkij, n, m, oldGM, oldNS, oldR, symetric},

  Print[i,k];

  {n,m}   = Dimensions[$U];

  If[ !MissingQ@$U[[i, k]],
      $MAS[i] = DeleteCases[$MAS[i], k];
    , $MAS[i] = Union[$MAS[i]~Join~{k}];
  ];

  If[ Length@$MAS[i] > 0,
      $MOS = Union[$MOS~Join~{i}]
  ,   $MOS = DeleteCases[$MOS, i];
      $OMS[k] = DeleteCases[$OMS[k], i];
  ];

  Table[If[i != j,

      symetric = And@@Table[
        If[ !MissingQ@$U[[i,kk]],
            SameQ[ $U[[j,kk]], $U[[i, kk]] ]
          , Nothing
        ]
      , {kk,1, m}];

      If[symetric,
        $R[j] = Union[$R[j]~Join~{i}];
      ];

      If[MissingQ@$U[[i, k]],
        $GM[i,j] = DeleteCases[$GM[i,j], k];
        $GM[j,i] = DeleteCases[$GM[j,i], k];
        If[ Length@$GM[i,j] == 0,
          $NS[i] = Union[$NS[i]~Join~{j}];
          $NS[j] = Union[$NS[j]~Join~{i}];
        ];
        If[!MissingQ@$U[[j,k]],
          $R[j] = DeleteCases[$R[j], i];
        ];
      , (* I know here $U[[i,k]] != "*" *)

        equals = SameQ[ $U[[i,k]], $U[[j,k]] ];
        If[ equals,
            $GM[i,j] = DeleteCases[$GM[i,j], k];
            $GM[j,i] = DeleteCases[$GM[j,i], k];
            If[ Length@$GM[i,j] == 0,
              $NS[i] = Union[$NS[i]~Join~{j}];
              $NS[j] = Union[$NS[j]~Join~{i}];
            ];

        ,  $R[j] = DeleteCases[$R[j], i];
           If[ !MissingQ@$U[[j,k]],
              $GM[i,j]  = Union[$GM[i,j]~Join~{k}];
              $GM[j,i]  = Union[$GM[j,i]~Join~{k}];
              $NS[i]    = DeleteCases[$NS[i], j];
              $NS[j]    = DeleteCases[$NS[j], i];
            ];
        ];

      ];
    ], {j, 1, n}];

    Table[
      $NS[i]  = Union@$NS[i];
      $MAS[i] = Union@$MAS[i];
      $R[i]   = Union@$R[i];
    ,{j, 1, n}];
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
      Print[ToString@cDataset<>" Algo: "<>ToString@$numCorrectAlgo<>"/"<>ToString@$numAlgo<>" + "<>ToString@$numMeanCompleter];

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

(* TEST --------------------------------------------------------------------- *)
(* Example taken from the Stefanowski Paper                                   *)

$e21 = {
   {3, 2, 1, 0},
   {2, 3, 2, 0},
   {2, 3, 2, 0},
   {Missing[], 2, Missing[], 1},
   {Missing[], 2, Missing[], 1},
   {2, 3, 2, 1},
   {3, Missing[], Missing[], 3},
   {Missing[], 0, 0, Missing[]},
   {3, 2, 1, 3},
   {1, Missing[], Missing[], Missing[]},
   {Missing[], 2, Missing[], Missing[]},
   {3, 2, 1, Missing[]}
 };

$mlvE21 =   {
    {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/64, 1/4}
  , {0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}
  , {0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}
  , {0, 0, 0, 1, 1/256, 0, 0, 0, 0, 1/1024, 1/1024, 1/64}
  , {0, 0, 0, 1/256, 1, 0, 0, 0, 0, 1/1024, 1/1024, 1/64}
  , {0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0}
  , {0, 0, 0, 0, 0, 0, 1, 1/256, 1/16, 0, 1/1024, 1/64}
  , {0, 0, 0, 0, 0, 0, 1/256, 1, 0, 1/1024, 0, 0}
  , {0, 0, 0, 0, 0, 0, 1/16, 0, 1, 0, 1/64, 1/4}
  , {0, 0, 0, 1/1024, 1/1024, 0, 0, 1/1024, 0, 1, 1/4096, 0}
  , {1/64, 0, 0, 1/1024, 1/1024, 0, 1/1024, 0, 1/64, 1/4096, 1, 1/256}
  , {1/4, 0, 0, 1/64, 1/64, 0, 1/64, 0, 1/4, 0, 1/256, 1}
};


$gmE21[i_,j_] := $gmE21[j,i] /; i > j;
$gmE21[_,_]   := {};
$gmE21[1,2]   = {1,2,3};
$gmE21[1,3]   = {1,2,3};
$gmE21[1,4]   = {4};
$gmE21[1,5]   = {4};
$gmE21[1,6]   = {1,2,3,4};
$gmE21[1,7]   = {4};
$gmE21[1,8]   = {2,3};
$gmE21[1,9]   = {4};
$gmE21[1,10]  = {1};
$gmE21[1,11]  = {};
$gmE21[1,12]  = {};
$gmE21[2,3]   = {};
$gmE21[2,4]   = {2,4};
$gmE21[2,5]   = {2,4};
$gmE21[2,6]   = {4};
$gmE21[2,7]   = {1,4};
$gmE21[2,8]   = {2,3};
$gmE21[2,9]   = {1,2,3,4};
$gmE21[2,10]  = {1};
$gmE21[2,11]  = {2};
$gmE21[2,12]  = {1,2,3};
$gmE21[3,4]   = {2,4};
$gmE21[3,5]   = {2,4};
$gmE21[3,6]   = {4};
$gmE21[3,7]   = {1,4};
$gmE21[3,8]   = {2,3};
$gmE21[3,9]   = {1,2,3,4};
$gmE21[3,10]  = {1};
$gmE21[3,11]  = {2};
$gmE21[3,12]  = {1,2,3};
$gmE21[4,5]   = {};
$gmE21[4,6]   = {2};
$gmE21[4,7]   = {4};
$gmE21[4,8]   = {2};
$gmE21[4,9]   = {4};
$gmE21[4,10]  = {};
$gmE21[4,11]  = {};
$gmE21[4,12]  = {};
$gmE21[5,6]   = {2};
$gmE21[5,7]   = {4};
$gmE21[5,8]   = {2};
$gmE21[5,9]   = {4};
$gmE21[5,10]  = {};
$gmE21[5,11]  = {};
$gmE21[5,12]  = {};
$gmE21[6,7]   = {1,4};
$gmE21[6,8]   = {2,3};
$gmE21[6,9]   = {1,2,3,4};
$gmE21[6,10]  = {1};
$gmE21[6,11]  = {2};
$gmE21[6,12]  = {1,2,3};
$gmE21[7,8]   = {};
$gmE21[7,9]   = {};
$gmE21[7,10]  = {1};
$gmE21[7,11]  = {};
$gmE21[7,12]  = {};
$gmE21[8,9]   = {2,3};
$gmE21[8,10]  = {};
$gmE21[8,11]  = {2};
$gmE21[8,12]  = {2,3};
$gmE21[9,10]  = {1};
$gmE21[9,11]  = {};
$gmE21[9,12]  = {};
$gmE21[10,11] = {};
$gmE21[10,12] = {1};
$gmE21[11,12] = {};

{$n, $m} = Dimensions[$e21];
Table[
  $V[k] = 4;
  , {k, 1, 4}
  ];

$U = $oldU = $e21;
Preprocessing[];

$reportE21 = {
    VerificationTest[$MOS, {4,5,7,8,10,11,12}]
  , VerificationTest[$OMS[1], {4,5,8,11}]
  , VerificationTest[$OMS[2], {7,10}]
  , VerificationTest[$OMS[3], {4,5,7,10,11}]
  , VerificationTest[$OMS[4], {8,10,11,12}]
  , VerificationTest[$MAS[1], {}]
  , VerificationTest[$MAS[2], {}]
  , VerificationTest[$MAS[3], {}]
  , VerificationTest[$MAS[4], {1,3}]
  , VerificationTest[$MAS[5], {1,3}]
  , VerificationTest[$MAS[6], {}]
  , VerificationTest[$MAS[7], {2,3}]
  , VerificationTest[$MAS[8], {1,4}]
  , VerificationTest[$MAS[9], {}]
  , VerificationTest[$MAS[10], {2,3,4}]
  , VerificationTest[$MAS[11], {1,3,4}]
  , VerificationTest[$MAS[12], {4}]
  , VerificationTest[And@@Flatten@Table[$GM[i,j]==$GM[j,i],{i,1,$n},{j,1,$n}], True]
  , VerificationTest[$NS[1], {11,12}]
  , VerificationTest[$NS[2], {3}]
  , VerificationTest[$NS[3], {2}]
  , VerificationTest[$NS[4], {5,10,11,12}]
  , VerificationTest[$NS[5], {4,10,11,12}]
  , VerificationTest[$NS[6], {}]
  , VerificationTest[$NS[7], {8, 9, 11, 12}]
  , VerificationTest[$NS[8], {7,10}]
  , VerificationTest[$NS[9], {7,11, 12}]
  , VerificationTest[$NS[10], {4,5,8,11}]
  , VerificationTest[$NS[11], {1,4,5,7,9,10,12}]
  , VerificationTest[$NS[12], {1,4,5,7,9,11}]
  , VerificationTest[$R[1], {11,12}]
  , VerificationTest[$R[2], {3}]
  , VerificationTest[$R[3], {2}]
  , VerificationTest[$R[4], {5,11}]
  , VerificationTest[$R[5], {4, 11}]
  , VerificationTest[$R[6], {}]
  , VerificationTest[$R[7], {}]
  , VerificationTest[$R[8], {}]
  , VerificationTest[$R[9], {7, 11, 12}]
  , VerificationTest[$R[10], {}]
  , VerificationTest[$R[11], {}]
  , VerificationTest[$R[12], {11}]
  }~Join~Flatten@Table[
      VerificationTest[ {{i,j}, $Mlv[i,j]}, {{i,j},$mlvE21[[i,j]]} ]
    ,{i,1,$n},{j,1,$n}]~Join~Flatten@Table[
        VerificationTest[ {{i,j}, $GM[i,j]}, {{i,j},$gmE21[i,j]} ]
      ,{i,1,$n},{j,1,$n}];

Table[
  $oldNS[i]   = $NS[i];
  $oldR[i]    = $R[i];
  $oldMAS[i]  = $MAS[i];
,{i, 1, $n}];

$e21 = {
 {3, 2, 1, 0},
 {2, 3, 2, 0},
 {2, 3, 2, 0},
 {3, 2, Missing[], 1},
 {Missing[], 2, Missing[], 1},
 {2, 3, 2, 1},
 {3, Missing[], Missing[], 3},
 {Missing[], 0, 0, Missing[]},
 {3, 2, 1, 3},
 {1, Missing[], Missing[], Missing[]},
 {Missing[], 2, Missing[], Missing[]},
 {3, 2, 1, Missing[]}
};

$U = $oldU = $e21;
Preprocessing[];
(*PrintData[$e21];*)
$change = FillWith[4,1, Missing[]];
(*Print["after"];*)
(*PrintData[$e21];*)


$reportE21 = $reportE21~Join~Flatten@Table[
{ VerificationTest[{i, $NS[i]},{i, $oldNS[i]}]
, VerificationTest[{i, $R[i]}, {i, $oldR[i]}]
, VerificationTest[{i, $MAS[i]}, {i, $oldMAS[i]}]
}
,{i,1, $n}]~Join~Flatten@Table[
  VerificationTest[ {{i,j}, $GM[i,j]}, {{i,j},$gmE21[i,j]} ]
,{i,1,$n},{j,1,$n}];

$reportE21 = TestReport[$reportE21];
Print[$reportE21];
Print[Column /@ (Normal /@ $reportE21["TestsFailed"]) // TabView];
Print[$reportE21["TimeElapsed"]];

(* END of Tests ------------------------------------------------------------- *)


EndPackage[];
