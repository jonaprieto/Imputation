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

BeginPackage["Imputation2`"];



ImputeVersion::usage = "ImputateVersion";
TestAlgorithm::usage = "TestAlgorithm[]";
Impute::usage = "Impute[]";
checkMatches::usage = "";
SetMissings::usage = "";

$U::usage = "dataset";
$oldU::usage = "original dataset";
$missingU::usage = "dataset with random missing values";
StepOne::usage = "";
StepTwo::usage = "";

$V::usage = "";
$MOS::usage = "";
$MAS::usage = "";
$NS::usage = "";
$GM::usage = "";
$Mlv::usage = "";
$OMS::usage = "";
$outcome::usage = "";
$verboseOutcome::usage = "";
$missingRate::usage ="";



Off[AbortAssert];
AbortAssert::trace = "Assertion failed ``.";
(*AbortAssert /: On[AbortAssert] := On[AbortAssert::trace];*)
(*AbortAssert /: Off[AbortAssert] := Off[AbortAssert::trace];*)
SetAttributes[AbortAssert, {HoldFirst}];
AbortAssert[test_, message__] :=
    Check[TrueQ[test] || Message[AbortAssert::trace, {HoldForm[test], message}],
      Abort[]];



ImputeVersion = "Imputation Package v0.2";


$FailCompleteSymbol = Missing["Failure"];
$FailCompleteSymbolPrint = "\!\(\* StyleBox[\"*\",\nFontSize->24,\nBackground->RGBColor[1, 0.5, 0.5]]\)";

$missingRate = 0.05;
$missingSymbol = Missing[];
$missingSymbolPrint = "\!\(\* StyleBox[\"?\",\nFontSize->18,\nBackground->RGBColor[1, 1, 0]]\)";
$missingU = {};
$oldU = {};
$U = {};


Clear[SetInitValues]
SetInitValues[] := Module[{},
  $U = {};
  $oldU = {};
  $missingU = {};

  Clear[$MOS, $MAS, $V, $OMS, $GM, $Mlv, $NS];

  $numMissings = 0;
  $numIncompleteRows = 0;
];
SetInitValues[];


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
  $U = database;
  If[ per > 0 && per < 1, $missingRate = per];
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
  AbortAssert[$missingRate < 1 && $missingRate > 0, "SetMissings"];

  cant = Ceiling[n * (m-1) * $missingRate];
  ms = MakeArrange[{n, m-1}, $missingRate];
  Table[
    $U[[pos[[1]], pos[[2]]]] = $missingSymbol, {pos, ms}];
  $numMissings = Length[ms];
  $missingU = $U;
];



Clear[TestAlgorithm];
TestAlgorithm[dataset_Association, numIter_Integer, miss_,algo_String]:= TestAlgorithm[{dataset}, numIter, miss, algo]
TestAlgorithm[datasets_List, numIter_Integer:30, miss_, algo_String] := Module[
  {oData, name, citer, outcome = <||>, matches, cDataset = 0,
  res, oldJ, n=0,m=0, numMissing=0, attr, mean, stand, conf},

  SetInitValues[];
  $missingRate = miss;
  AbortAssert[ $missingRate > 0 && $missingRate < 0.5 ];

  $lastResult = "";

  $minResult = Infinity;
  $maxResult = -Infinity;

  PrintTemporary@Dynamic@Dataset[
    <|
      "Algo" -> algo,
      "No." -> ToString@(cDataset) <> "/" <> ToString@Length@datasets,
      "Dataset" -> name,
      "Missing Rate" -> $missingRate,
      "No. Instances" -> n,
      "No. Attributes" -> m,
      "No. Missing Values" -> ($numMissings - 1),
      "Current Iteration" -> ToString@citer <> "/" <> ToString@numIter,
      "Last result" -> ToString@$lastResult,
      "Min. result" -> $minResult,
      "Max. result" -> $maxResult
    |>
  ];

  Clear[$outcome, $verboseOutcome];
  $outcome = Association@Table[i-> <||>,{i, 1, Length@datasets}];
  $verboseOutcome =  Association@Table[i-> <||>,{i, 1, Length@datasets}];

  cDataset = 1;
  Table[
    name = dataset["name"];
   
    oData = Import@dataset["data"];
     attr = Import@dataset["attr"];
    
    With[
      {d = Dimensions@oData},
      AbortAssert[Length@d == 2, "TestAlgorithm"];
      {n, m} = d;
      AbortAssert[n > 2 && m > 2, "TestAlgorithm"];
    ];

    $minResult = Infinity;
    $maxResult = -Infinity;

      citer = 0;
      res = Table[
        citer++;
        $U = oData;
        AbortAssert[Length@attr == m, "attribute file invalid " <>ToString@m<>" vs "<>ToString@Length@attr];
        Table[$V[k] = attr[[k]], {k, 1, m}];
        SetMissings[];
        StepOne[];
        oldJ = Table[$MAS[i], {i, 1, n}];
        numMissing = $numMissings;
  
        Which[
           algo=="ROUSTIDA", ROUSTIDA[]
         , algo=="VTRIDA", VTRIDA[]
         ];
        matches = checkMatches[oldJ];

        With[{stat = N[Total@matches /numMissing]},
          $lastResult = stat;
          $minResult = Min[$minResult, stat];
          $maxResult = Max[$maxResult, stat];
        ];
        $lastResult
       , {numIter}];

      $outcome[[cDataset]]= <|
         "Dataset" -> name
      ,  "Size" -> ToString@n <> "x" <> ToString@m
      , "min"-> $minResult
      , "mean" -> NumberForm[Mean@res, {3, 2}]
      , "max" ->  $maxResult
      |>;
      
      mean = Mean@res; stand = StandardDeviation@res;
      conf = {mean - 2.01*(stand/Sqrt[numIter]), mean + 2.01*(stand/Sqrt[numIter])};

      $verboseOutcome[[cDataset]] = <|
          "Dataset" -> name
      ,   "Size" -> ToString@n <> "x" <> ToString@m
      ,   "ConfidenceInt" -> conf
      |>;
   
      Export[FileNameJoin[{dataset[["dir"]], algo<>"-"<>ToString[miss]<>".csv"}],
      { algo
      , name
      , $missingRate
      , numIter
      , res
      , {$minResult, $maxResult}
      , {mean, stand} 
      , conf}
      , "CSV"];
 
     cDataset++;
    , {dataset, datasets}];
    Return[$verboseOutcome];
];



Clear[checkMatches];
checkMatches[oldJ_] := Module[
  {matches, n, m},
  If[ Length@Dimensions@$U != 2, Print["checkMathes. Invalid $U"];Return[0]];
  {n, m} = Dimensions@$U;
  matches = Table[
    Length[oldJ[[i]]] - HammingDistance[$U[[i, oldJ[[i]]]], $oldU[[i, oldJ[[i]]]]]
    , {i, n}];
  Return@matches;
];


Clear[Impute];
Impute[ex_] := Module[
  {},
  $U = ex;
  StepOne[];
  StepTwo[];
  Return@$U;
];


Clear[StepOne];
StepOne[] := Module[
{n=0,m=0},

If[Length@Dimensions[$U] != 2, Print["Step One, invalid $U"]; Return[]];

{n, m} = Dimensions[$U];

Clear[$MOS, $MAS, $OMS, $GM, $Mlv, $NS];

Table[
	$OMS[k] = {};
,{k, 1, m}];

$MOS = {};

Table[
	$MAS[i] = {};
	$NS[i] = {};
	Table[
		If[ MissingQ@$U[[i,k]],
			$MAS[i] = $MAS[i]~Join~{k};
			$OMS[k] = $OMS[k]~Join~{i};
		];
	,{k, 1, m}];
	If[ Length@$MAS[i] > 0, $MOS = $MOS~Join~{i}];
	$Mlv[i,i] = 1;
	$GM[i,i] = {};
,{i, 1, n}];

Table[
$GM[i,j] = {};
$Mlv[i,j] = 1;
Table[
	If[ MissingQ@$U[[i,k]],
		If[  MissingQ@$U[[j,k]],
			(*$P[i,j,k] = 1 / $V[k]]^2;*)
			$Mlv[i,j] = $Mlv[i,j] / ($V[k]^2);
		,   $Mlv[i,j] = $Mlv[i,j] / $V[k];
			(*$P[i,j,k] = 1 / $V[k];*)
		];
	, If[  MissingQ@$U[[j,k]],
		$Mlv[i,j] = $Mlv[i,j] / $V[k];
		(*$P[i,j,k] = 1 / ($V[k]]^2);*)
	,
		If[ $U[[i,k]]!= $U[[j,k]],
			(*$P[i,j,k] = 0;*)
			$Mlv[i,j] = 0;
			$GM[i,j] = $GM[i,j]~Join~{k};
		,
			(*$P[i,j,k] = 1;*)
			$Mlv[i,j] = $Mlv[i,j]*1;
			];
	   ];
    ];
		(*   $P[j,i,k] = $P[i,j, k];*)
,{k, 1, m}];
If[ Length@$GM[i,j] == 0,
	$NS[i] = $NS[i]~Join~{j};
];

$Mlv[j,i] = $Mlv[i,j];
,{i, 1, n}, {j, i+1, n}];
];


Clear[VTRIDA];
VTRIDA[] := Module[
{valMaxJ, maxJ, n,m, flag},
If[ Length@Dimensions@$U != 2, Print["Step Two. Invalid $U."]; Return[]];
{n,m} = Dimensions[$U];

Table[
 (* no hay cambios por el momento *)
Which[
 Length@$NS[i] == 1,
	With[{j = First@$NS[i]},
		Table[
			$U[[i,k]] = $U[[j,k]];
            If[ $numMissings>0, --$numMissings];
			$OMS[k] = DeleteCases[$OMS[k], i];
			flag = True;
		,{k, $MAS[i]}];
	];
	$MAS[i] = {};
	$MOS = DeleteCases[$MOS, i];
	, Length@$NS[i] >= 2 ,

	maxJ = -1;
    valMaxJ = 0;
	Table[
		If[ i != j && !MissingQ[$U[i,j]],
			If[ $Mlv[i,j] > valMaxJ,
				maxJ = j;
				valMaxJ = $Mlv[i,j];
			];
		];
	, {j, $NS[i]}];

	If[ maxJ > 0,
		Table[
			$U[[i,k]] = $U[[maxJ,k]];
            $OMS[k] = DeleteCases[$OMS[k], i];
            flag = True;
            If[ $numMissings>0, --$numMissings];
		,{k, $MAS[i]}];
		
		$MAS[i] = {};
		$MOS = DeleteCases[$MOS, i];
		(* then, update the containers *)
		Table[
			If[i != j,
			$Mlv[i,j] = 1;
			$GM[i,j] = {};

			Table[
				If[ MissingQ@$U[[j,k]],
					(*$P[i,j] = 1 / $V[k];*)
					$Mlv[i,j] = $Mlv[i,j] / $V[k];
				,   $Mlv[i,j] = 0;
					(*$P[i,j] = 0;*)
					$GM[i,j] = $GM[i,j]~Join~{k};
				];
			, {k, 1, m}];
			If[ Length@$GM[i,j] == 0,
				$NS[i] = $NS[i]~Join~{j};
			];
			];
		, {j, 1, n}];
		];
	];
,{i, $MOS}];
If[ flag,
	VTRIDA[]
,   StepThree[]
];
];


Clear[ROUSTIDA];
ROUSTIDA[] := Module[
{condition, n,m, flag },

If[ Length@Dimensions@$U != 2, 
	Print["Step Two. Invalid $U."];
	Return[]
];

{n,m} = Dimensions[$U];
flag = False;

Table[
Which[
	Length@$NS[i] == 1,
	With[{j = $NS[i][[1]]},
		$U[[i,k]] = $U[[j,k]];
        If[ $numMissings > 0, --$numMissings];
	    $MAS[i] = DeleteCases[$MAS[i], k];
	    flag = True;
	];
  , True,
	condition = False;
	For[j0 = 1, j0 <= Length@$NS[i] && condition, j0++,
		For[j1 = j0+1, j1 <= Length@$NS[i] && condition, j1++, 
			If[!MissingQ@$U[[$NS[i][[j0]], k]]
			&& !MissingQ@$U[[$NS[i][[j1]], k]]
			&& $U[[$NS[i][[j0]],k]] !=  $U[[$NS[i][[j1]],k]],
			$U[[i, k ]] = Missing[]; (* sobra *)
		       condition = True;
		     ];
		  ];
    ];
     
	If[ !condition,
	  For[jj = 1, jj <= Length@$NS[i], jj++,
		If[ !MissingQ@$U[[$NS[i][[jj]], k]], 
			 $U[[i,k]] = $U[[$NS[i][[jj]],k]];
			 $MAS[i] = DeleteCases[$MAS[i], k];
			 If[ $numMissings>0, --$numMissings];
			 flag = True;
			 Break[];
		];
	   ];
	];
		
	(* then, update the containers *)
	Table[
	  If[i != j,
          $GM[i,j] = {};
		  Table[
			If[ !MissingQ@$U[[i,kk]]
			  && !MissingQ@$U[[j,kk]]
			  && $U[[i,k]]!= $U[[j,kk]],
				$GM[i,j] = $GM[i,j]~Join~{kk};
		    ];
		,{kk, 1, m}];
		If[ Length@$GM[i,j] == 0,
			$NS[i] = $NS[i]~Join~{j};
		];
	  ];
	, {j, 1, n}];
];

If[ $MAS[i] == {},
	$MOS = DeleteCases[$MOS, i];
];

,{i, $MOS}, {k, $MAS[i]}];

If[ flag,
	ROUSTIDA[]
,   StepThree[]
];
];


Clear[StepThree];
StepThree[]:=Module[{},
	MeanCompleter[]
];


Clear[MeanCompleter];
MeanCompleter[]:= Module[
{n,m, freq},
If[Length@Dimensions@$U ==0, Print["MeanCompleter. Invalid $U"]; Return[]];
{n,m} = Dimensions[$U];

Table[
	freq = (Select[$U[[All, k]],Not@MissingQ[#] & ]// Tally // SortBy[#, Last]&)[[-1,1]];
	Table[
		If[ MemberQ[$MAS[i], k],
			$U[[i,k]] = freq;
		];
	,{i, $MOS}];

,{k, 1,m}];
];


EndPackage[];
