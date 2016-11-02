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



Begin["`Private`"];

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
TestAlgorithm[datasets_List, numIter_Integer:30] := Module[
  {oData, name, citer, outcome = <||>, matches, cDataset = 0,
  res, oldJ, n=0,m=0},

  SetInitValues[];
  AbortAssert[ $missingRate > 0 && $missingRate < 0.5 ];

  Clear[$outcome]
  Clear[$verboseOutcome];
  $outcome = <||>;
  
  $lastResult = "";

  $minResult = Infinity;
  $maxResult = -Infinity;

  PrintTemporary@Dynamic@Dataset[
    <|
      "No." -> ToString@cDataset <> "/" <> ToString@Length@datasets,
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
  
  $outcome = Association@Table[i-> <||>,{i, 1, Length@datasets}];
  
  cDataset = 1;
  Table[
    name = dataset["filename"];
    oData = Import@dataset["data"];
    With[
      {d = Dimensions@oData},
      AbortAssert[Length@d == 2, "TestAlgorithm"];
      {n, m} = d;
      AbortAssert[n > 2 && m > 2, "TestAlgorithm"];
    ];
    
    $outcome[cDataset] = <|
      "Dataset" -> name,
      "Size" -> ToString@n <> "x" <> ToString@m
    |>;

      $minResult = Infinity;
      $maxResult = -Infinity;
 
      citer = 0;
      res = Table[
        citer++;
        $U = oData;
        SetMissings[];
        StepOne[];
        oldJ = Table[$MAS[i], {i, 1, n}];
        StepTwo[];
       
        matches = checkMatches[oldJ];
        
        With[{stat = N[Total@matches / $numMissings]},
          $lastResult = stat;
          $minResult = Min[$minResult, stat];
          $maxResult = Max[$maxResult, stat];
          stat
        ]
       , {numIter}];
       
      $outcome[[cDataset]]= <|
        "min"-> $minResult
      , "mean" -> NumberForm[Mean@res, {3, 2}]
      , "max" ->  $maxResult
      |>;
      $verboseOutcome[[cDataset]] = <|
         "measures"-> res
      |>;
      
    cDataset++;
    , {dataset, datasets}];
    Print[Dataset[$outcome]];
    Return[$outcome];
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

Clear[$MOS, $MAS, $V, $OMS, $GM, $Mlv, $NS];

Table[
	$V[k] = {};
	$OMS[k] = {};
,{k, 1, m}];

$MOS = {};

Table[
	$MAS[i] = {}; $NS[i] = {};
	Table[
		If[ MissingQ@$U[[i,k]],
			$MAS[i] = {$MAS[i], k};
			$OMS[k] = {$OMS[k], i};
		,
			$V[k] = {$V[k], $U[[i, k]]};
		];
	,{k, 1, m}];

	If[ Length@$MAS[i] > 0, $MOS = {$MOS, i}];
	$Mlv[i,i] = 1;
,{i, 1, n}];

Table[
		$V[k] = Union@Flatten[$V[k], Infinity];
		$OMS[k] = Union@Flatten[$OMS[k], Infinity];
,{k,1, m}];
	
	
Table[	
	Table[
		$GM[i,j] = {}; $Mlv[i,j] = 1;
		Table[
			If[ MissingQ@$U[[i,k]],
				If[  MissingQ@$U[[j,k]],
					$Mlv[i,j] = $Mlv[i,j] / (Length[$V[k]]^2);
				,   $Mlv[i,j] = $Mlv[i,j] / Length[$V[k]];
				];
			, If[  MissingQ@$U[[j,k]], 
					$Mlv[i,j] = $Mlv[i,j] / Length[$V[k]];
					,
					If[ $U[[i,k]]!= $U[[j,k]],
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


];


Clear[StepTwo];
StepTwo[] := Module[
{valMaxJ, maxJ, n,m},
If[ Length@Dimensions@$U != 2, Print["Step Two. Invalid $U."]; Return[]];
{n,m} = Dimensions[$U];

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
			$U[[i,k]] = $U[[maxJ,k]];
			$OMS[k] = DeleteCases[$OMS[k], i];
		,{k, 1, m}];
		$MAS[i] = {};
		$MOS = DeleteCases[$MOS, i];
		Table[
			$Mlv[i,j] = 1; $GM[i,j] = {};
			Table[
				If[ MissingQ@$U[[j,k]],
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
If[Length@Dimensions@$U ==0, Print["MeanCompleter. Invalid $U"]; Return[]];
{n,m} = Dimensions[$U];

Table[
	freq = SortBy[Tally@Table[$U[[i,k]], {i,1,n}], -#[[2]] &];
	Table[
		If[ MemberQ[$MAS[i], k],
			$U[[i,k]] = freq;
		];
	,{i, $MOS}];
	
,{k, 1,m}];
];


End[];

(*`Private`*)



EndPackage[];
