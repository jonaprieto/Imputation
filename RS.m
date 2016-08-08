(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: RS *)
(* :Context: RS` *)
(* :Author: jonaprieto *)
(* :Date: 2016-08-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 jonaprieto *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["RS`"];
(* Exported symbols added here with SymbolName::usage *)

Universe::usage = "";
Attrs::usage = "";
Conditions::usage = "";
Decisions::usage = "";
Base::usage = "";

Begin["`Private`"];

Clear[Universe];
Universe[] := Return@$universe;
Universe[list_List] := ($universe = list);

Clear[Attrs];
Attrs[] := Return@$attributes;
Attrs[list_] := ($attributes = list);

Clear[Conditions];
Conditions[] := Return@$conditions;
Conditions[list_] := ($conditions = list);

Clear[Decisions];
Decisions[] := Return@$decisions;
Decisions[list_] := ($deecisions = list);

Clear[Base];
Base[] := Return@$base;
Base[list_] := ($base = list);

Clear[VFunction];
VFunction[a_] := Module[
  {pos = Position[$attributes, a]},
  Assert[Length@pos == 1 && Length@pos[[1]] == 1 ];
  pos = Flatten[pos][[1]];
  Union@$universe[[All, pos]]
];

Clear[FindColsBase];
FindColsBase[B_List] := Flatten[Map[Position[$attributes, #]&, B], Infinity];

Clear[SetBase];
SetBase[] := SetBase[RS`Base];
SetBase[B_List] := Module[{},
  Assert[Length@B > 0];
  Base@B;
  $colsBase = Flatten[Map[Position[$attributes, #]&, B], Infinity];
  Return@Base[];
];


Clear[GetAttributes];
GetAttributes[x_] := Part[x, $colsBase];
GetAttributes[x_, B_] := Module[
  {cols},
  cols = FindColsBase[B];
  Return@Part[x, cols];
];


Clear[IRelation];
IRelation[x_, y_] := (IRelation[x, y] = IRelation[x, y, Base[]]);
IRelation[x_, y_, Base_] := (IRelation[x, y, Base] = Module[
  {},
  Return@AllTrue[
    Thread[{GetAttributes[x, Base], GetAttributes[y, Base]}] ,
    #[[1]] == #[[2]] &];
];
);


Clear[EquivIndexes];
EquivIndexes[] := EquivIndexes[Universe[], Base[]];
EquivIndexes[li_] := (EquivIndexes[li] = EquivIndexes[li, Base[]]);
EquivIndexes[li_, B_] := (
  EquivalenceIndexes[li, B] = Module[
    {indices = Range@Length@li, eq, res654 = h654[]},
    While[indices =!= {},
      With[{el = li[[First[indices] ]]},
        eq = Select[indices, IRelation[el, li[[#]], B] &];
        res654 = h654[res654, eq];
        indices = Complement[indices, eq]
      ]
    ];
    Return[List @@ Flatten[res654, Infinity, h654]];
  ];
);


Clear[EquivClasses];
EquivClasses[] := EquivClasses[Universe[], Base[]];
EquivClasses[li_] := (EquivClasses[li] = EquivClasses[li, Base[]]);
EquivClasses[li_, B_] := (EquivClasses[li, B] = (Part[li, #] & /@ EquivIndexes[li, B]));

Clear[EquivClass];
EquivClass[x_] := (EquivClass[x] = EquivClass[x, Base[]]);
EquivClass[x_, B_] := (EquivClass[x, B] = Block[{res},
  res = Union@Select[Universe[], IRelation[x, #, B] &];
  Return@If[Length@res == 0,
    {x},
    res
  ];
]);



Clear[LowerApprox];
LowerApprox[Xsubset_] := (LowerApprox[Xsubset] = LowerApprox[Xsubset, Base[]]);
LowerApprox[Xsubset_, B_] := (LowerApprox[Xsubset, B] = Block[
  {x, clase, setClass, asigClase, claseEquiv, i = 0, Bx},
  claseEquiv = EquivIndexes[Universe[], B];
  asigClase = Flatten@Table[++i; Map[ (# -> i )&, clase] , {clase, claseEquiv}];
  Union@Table[
    x = $universe[[i]];
    clase = i /. asigClase;
    setClass = List /@ claseEquiv[[clase]];
    Bx = Extract[$universe, setClass];
    If[ SubsetQ[ Xsubset, Bx], x, Nothing]
    , {i, Length@$universe}]
]);

Clear[UpperApprox];
UpperApprox[Xsubset_] := (UpperApprox[Xsubset] = UpperApprox[Xsubset, Base[]]);
UpperApprox[Xsubset_, B_] := (UpperApprox[Xsubset, B] = Block[
  {x, clase, setClass, asigClase, claseEquiv, i = 0, Bx},
  claseEquiv = EquivIndexes[Universe[], B];
  asigClase = Flatten@Table[++i; Map[(# -> i)&, clase] , {clase, claseEquiv}];
  Union@Table[
    x = $universe[[i]];
    clase = i /. asigClase;
    setClass = List /@ claseEquiv[[clase]];
    Bx = Extract[$universe, setClass];
    If[ Intersection[Xsubset, Bx] != {},
      x,
      Nothing
    ]
    , {i, Length@$universe}]
]);

Clear[Boundary];
Boundary[Xsubset_] := Boundary[Xsubset, Base[]];
Boundary[Xsubset_, B_] := Intersection[UpperApprox[Xsubset, B], Complement[$universe, LowerApprox[Xsubset, B]]];


Clear[MemberFunction];
MemberFunction[x_, X_] := MemberFunction[x, X, Base[]];
MemberFunction[x_, X_, B_] := With[{
  Bx = EquivClass[x, B]},
  N@Divide[Length@Intersection[Bx, X], Length@Bx]
];


Clear[POS];
POS[Dsubset_, B_] := (RS`POS[Dsubset, B] = With[
  {classes = EquivClasses[$universe, Dsubset]},
  Union@Flatten[Table[LowerApprox[X, B], {X, classes}], 1]
]);

Clear[NEG];
NEG[Dsubset_, B_] := With[
  {classes = EquivClasses[$universe, Dsubset]},
  Complement[$universe, Union@Flatten[Table[ UpperApprox[X, B], {X, classes}], 1]]
];

Clear[BND];
BND[Dsubset_, B_] := Complement[ NEG[Dsubset, B], POS[Dsubset, B]];


Clear[DependencyAttributes];
DependencyAttributes[Dsubset_, Csubset_] := (
  DependencyAttributes[Dsubset, Csubset] = N@Divide[Length@POS[Dsubset, Csubset], Length@$universe]);

Clear[SignificanceAttributes];
SignificanceAttributes[a_] := (SignificanceAttributes[a] = SignificanceAttributes[$conditions, $decisions, a]);
SignificanceAttributes[a_, Csubset_, Dsubset_] := (
  SignificanceAttributes[a, Csubset, Dsubset] = 1 - N@Divide[DependencyAttributes[Dsubset, Complement[Csubset, {a}]], DependencyAttributes[Dsubset, Csubset]]);


Clear[QuickReduct];
QuickReduct[] := QuickReduct[$conditions, $decisions];
QuickReduct[Csubset_List, Dsubset_List] := Module[
  {R = {}, T},
  While[ True,
    T = R;
    Table[
      If[ DependencyAttributes[Dsubset, R ~ Join ~ {attr}] > DependencyAttributes[Dsubset, T],
        T = Flatten[{R, attr}, Infinity];
      ]
      , {attr, Complement[Csubset, R]}];
    R = T;
    If[ DependencyAttributes[Dsubset, R] == DependencyAttributes[Dsubset, Csubset], Break[]]
  ];
  R
];

End[]; (* `Private` *)

EndPackage[];