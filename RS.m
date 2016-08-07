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


End[]; (* `Private` *)

EndPackage[];