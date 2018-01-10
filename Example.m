BeginPackage["Example`"];

paperIS =
  {
     { 3,         2,          1,          0}
  ,  { Missing[], 2,          Missing[],  0}
  ,  { 3,         3,          1,          Missing[]}
  ,  { 2,         2,          Missing[],  0}
  ,  { 3,         Missing[],  Missing[],  3}
  ,  { Missing[], 2,          2,          Missing[]}
  ,  { 3,         2,          3,          3}
  ,  { 2,         Missing[],  2,          Missing[]}
  }

Table1 =
  {
    { "female",   2,          "yes",      "yes"}
  , { Missing[],  Missing[],  "no",       "no"}
  , { "male",     3,          Missing[],  "yes"}
  , { "female",   Missing[],  "yes",      "no"}
  , { Missing[],  2,          "no",       "no"}
  , { "male",     Missing[],  "no",       "no"}
  , { Missing[],  3,          "no",       "no"}
  }

EndPackage[];

