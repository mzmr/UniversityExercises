package Lab03_Pkg is

 type Vector is array (Integer range <>) of Float;
 procedure PrintVector(Vec : in Vector);
 procedure RandomizeVector(Vec : out Vector);
 function IsVectorSorted(Vec : in Vector) return Boolean;
 procedure SortVector(Vec : in out Vector);
 procedure VectorToFile(Vec : in Vector; Filename : in String);

end Lab03_Pkg;
