with Ada.Text_IO, Ada.Numerics.Float_Random;
use Ada.Text_IO, Ada.Numerics.Float_Random;

package body Lab03_Pkg is

  procedure PrintVector(Vec : in Vector) is
  begin
    for I in Vec'Range loop
      Put_Line(I'Img & " -> " & Vec(I)'Img);
    end loop;
  end PrintVector;
  
  procedure RandomizeVector(Vec : out Vector) is
    Gen : Generator;
  begin
    Reset(Gen);
    
    for I in Vec'Range loop
      Vec(I) := Random(Gen);
    end loop;
  end RandomizeVector;
  
  function IsVectorSorted(Vec : in Vector) return Boolean is
  (for all I in Vec'First .. Vec'Last - 1 => Vec(I) <= Vec(I + 1));

  procedure SwapElements(Vec : in out Vector; Idx1, Idx2 : in Integer) is
    Temp : Float;
  begin
    Temp := Vec(Idx1);
    Vec(Idx1) := Vec(Idx2);
    Vec(Idx2) := Temp;
  end SwapElements;
  
  function PartitionVector(Vec : in out Vector; Left, Right : in Integer) return Integer is
    Pivot : Float;
    I : Integer;
  begin
    Pivot := Vec(Right);
    I := Left - 1;
      
    for J in Left .. Right - 1 loop
      if Vec(J) < Pivot then
        I := I + 1;
        SwapElements(Vec, I, J);
      end if;
    end loop;
	
    if Vec(Right) < Vec(I + 1) then
      SwapElements(Vec, Right, I + 1);
    end if;
  
    return I + 1;
  end PartitionVector;
    
  procedure QuickSort(Vec : in out Vector; Left, Right : in Integer) is
    Pivot : Integer;
  begin
    if Left < Right then
      Pivot := PartitionVector(Vec, Left, Right);
      QuickSort(Vec, Left, Pivot - 1);
      QuickSort(Vec, Pivot + 1, Right);
    end if;
  end QuickSort;
  
  procedure SortVector(Vec : in out Vector) is
  begin
    QuickSort(Vec, Vec'First, Vec'Last);
  end SortVector;
  
  procedure VectorToFile(Vec : in Vector; Filename : in String) is
    FType : File_Type;
  begin
    Create(FType, Out_File, FileName);
    for I in Vec'Range loop
      Put_Line(FType, I'Img & ";" & Vec(I)'Img);
    end loop;
    Close(FType);  
  end VectorToFile;

end Lab03_Pkg;
