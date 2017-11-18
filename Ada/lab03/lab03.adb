with Ada.Text_IO, Lab03_Pkg, Ada.Calendar;
use Ada.Text_IO, Lab03_Pkg, Ada.Calendar;

procedure Lab03 is
  MyVector : Vector(-5 .. 6) := (-4 .. 1 => 2.5, 4 .. 5 => 3.14, others => -1.234);
  StartTime, EndTime : Time;
  ExecutionTime : Duration;

begin
  StartTime := Clock;

  Put_Line("Vector at the start:");
  PrintVector(MyVector); New_Line;

  RandomizeVector(MyVector);

  Put_Line("Vector after randomization:");
  PrintVector(MyVector); New_Line;

  if IsVectorSorted(MyVector) then
    Put_Line("Congratualtions! Your vector is sorted aright.");
  else
    Put_Line("Eventually, you will have to sort your vector...");
  end if;

  New_Line;
  SortVector(MyVector);

  Put_Line("Vector after sorting:");
  PrintVector(MyVector); New_Line;

  Put("Is it now sorted? ");
  Put_Line(IsVectorSorted(MyVector)'Img); New_Line;

  Put_Line("Writing the vector to file.");
  VectorToFile(MyVector, "my_vector.txt");

  EndTime := Clock;
  ExecutionTime := EndTime - StartTime;
  Put_Line("Time of program execution: " & ExecutionTime'Img & "[s]");
end Lab03;
