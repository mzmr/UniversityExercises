with Ada.Text_IO, Ada.Integer_Text_IO, List, Tree;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure Main is

  procedure TestListPackage is
    MyList : List.Elem_Ptr := null;
    SearchedNum : Integer := 4;
    SearchedPos : Integer;
  begin
    Put_Line("-- Printing an empty list");
    List.Print(MyList);

    New_Line; Put_Line("-- Inserting two elements to the list");
    MyList := List.Insert(MyList, 21);
    List.Insert(MyList, 20);
    List.Print(MyList);

    New_Line; Put_Line("-- Inserting next 12 elements");
    for I in reverse 1..12 loop
      List.Insert(MyList, I);
    end loop;
    List.Print(MyList);

    New_Line; Put_Line("-- Inserting two elements keeping list sorted");
    List.Insert_Sort(MyList,15);
    List.Insert_Sort(MyList,3455);
    List.Print(MyList);

    New_Line; Put_Line("-- Inserting 6 random numbers from range [0; 4]");
    List.Insert_Random(MyList, 6, 4);
    List.Print(MyList);

    New_Line; Put_Line("-- Looking for a number " & SearchedNum'Img);
    SearchedPos := List.Search(MyList, SearchedNum);
    if SearchedPos /= -1 then
      Put_Line("Found " & SearchedNum'Img & " at the position " & SearchedPos'Img & ".");
    else
      Put_Line("Number " & SearchedNum'Img & " is not in the list.");
    end if;

    New_Line; Put_Line("-- Removing first occurence of number 2");
    if List.Remove(MyList, 2) then
      Put_Line("Succesfully removed number 2");
    else
      Put_Line("Couldn't remove number 2, it's not present in the list");
    end if;
    List.Print(MyList);
  end TestListPackage;


  procedure TestTreePackage is
    MyTree1 : Tree.Node_Ptr := null;
    MyTree2 : Tree.Node_Ptr := null;
    MyTree3 : Tree.Node_Ptr := null;
    SearchRes : Boolean;
  begin
    Put_Line("-- Printing an empty tree");
    Tree.Print(MyTree1);

    New_Line; Put_Line("-- Inserting 5 elements to the tree");
    Tree.Insert(MyTree1, 6);
    Tree.Insert(MyTree1, 10);
    Tree.Insert(MyTree1, 12);
    Tree.Insert(MyTree1, 8);
    Tree.Insert(MyTree1, 7);
    Tree.Print(MyTree1);

    New_Line; Put_Line("-- Inserting 4 random elements");
    Tree.Insert_Random(MyTree1, 4, 10);
    Tree.Print(MyTree1);

    New_Line; Put_Line("-- Looking for number 8");
    SearchRes := Tree.Search(MyTree1, 8);
    if SearchRes then
      Put_Line("Number 8 found.");
    else
      Put_Line("Number 8 is not in the tree.");
    end if;

    New_Line; Put_Line("-- Removing first occurence of 8 if exists");
    if Tree.Remove(MyTree1, 8) then
      Put_Line("Succesfully removed number 8");
    else
      Put_Line("Couldn't remove number 8, it's not present in the tree");
    end if;
    Tree.Print(MyTree1);

    New_Line; Put_Line("-- Another tree with the children placed only on the RIGHT side of each node");
    Tree.Insert(MyTree2, 1);
    Tree.Insert(MyTree2, 2);
    Tree.Insert(MyTree2, 3);
    Tree.Insert(MyTree2, 4);
    Tree.Insert(MyTree2, 5);
    Tree.Insert(MyTree2, 6);
    Tree.Print(MyTree2);

    New_Line; Put_Line("-- One more tree with the children placed only on the LEFT side of each node");
    Tree.Insert(MyTree3, -1);
    Tree.Insert(MyTree3, -2);
    Tree.Insert(MyTree3, -3);
    Tree.Insert(MyTree3, -4);
    Tree.Insert(MyTree3, -5);
    Tree.Insert(MyTree3, -6);
    Tree.Print(MyTree3);

  end TestTreePackage;


begin

  TestListPackage;
  New_Line;
  Put_Line(" --- --- --- --- ---");
  New_Line;
  TestTreePackage;

end Main;
