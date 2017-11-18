with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Numerics.Discrete_Random, Ada.Unchecked_Deallocation;
use Ada.Text_IO, Ada.Integer_Text_IO;

package body List is

  procedure Free is new Ada.Unchecked_Deallocation(Element, Elem_Ptr);

  procedure Print(List : access Element) is
    L : access Element := List;
  begin
    if List = null then
      Put_Line("List EMPTY!");
    else
      Put_Line("List:");
    end if;
    while L /= null loop
      Put(L.Data, 4);
      New_Line;
      L := L.Next;
    end loop;
  end Print;

  procedure Insert(List : in out Elem_Ptr; D : in Integer) is
    E : Elem_Ptr := new Element;
  begin
    E.Data := D;
    E.Next := List;
    List := E;
  end Insert;

  function Insert(List : access Element; D : in Integer) return access Element is
  ( new Element'(D,List) );

  procedure Insert_Sort(List : in out Elem_Ptr; D : in Integer) is
    E : Elem_Ptr := new Element'(D,List);
    Current : Elem_Ptr := List;
  begin
    if List = null or else List.Data >= D then
      List := E;
      return;
    end if;

    while Current.Next /= null and then Current.Next.Data < D loop
      Current := Current.Next;
    end loop;

    if Current.Next = null then
      E.Next := null;
      Current.Next := E;
    else
      E.Next := Current.Next;
      Current.Next := E;
    end if;
  end Insert_Sort;


  procedure Insert_Random(List : in out Elem_Ptr; N, TopLimit : in Integer) is
    package Rand_Int is new Ada.Numerics.Discrete_Random(Integer);
    use Rand_Int;
    Gen : Generator;
    RandNum : Integer;
  begin
    Reset(Gen);

    for I in 0 .. N loop
      RandNum := (abs Random(Gen)) mod (TopLimit + 1);
      Insert(List, RandNum);
    end loop;
  end Insert_Random;


  function Search(List : in Elem_Ptr; E : in Integer) return Integer is
    Current : Elem_Ptr := List;
    I : Integer := 0;
  begin
    while Current /= null loop
      if E = Current.Data then
        return I;
      end if;

      Current := Current.Next;
      I := I + 1;
    end loop;

    return -1;
  end Search;


  function Remove(List : in out Elem_Ptr; E : in Integer) return Boolean is
    Current : Elem_Ptr := List;
    Previous : Elem_Ptr := null;
  begin
    while Current /= null loop
      if Current.Data = E then
        if Previous /= null then
          Previous.Next := Current.Next;
        else
          List := Current.Next;
        end if;

        Free(Current);
        return True;
      end if;

      Previous := Current;
      Current := Current.Next;
    end loop;

    return False;
  end Remove;

end List;
