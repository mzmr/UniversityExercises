package List is

  type Element is
  record
    Data : Integer := 0;
    Next : access Element := null;
  end record;

  type Elem_Ptr is access all Element;

  procedure Print(List : access Element);
  procedure Insert(List : in out Elem_Ptr; D : in Integer);
  function Insert(List : access Element; D : in Integer) return access Element;
  procedure Insert_Sort(List : in out Elem_Ptr; D : in Integer);
  procedure Insert_Random(List : in out Elem_Ptr; N, TopLimit : in Integer);
  function Search(List : in Elem_Ptr; E : in Integer) return Integer;
  function Remove(List : in out Elem_Ptr; E : in Integer) return Boolean;

end List;
