package Tree is

  type Node is
  record
    Data : Integer := 0;
    Left : access Node := null;
    Right : access Node := null;
  end record;
  
  type Node_Ptr is access all Node;

  procedure Print(Tree : access Node);
  procedure Insert(Tree : in out Node_Ptr; D : in Integer);
  procedure Insert_Random(Tree : in out Node_Ptr; N, TopLimit : in Integer);
  function Search(Tree : in Node_Ptr; E : in Integer) return Boolean;
  function Remove(Tree : in out Node_Ptr; E : in Integer) return Boolean;
  
private
  
  type NodeWithParent is
  record
    TheNode : Node_Ptr := null;
    Parent : Node_Ptr := null;
  end record;
  
  type NP_Ptr is access all NodeWithParent;
  
  procedure PrintEl(Tree : access Node; IndentWidth : in Integer);
  procedure Indent(Width : in Integer);
  function FindMinNode(Tree, Parent : in Node_Ptr) return NP_Ptr;
  procedure RemoveEl(Tree : in out Node_Ptr; E : in Integer; Parent : in out Node_Ptr);
  function FindNodeToRemove(Tree : in Node_Ptr; E : in Integer; Parent : in Node_Ptr) return NP_Ptr;
end Tree;
