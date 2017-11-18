with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Numerics.Discrete_Random, Ada.Unchecked_Deallocation;
use Ada.Text_IO, Ada.Integer_Text_IO;

package body Tree is

  procedure FreeNode is new Ada.Unchecked_Deallocation(Node, Node_Ptr);
  procedure FreeNP is new Ada.Unchecked_Deallocation(NodeWithParent, NP_Ptr);

  procedure Print(Tree : access Node) is
  begin
    if Tree = null then
      Put_Line("Tree is empty!");
    else
      Put_Line("Tree:");
      PrintEl(Tree, 0);
    end if;
  end Print;
  
  procedure PrintEl(Tree : access Node; IndentWidth : in Integer) is
  begin
    Indent(IndentWidth);
  
    if Tree = null then
      Put_Line(".");
      return;
    end if;
    
    Put(Tree.Data, 1);
    New_Line;
    PrintEl(Tree.Left, IndentWidth + 1);
    PrintEl(Tree.Right, IndentWidth + 1);
  end PrintEl;
  
  procedure Indent(Width : in Integer) is
  begin
    for I in 1..Width loop
      Put(" ");
    end loop;
  end Indent;
  
  procedure Insert(Tree : in out Node_Ptr; D : in Integer) is
    N : Node_Ptr := new Node;
    Current : Node_Ptr := Tree;
    Next : Node_Ptr := Tree;
  begin
    N.Data := D;
    
    if Tree = null then
      Tree := N;
      return;
    end if;
    
    loop
      if Current.Data > D then
        if Current.Left /= null then
          Current := Current.Left;
        else
          Current.Left := N;
          return;
        end if;
      else
        if Current.Right /= null then
          Current := Current.Right;
        else
          Current.Right := N;
          return;
        end if;
      end if;
    end loop;
  end Insert;
  
  procedure Insert_Random(Tree : in out Node_Ptr; N, TopLimit : in Integer) is
    package Rand_Int is new Ada.Numerics.Discrete_Random(Integer);
    use Rand_Int;
    Gen : Generator;
    RandNum : Integer;
  begin
    Reset(Gen);

    for I in 0 .. N loop
      RandNum := (abs Random(Gen)) mod (TopLimit + 1);
      Insert(Tree, RandNum);
    end loop;
  end Insert_Random;
  
  function Search(Tree : in Node_Ptr; E : in Integer) return Boolean is
  begin
    if Tree = null then
      return False;
    elsif Tree.Data = E then
      return True;
    elsif Search(Tree.Left, E) then
      return True;
    elsif Search(Tree.Right, E) then
      return True;
    else
      return False;
    end if;
  end Search;
  
  function Remove(Tree : in out Node_Ptr; E : in Integer) return Boolean is
    EmptyParent : Node_Ptr := null;
    ToRemove : NP_Ptr;
    N, P : Node_Ptr;
  begin
    ToRemove := FindNodeToRemove(Tree, E, EmptyParent);
    
    if ToRemove /= null then
      N := ToRemove.TheNode;
      P := ToRemove.Parent;
      RemoveEl(N, E, P);
      return True;
    else
      return False;
    end if;
  end Remove;
  
  function FindNodeToRemove(Tree : in Node_Ptr; E : in Integer; Parent : in Node_Ptr) return NP_Ptr is
    SearchResult : NP_Ptr;
  begin
    if Tree = null then
      return null;
    end if;
    
    if Tree.Data = E then
      SearchResult := new NodeWithParent'(Tree, Parent);
      return SearchResult;
    end if;
    
    SearchResult := FindNodeToRemove(Tree.Left, E, Tree);
    if SearchResult /= null then
      return SearchResult;
    end if;
    
    SearchResult := FindNodeToRemove(Tree.Right, E, Tree);
    if SearchResult /= null then
      return SearchResult;
    end if;
    
    return null;
  end FindNodeToRemove;

  procedure RemoveEl(Tree : in out Node_Ptr; E : in Integer; Parent : in out Node_Ptr) is
    ToRemove : Node_Ptr := Tree;
    MinNode : NP_Ptr;
    NewChild : Node_Ptr;
  begin
    if Tree.Left /= null and Tree.Right /= null then
      MinNode := FindMinNode(Tree.Right, Tree);
      Tree.Data := MinNode.TheNode.Data;
      RemoveEl(MinNode.TheNode, E, MinNode.Parent);
      FreeNP(MinNode);
    else
      if ToRemove.Left = null and ToRemove.Right = null then
        NewChild := null;
        Tree := null;
      elsif Tree.Left = null and Tree.Right /= null then
        NewChild := Tree.Right;
      else
        NewChild := Tree.Left;
      end if;
      
      if Parent /= null then
        if Parent.Left = ToRemove then
          Parent.Left := NewChild;
        else
          Parent.Right := NewChild;
        end if;
      else
        if Tree.Left = null or Tree.Right = null then
          ToRemove := NewChild;
          Tree.all := NewChild.all;
        end if;
      end if;
      
      FreeNode(ToRemove);
    end if;
  end RemoveEl;
  
  function FindMinNode(Tree, Parent : in Node_Ptr) return NP_Ptr is
  begin
    if Tree.Left /= null then
      return FindMinNode(Tree.Left, Tree);
    else
      return new NodeWithParent'(Tree, Parent);
    end if;
  end FindMinNode;
  
end Tree;
