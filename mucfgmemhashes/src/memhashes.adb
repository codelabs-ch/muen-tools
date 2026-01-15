--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions;
with Ada.Streams;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents;

with Interfaces;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Strings;
with Mutools.Utils;

with Memhashes.Utils;
with Memhashes.Pre_Checks;

package body Memhashes
is

   --  As sinfo files are not hashed, create a copy of the given policy with
   --  sinfo memory regions stripped.
   function Remove_Sinfo_Files
     (Policy : Muxml.XML_Data_Type)
      return Muxml.XML_Data_Type;

   -------------------------------------------------------------------------

   procedure Generate_Hashes
     (Policy    : in out Muxml.XML_Data_Type;
      Input_Dir :        String)
   is
      use type Ada.Containers.Count_Type;

      type Input_Data_Type is record
         Mem_Node : DOM.Core.Node;
      end record;

      type Output_Data_Type is record
         Mem_Node     : DOM.Core.Node;
         Hash_Str     : Utils.Hash_String;
         Use_Size     : Boolean;
         Content_Size : Ada.Streams.Stream_Element_Count;
      end record;

      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type!='subject_info']/*"
           & "[self::fill or self::file]/..");
      Input_Dirs : constant Mutools.Strings.String_Array
        := Mutools.Strings.Tokenize (Str => Input_Dir);
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);

      package Input_Queue_Interfaces is new
        Ada.Containers.Synchronized_Queue_Interfaces
          (Element_Type => Input_Data_Type);
      package Input_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
        (Queue_Interfaces => Input_Queue_Interfaces);
      package Output_Queue_Interfaces is new
        Ada.Containers.Synchronized_Queue_Interfaces
          (Element_Type => Output_Data_Type);
      package Output_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
        (Queue_Interfaces => Output_Queue_Interfaces);

      Work_Input  : Input_Queues.Queue;
      Work_Output : Output_Queues.Queue;

      Worker_Count : constant := 16;
      type Worker_Id_Type is range 1 .. Worker_Count;

      task type Worker is
         entry Start (Id : Worker_Id_Type);
         entry Finish (Success : out Boolean);
      end Worker;

      task body Worker
      is
         Item          : Input_Data_Type;
         Result        : Utils.Result_Type;
         My_Id         : Worker_Id_Type;
         Processed     : Natural := 0;
         Error_Occured : Boolean := False;
      begin
         accept Start (Id : Worker_Id_Type) do
            My_Id := Id;
         end Start;

         begin
            Process : loop
               select
                  Work_Input.Dequeue (Element => Item);
               else
                  exit Process;
               end select;

               Result := Utils.SHA256_Digest (Node       => Item.Mem_Node,
                                              Input_Dirs => Input_Dirs);
               Work_Output.Enqueue
                 (New_Item => (Mem_Node     => Item.Mem_Node,
                               Hash_Str     => Result.Hash,
                               Use_Size     => Result.Use_Size,
                               Content_Size => Result.Content_Size));
               Processed := Processed + 1;
            end loop Process;
            Mulog.Log
              (Level => Mulog.Info,
               Msg   => "Worker" & My_Id'Img & " is done," & Processed'Img
                 & " items processed");

         exception
            when X : others =>
               Mulog.Log (Level => Mulog.Error,
                          Msg   => "Worker" & My_Id'Img & " terminated due to "
                            & Ada.Exceptions.Exception_Information (X));
               Error_Occured := True;
         end;

         accept Finish (Success : out Boolean) do
            Success := not Error_Occured;
         end Finish;
      end Worker;

   begin
      Mulog.Log (Msg => "Looking for input files in '" & Input_Dir & "'");
      Mulog.Log (Msg => "Generating hashes for" & Count'Img
                 & " memory regions");

      for I in 0 .. Count - 1 loop
         Work_Input.Enqueue (New_Item => Input_Data_Type'
           (Mem_Node => DOM.Core.Nodes.Item (List => Nodes, Index => I)));
      end loop;

      declare
         Workers : array (Worker_Id_Type) of Worker;
         Success : array (Worker_Id_Type) of Boolean;
      begin
         for Id in Worker_Id_Type loop
            Workers (Id).Start (Id => Id);
         end loop;

         for Id in Worker_Id_Type loop
            Workers (Id).Finish (Success => Success (Id));
         end loop;

         if (for some Id in Worker_Id_Type => not Success (Id)) then
            raise Thread_Error with "Error in hashing worker thread";
         end if;
      end;

      while Work_Output.Current_Use > 0 loop
         declare
            use type DOM.Core.Node;
            Item         : Output_Data_Type;
            Hash_Node    : DOM.Core.Node;
            Content_Node : DOM.Core.Node;
         begin
            Work_Output.Dequeue (Element => Item);

            Hash_Node := Muxml.Utils.Get_Element
              (Doc   => Item.Mem_Node,
               XPath => "hash");

            --  Annotate computed size of content node
            if Item.Use_Size then
               Content_Node := Muxml.Utils.Get_Element
                 (Doc   => Item.Mem_Node,
                  XPath => "*[self::fill or self::file]");
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Content_Node,
                  Name  => "size",
                  Value => Mutools.Utils.To_Hex
                    (Number => Interfaces.Unsigned_64 (Item.Content_Size)));
            end if;

            if Hash_Node = null then
               Hash_Node := DOM.Core.Documents.Create_Element
                 (Doc      => Policy.Doc,
                  Tag_Name => "hash");
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Hash_Node,
                  Name  => "value",
                  Value => Item.Hash_Str);
               Muxml.Utils.Append_Child (Node      => Item.Mem_Node,
                                         New_Child => Hash_Node);
            else
               if DOM.Core.Elements.Get_Attribute
                 (Elem => Hash_Node,
                  Name => "value") = "none"
               then
                  Mulog.Log (Msg => "Skipping region with hash none: '"
                             & DOM.Core.Elements.Get_Attribute
                               (Elem => Item.Mem_Node,
                                Name => "name") & "'");
               elsif Item.Hash_Str /= DOM.Core.Elements.Get_Attribute
                 (Elem => Hash_Node,
                  Name => "value")
               then
                  raise Hasher_Error with "Hash mismatch for memory "
                    & "region '"
                    & DOM.Core.Elements.Get_Attribute (Elem => Item.Mem_Node,
                                                       Name => "name")
                    & "'";
               end if;
            end if;
         end;
      end loop;
   end Generate_Hashes;

   -------------------------------------------------------------------------

   function Remove_Sinfo_Files
     (Policy : Muxml.XML_Data_Type)
      return Muxml.XML_Data_Type
   is
      New_Doc : DOM.Core.Document;
      Impl    : DOM.Core.DOM_Implementation;
      Dummy   : DOM.Core.Node;
   begin
      New_Doc := DOM.Core.Create_Document (Implementation => Impl);
      Dummy    := DOM.Core.Documents.Import_Node
        (Doc           => New_Doc,
         Imported_Node => DOM.Core.Documents.Get_Element
           (Doc => Policy.Doc),
         Deep          => True);
      Dummy := DOM.Core.Nodes.Append_Child
        (N         => New_Doc,
         New_Child => Dummy);

      declare
         Mem_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => New_Doc,
              XPath => "/system/memory");
         Nodes : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Mem_Node,
              XPath => "memory[@type='subject_info']");
         Count : constant Positive := DOM.Core.Nodes.Length (List => Nodes);
      begin
         Mulog.Log (Msg => "Skipping" & Count'Img & " sinfo region(s)");

         for I in 0 .. Count - 1 loop
            Dummy := DOM.Core.Nodes.Remove_Child
              (N         => Mem_Node,
               Old_Child => DOM.Core.Nodes.Item
                 (List  => Nodes,
                  Index => I));
            DOM.Core.Nodes.Free (N => Dummy);
         end loop;
      end;

      return X : Muxml.XML_Data_Type do
         X.Doc := New_Doc;
      end return;
   end Remove_Sinfo_Files;

   -------------------------------------------------------------------------

   procedure Resolve_Refs (Policy : in out Muxml.XML_Data_Type)
   is
      Mem_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Ref_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory/hashRef");
      Count : constant Natural
        := DOM.Core.Nodes.Length (List => Ref_Nodes);
   begin
      if Count /= 0 then
         Mulog.Log (Msg => "Resolving" & Count'Img & " hash reference(s)");

         for I in 0 .. Count - 1 loop
            declare
               use type DOM.Core.Node;

               Hash_Ref : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Ref_Nodes,
                                         Index => I);
               Mem : constant DOM.Core.Node
                 := DOM.Core.Nodes.Parent_Node (N => Hash_Ref);
               Memname : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Mem,
                    Name => "name");
               Referenced_Memname : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Hash_Ref,
                    Name => "memory");
               Referenced_Mem : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Mem_Nodes,
                    Ref_Attr  => "name",
                    Ref_Value => Referenced_Memname);
               Referenced_Hash : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Referenced_Mem,
                    XPath => "hash");
            begin
               if Referenced_Hash = null then
                  raise Reference_Error with "Physical memory '"
                    & Referenced_Memname & "' referenced by hashRef of memory "
                    & "'" & Memname & "' does not provide hash element";
               end if;

               Muxml.Utils.Append_Child
                 (Node      => Mem,
                  New_Child => DOM.Core.Nodes.Clone_Node
                    (N    => Referenced_Hash,
                     Deep => False));
               Muxml.Utils.Remove_Child
                 (Node       => Mem,
                  Child_Name => "hashRef");
            end;
         end loop;
      end if;
   end Resolve_Refs;

   -------------------------------------------------------------------------

   procedure Run (Policy_In, Policy_Out, Input_Dir : String)
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing policy '" & Policy_In & "'");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_In);

      Pre_Checks.Register_All;
      Mulog.Log (Msg => "Registered pre-check(s)" & Pre_Checks.Get_Count'Img);

      Pre_Checks.Run
        (Data      => Remove_Sinfo_Files (Policy => Policy),
         Input_Dir => Input_Dir);

      Generate_Hashes (Policy    => Policy,
                       Input_Dir => Input_Dir);
      Resolve_Refs (Policy);

      Mulog.Log (Msg => "Writing policy to '" & Policy_Out & "'");
      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_Out);

      Pre_Checks.Clear;

   exception
      when others =>
         Pre_Checks.Clear;
         raise;
   end Run;

end Memhashes;
