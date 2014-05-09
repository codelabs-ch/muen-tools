--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Types;
with Musinfo.Utils;
with Musinfo.Writer;

package body Sinfo.Generator
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Sinfos : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='subject_info']/file");
   begin
      Mulog.Log (Msg => "Found" & DOM.Core.Nodes.Length (List => Sinfos)'Img
                 & " subject info file(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Sinfos) - 1 loop
         declare
            Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Sinfos,
                                      Index => I);
            Filename : constant String
              := Output_Dir & "/" & DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "filename");
            Memname  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
                 Name => "name");
            Subj_Name : constant String
              := Mutools.Utils.Decode_Entity_Name (Encoded_Str => Memname);
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system/subjects/subject[@name='" & Subj_Name
                 & "']");
            Subj_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
            Subject_Info : Musinfo.Subject_Info_Type
              := Musinfo.Null_Subject_Info;
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Subj_Memory) - 1 loop
               declare
                  use type Mutools.Types.Memory_Kind;

                  Virt_Mem_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Subj_Memory,
                       Index => J);
                  Phys_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Virt_Mem_Node,
                       Name => "physical");
                  Phys_Mem_Node : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Policy.Doc,
                       XPath => "/system/memory/memory[@name='" & Phys_Name
                       & "']");
                  Mem_Type : constant Mutools.Types.Memory_Kind
                    := Mutools.Types.Memory_Kind'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Mem_Node,
                          Name => "type"));
               begin
                  if Mem_Type = Mutools.Types.Subject_Channel then
                     declare
                        Address : constant Interfaces.Unsigned_64
                          := Interfaces.Unsigned_64'Value
                            (DOM.Core.Elements.Get_Attribute
                               (Elem => Virt_Mem_Node,
                                Name => "virtualAddress"));
                        Writable : constant Boolean
                          := Boolean'Value
                            (DOM.Core.Elements.Get_Attribute
                               (Elem => Virt_Mem_Node,
                                Name => "writable"));
                        Size : constant Interfaces.Unsigned_64
                          := Interfaces.Unsigned_64'Value
                            (DOM.Core.Elements.Get_Attribute
                               (Elem => Phys_Mem_Node,
                                Name => "size"));
                        Event_ID_Str : constant String
                          := Muxml.Utils.Get_Attribute
                            (Doc   => Subj_Node,
                             XPath => "events/source/group/event/"
                             & "notify[@physical='" & Phys_Name & "']/..",
                             Name  => "id");
                        Has_Event : constant Boolean
                          := Event_ID_Str'Length > 0;
                        Event_Nr : Musinfo.Event_Number_Range
                          := Musinfo.Event_Number_Range'First;
                        Vector_Str : constant String
                          := Muxml.Utils.Get_Attribute
                            (Doc   => Subj_Node,
                             XPath => "events/target/event[@physical='"
                             & Phys_Name & "']",
                             Name  => "vector");
                        Has_Vector : constant Boolean := Vector_Str'Length > 0;
                        Vector : Musinfo.Vector_Range
                          := Musinfo.Vector_Range'First;
                     begin
                        if Has_Event then
                           Event_Nr := Musinfo.Event_Number_Range'Value
                             (Event_ID_Str);
                        end if;

                        if Has_Vector then
                           Vector := Musinfo.Vector_Range'Value (Vector_Str);
                        end if;

                        Mulog.Log
                          (Msg => "Announcing channel to subject '" & Subj_Name
                           & "': " & Phys_Name & "@"
                           & Mutools.Utils.To_Hex (Number => Address) & ", "
                           & "size " & Mutools.Utils.To_Hex (Number => Size)
                           & ", "
                           & (if Writable then "writable" else "read-only")
                           & (if Has_Event then
                                  ", event " & Event_ID_Str
                             else "")
                           & (if Has_Vector then
                                  ", vector " & Vector_Str
                             else ""));

                        Musinfo.Utils.Append_Channel
                          (Info    => Subject_Info,
                           Channel => Musinfo.Utils.Create_Channel
                             (Name       => Musinfo.Utils.Create_Name
                                  (Str => Phys_Name),
                              Address    => Address,
                              Size       => Size,
                              Writable   => Writable,
                              Has_Event  => Has_Event,
                              Has_Vector => Has_Vector,
                              Event      => Event_Nr,
                              Vector     => Vector));
                     end;
                  end if;
               end;
            end loop;

            Mulog.Log (Msg => "Writing subject info data to '" & Filename
                       & "'");
            Musinfo.Writer.Serialize
              (Info     => Subject_Info,
               Filename => Filename);
         end;
      end loop;
   end Write;

end Sinfo.Generator;
