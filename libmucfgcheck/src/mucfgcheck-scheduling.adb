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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

package body Mucfgcheck.Scheduling
is

   use McKae.XML.XPath.XIA;

   --  Returns True if the minor frame subject name matches.
   function Match_Subject_Name (Left, Right : DOM.Core.Node) return Boolean;

   -------------------------------------------------------------------------

   procedure CPU_Element_Count (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
               (Doc   => XML_Data.Doc,
                XPath => "/system/platform/processor",
                Name  => "logicalCpus"));
      Majors    : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/scheduling/majorFrame");
   begin
      Mulog.Log (Msg => "Checking CPU element count in" & DOM.Core.Nodes.Length
                 (List => Majors)'Img & " scheduling major frame(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Majors) - 1 loop
         declare
            CPUs : constant DOM.Core.Node_List
              := XPath_Query (N     => DOM.Core.Nodes.Item
                              (List  => Majors,
                               Index => I),
                              XPath => "cpu");
         begin
            if DOM.Core.Nodes.Length (List => CPUs) /= CPU_Count then
               raise Validation_Error with "CPU element count of"
                 & DOM.Core.Nodes.Length (List => CPUs)'Img & " in scheduling "
                 & "plan invalid, logical CPU count is" & CPU_Count'Img;
            end if;
         end;
      end loop;
   end CPU_Element_Count;

   -------------------------------------------------------------------------

   procedure Major_Frame_Ticks (XML_Data : Muxml.XML_Data_Type)
   is
      Ref_Ticks : Natural;
      Majors    : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/scheduling/majorFrame");
   begin
      Mulog.Log (Msg => "Checking tick count in" & DOM.Core.Nodes.Length
                 (List => Majors)'Img & " scheduling major frame(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Majors) - 1 loop
         Ref_Ticks := 0;

         declare
            CPU_Ticks : Natural;
            CPUs      : constant DOM.Core.Node_List
              := XPath_Query (N     => DOM.Core.Nodes.Item
                              (List  => Majors,
                               Index => I),
                              XPath => "cpu");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
               CPU_Ticks := 0;

               declare
                  Minors : constant DOM.Core.Node_List
                    := XPath_Query (N     => DOM.Core.Nodes.Item
                                    (List  => CPUs,
                                     Index => J),
                                    XPath => "minorFrame/@ticks");
               begin
                  for K in 0 .. DOM.Core.Nodes.Length (List => Minors) - 1 loop
                     CPU_Ticks := CPU_Ticks + Positive'Value
                       (DOM.Core.Nodes.Node_Value
                          (N => DOM.Core.Nodes.Item
                             (List  => Minors,
                              Index => K)));
                  end loop;
               end;

               if Ref_Ticks = 0 then
                  Ref_Ticks := CPU_Ticks;
               elsif Ref_Ticks /= CPU_Ticks then
                  raise Validation_Error with "Invalid CPU elements in "
                    & "scheduling plan, tick counts differ";
               end if;
            end loop;
         end;
      end loop;
   end Major_Frame_Ticks;

   -------------------------------------------------------------------------

   function Match_Subject_Name (Left, Right : DOM.Core.Node) return Boolean
   is
      Frame_Name   : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "subject");
      Subject_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "name");
   begin
      return Frame_Name = Subject_Name;
   end Match_Subject_Name;

   -------------------------------------------------------------------------

   procedure Subject_CPU_Affinity (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Returns True if the minor frame CPU id matches.
      function Match_CPU_ID (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Frame_CPU_ID : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
               Name => "id");
         Subj_Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "subject");
         Subject      : constant DOM.Core.Node := DOM.Core.Nodes.Item
             (List  => XPath_Query
                  (N     => XML_Data.Doc,
                   XPath => "/system/subjects/subject[@name='" & Subj_Name
                   & "']"),
              Index => 0);
         Subj_CPU_ID  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "cpu");
      begin
         return "Subject '" & Subj_Name & "' scheduled on wrong CPU "
           & Frame_CPU_ID & ", should be " & Subj_CPU_ID;
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_CPU_ID (Left, Right : DOM.Core.Node) return Boolean
      is
         Frame_CPU_ID   : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Parent_Node (N => Left),
               Name => "id"));
         Subject_CPU_ID : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "cpu"));
      begin
         return Frame_CPU_ID = Subject_CPU_ID and then
           Match_Subject_Name (Left  => Left,
                               Right => Right);
      end Match_CPU_ID;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/scheduling/majorFrame/cpu/minorFrame",
         Ref_XPath    => "/system/subjects/subject",
         Log_Message  => "minor frame(s) for subject CPU affinity",
         Error        => Error_Msg'Access,
         Match        => Match_CPU_ID'Access);
   end Subject_CPU_Affinity;

   -------------------------------------------------------------------------

   procedure Subject_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "subject");
      begin
         return "Subject '" & Subj_Name
           & "' referenced in scheduling plan not found";
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/scheduling/majorFrame/cpu/minorFrame",
         Ref_XPath    => "/system/subjects/subject",
         Log_Message  => "subject reference(s) in scheduling plan",
         Error        => Error_Msg'Access,
         Match        => Match_Subject_Name'Access);
   end Subject_References;

end Mucfgcheck.Scheduling;
