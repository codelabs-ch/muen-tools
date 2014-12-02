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
with Mutools.XML_Utils;

package body Mucfgcheck.Scheduling
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure Barrier_ID (XML_Data : Muxml.XML_Data_Type)
   is
      Majors : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/scheduling/majorFrame");
   begin
      Mulog.Log (Msg => "Checking barrier ID(s) in" & DOM.Core.Nodes.Length
                 (List => Majors)'Img & " scheduling major frame(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Majors) - 1 loop
         declare
            Major         : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Majors,
                 Index => I);
            Barriers      : constant DOM.Core.Node_List
              := XPath_Query (N     => Major,
                              XPath => "barriers/barrier");
            Barrier_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Barriers);
            IDs_Present   : array (1 .. Barrier_Count) of Boolean
              := (others => False);
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Barriers) - 1 loop
               declare
                  Barrier    : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Barriers,
                       Index => J);
                  Barrier_ID : constant Positive := Positive'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Barrier,
                        Name => "id"));
               begin
                  if Barrier_ID > Barrier_Count then
                     raise Validation_Error with "Barrier of major frame"
                       & I'Img & " has invalid ID" & Barrier_ID'Img
                       & ", must be in range 1 .." & Barrier_Count'Img;
                  end if;

                  if IDs_Present (Barrier_ID) then
                     raise Validation_Error with "Major frame" & I'Img
                       & " has multiple barriers with ID" & Barrier_ID'Img;
                  end if;

                  IDs_Present (Barrier_ID) := True;
               end;
            end loop;
         end;
      end loop;
   end Barrier_ID;

   -------------------------------------------------------------------------

   procedure Barrier_Size (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count     : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Barriers      : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/scheduling/majorFrame/barriers/barrier");
      Barrier_Count : constant Natural := DOM.Core.Nodes.Length
        (List => Barriers);
   begin
      if Barrier_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking size of" & Barrier_Count'Img
                 & " minor frame barrier(s)");

      for I in 0 .. Barrier_Count - 1 loop
         declare
            Barrier : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Barriers,
                 Index => I);
            Size    : constant Natural := Natural'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Barrier,
                  Name => "size"));
         begin
            if Size > CPU_Count then
               raise Validation_Error with "Minor frame barrier with invalid"
                 & " size" & Size'Img & ", must not exceed" & CPU_Count'Img;
            end if;
         end;
      end loop;
   end Barrier_Size;

   -------------------------------------------------------------------------

   procedure CPU_Element_Count (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
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
               raise Validation_Error with "CPU element count"
                 & DOM.Core.Nodes.Length (List => CPUs)'Img
                 & " of major frame" & Natural'Image (I + 1) & " invalid,"
                 & " active CPU count is" & CPU_Count'Img;
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
                  raise Validation_Error with "CPU" & J'Img & " of major frame"
                    & I'Img & " specifies invalid tick count" & CPU_Ticks'Img
                    & ", should be" & Ref_Ticks'Img;
               end if;
            end loop;
         end;
      end loop;
   end Major_Frame_Ticks;

   -------------------------------------------------------------------------

   procedure Minor_Frame_Barrier_Refs (XML_Data : Muxml.XML_Data_Type)
   is
      Majors : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/scheduling/majorFrame");
   begin
      Mulog.Log (Msg => "Checking barrier references in"
                 & DOM.Core.Nodes.Length (List => Majors)'Img
                 & " scheduling major frame(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Majors) - 1 loop
         declare
            Major_Frame   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Majors,
                 Index => I);
            Barriers      : constant DOM.Core.Node_List
              := XPath_Query (N     => Major_Frame,
                              XPath => "barriers/barrier");
            Barrier_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Barriers);
            Barrier_Refs  : array (1 .. Barrier_Count) of Natural
              := (others => 0);
            Minor_Frames  : constant DOM.Core.Node_List
              := XPath_Query (N     => Major_Frame,
                              XPath => "cpu/minorFrame");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Minor_Frames) - 1 loop
               declare
                  Minor_Frame : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Minor_Frames,
                                            Index => J);
                  Barrier_Ref : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Minor_Frame,
                       Name => "barrier");
               begin
                  if Barrier_Ref /= "none" then
                     declare
                        Ref_Idx : constant Natural
                          := Natural'Value (Barrier_Ref);
                     begin
                        if Ref_Idx > Barrier_Count then
                           declare
                              CPU_ID : constant String
                                := DOM.Core.Elements.Get_Attribute
                                  (Elem => DOM.Core.Nodes.Parent_Node
                                     (N => Minor_Frame),
                                   Name => "id");
                           begin
                              raise Validation_Error with "Minor frame" & J'Img
                                & " of CPU " & CPU_ID & " in major frame"
                                & I'Img & " references invalid barrier"
                                & Ref_Idx'Img & ", must be less than"
                                & Barrier_Count'Img;
                           end;
                        end if;

                        Barrier_Refs (Ref_Idx) := Barrier_Refs (Ref_Idx) + 1;
                     end;
                  end if;
               end;
            end loop;

            for J in 1 .. Barrier_Count loop
               declare
                  Barrier      : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Barriers,
                                            Index => J - 1);
                  Barrier_ID   : constant Positive := Positive'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Barrier,
                        Name => "id"));
                  Barrier_Size : constant Natural
                    := Natural'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Barrier,
                          Name => "size"));
               begin
                  if Barrier_Size /= Barrier_Refs (Barrier_ID) then
                     raise Validation_Error with "References to barrier"
                       & Barrier_ID'Img & " of major frame" & I'Img
                       & " do not match barrier size:"
                       & Barrier_Refs (Barrier_ID)'Img
                       & " /=" & Barrier_Size'Img;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Minor_Frame_Barrier_Refs;

   -------------------------------------------------------------------------

   procedure Minor_Frame_Sync_Points (XML_Data : Muxml.XML_Data_Type)
   is
      Majors : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/scheduling/majorFrame");
   begin
      Mulog.Log (Msg => "Checking barrier count in" & DOM.Core.Nodes.Length
                 (List => Majors)'Img & " scheduling major frame(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Majors) - 1 loop
         declare
            Major_Frame   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Majors,
                 Index => I);
            Barriers      : constant DOM.Core.Node_List
              := XPath_Query (N     => Major_Frame,
                              XPath => "barriers/barrier");
            Deadlines     : constant Mutools.XML_Utils.Deadline_Array
              := Mutools.XML_Utils.Get_Minor_Frame_Deadlines
                (Major => Major_Frame);
            End_Ticks     : constant Interfaces.Unsigned_64
              := Deadlines (Deadlines'Last).Exit_Time;

            Sync_Points   : array (Deadlines'Range) of Positive
              := (others => 1);
            Cur_Idx       : Natural                := Sync_Points'First;
            Prev_Deadline : Interfaces.Unsigned_64 := 0;
         begin
            for Deadline of Deadlines loop
               if Deadline.Exit_Time /= End_Ticks
                 and then Deadline.Exit_Time = Prev_Deadline
               then

                  --  Current and previous minor frame have same deadline.

                  Sync_Points (Cur_Idx) := Sync_Points (Cur_Idx) + 1;
               else
                  if Sync_Points (Cur_Idx) > 1 then

                     --  Current minor frame has new deadline but preceding
                     --  minor frames had same exit time so move to next
                     --  synchronization point.

                     Cur_Idx := Cur_Idx + 1;
                  end if;
               end if;
               Prev_Deadline := Deadline.Exit_Time;
            end loop;

            declare
               Sync_Point_Count : Natural := 0;
            begin
               for S of Sync_Points loop
                  if S > 1 then
                     Sync_Point_Count := Sync_Point_Count + 1;
                  end if;
               end loop;

               if Sync_Point_Count /= DOM.Core.Nodes.Length (List => Barriers)
               then
                  raise Validation_Error with "Major frame" & I'Img & " has "
                    & "invalid barrier count" & DOM.Core.Nodes.Length
                    (List => Barriers)'Img & ", should be"
                    & Sync_Point_Count'Img;
               end if;
            end;

            for J in 1 .. DOM.Core.Nodes.Length (List => Barriers) loop
               declare
                  Barrier      : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Barriers,
                       Index => J - 1);
                  Barrier_ID   : constant Positive := Positive'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Barrier,
                        Name => "id"));
                  Barrier_Size : constant Positive := Positive'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Barrier,
                        Name => "size"));
               begin
                  if Barrier_Size /= Sync_Points (Barrier_ID) then
                     raise Validation_Error with "Barrier" & Barrier_ID'Img
                       & " of " & "major frame" & I'Img & " has "
                       & "invalid size" & Barrier_Size'Img & ", should be"
                       & Sync_Points (Barrier_ID)'Img;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Minor_Frame_Sync_Points;

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
         Subject      : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc     => XML_Data.Doc,
            XPath   => "/system/subjects/subject[@name='" & Subj_Name & "']");
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
