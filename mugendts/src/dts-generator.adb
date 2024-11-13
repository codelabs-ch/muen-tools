--
--  Copyright (C) 2023, 2023  David Loosli <david@codelabs.ch>
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

with DOM.Core;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;

with DTS.Root;

package body DTS.Generator
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      --  (1) extract all physical memory regions that contain a linux
      --  device tree (i.e. type of 'subject_devicetree')
      Physical_DTS : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='subject_devicetree']");
   begin
      --  (2) for every physical device tree memory node find the corresponding
      --  subject that exclusively makes use of this dts region to unambiguous-
      --  ly identify a Linux VM subject
      for I in 0 .. DOM.Core.Nodes.Length (Physical_DTS) - 1 loop
         declare
            DTS_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Physical_DTS,
                                      Index => I);
            DTS_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => DTS_Node,
                                                  Name => "name");

            -- (3) extract all subjects that the current dts region is assigned
            -- to; the resulting node list has to contain exactly one Linux VM
            -- subject
            Linux_Subjects : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/subjects/subject[memory/memory/" &
                   "@physical='" & DTS_Name & "']");
         begin
            if DOM.Core.Nodes.Length (Linux_Subjects) = 1 then
               declare
                  Subject      : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Linux_Subjects,
                                            Index => 0);

                  Subject_Name : constant String
                    := DOM.Core.Elements.Get_Attribute (Elem => Subject,
                                                        Name => "name");

                  Subject_ID   : constant String
                    := DOM.Core.Elements.Get_Attribute (Elem => Subject,
                                                        Name => "globalId");

                  DTS_Filename : constant String
                    := Output_Dir & "/devicetree_" & Subject_Name & ".dts";
               begin
                  Mulog.Log (Level => Mulog.Info,
                             Msg   => "Writing device tree for subject '" &
                               Subject_Name & "' with id '" & Subject_ID &
                               "' to '" & DTS_Filename & "'");

                  DTS.Root.Write (Policy       => Policy,
                                  Subject      => Subject,
                                  Subject_Name => Subject_Name,
                                  Filename     => DTS_Filename);
               end;
            end if;
         end;
      end loop;
   end Write;

end DTS.Generator;
