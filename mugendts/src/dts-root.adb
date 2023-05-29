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

with Muxml.Utils;

with String_Templates;

--  with Node_Utils;
--  with Mulog;

package body DTS.Root
is

   ------------------------
   --  Add_Aliases_Node  --
   ------------------------
   procedure Add_Aliases_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node)
   is
      pragma Unreferenced (Policy, Subject);
   begin
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__serial_alias__",
         Content  => "serial_0");
   end Add_Aliases_Node;

   -----------------------
   --  Add_Chosen_Node  --
   -----------------------
   procedure Add_Chosen_Node
     (Template : in out Mutools.Templates.Template_Type;
      Policy   :        Muxml.XML_Data_Type;
      Subject  :        DOM.Core.Node)
   is
      pragma Unreferenced (Policy);
   begin
      Mutools.Templates.Replace
        (Template => Template,
         Pattern  => "__chosen_bootparams__",
         Content  => Muxml.Utils.Get_Element_Value
           (Doc   => Subject,
            XPath => "bootparams"));
   end Add_Chosen_Node;

   -------------
   --  Write  --
   -------------
   procedure Write
     (Policy       : Muxml.XML_Data_Type;
      Subject      : DOM.Core.Node;
      Subject_Name : String;
      Filename     : String)
   is
      pragma Unreferenced (Subject_Name);
      Template : Mutools.Templates.Template_Type
        := Mutools.Templates.Create
          (Content => String_Templates.devicetree_dsl);
   begin
      Add_Aliases_Node (Template => Template,
                        Policy   => Policy,
                        Subject  => Subject);
      Add_Chosen_Node (Template => Template,
                       Policy   => Policy,
                       Subject  => Subject);

      Mutools.Templates.Write (Template => Template,
                               Filename => Filename);
   end Write;

end DTS.Root;
