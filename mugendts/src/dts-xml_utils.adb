--
--  Copyright (C) 2026  Reto Buerki <reet@codelabs.ch>
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

with McKae.XML.XPath.XIA;

with Muxml.Utils;
with Mutools.Match;

package body DTS.XML_Utils
is

   -------------------------------------------------------------------------

   function Get_UART_Count
     (Policy  : Muxml.XML_Data_Type;
      Subject : DOM.Core.Node)
      return Natural
   is
      Phys_UARTS : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device" &
             "[capabilities/capability/@name='uart']");
      Subj_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Subject,
           XPath => "devices/device");
      Pairs : Muxml.Utils.Matching_Pairs_Type;
   begin
      Pairs := Muxml.Utils.Get_Matching
        (Left_Nodes     => Subj_Devs,
         Right_Nodes    => Phys_UARTS,
         Match_Multiple => False,
         Match          => Mutools.Match.Is_Valid_Reference'Access);
      return DOM.Core.Nodes.Length (Pairs.Left);
   end Get_UART_Count;

end DTS.XML_Utils;
