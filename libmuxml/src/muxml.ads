--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Ada.Finalization;

package Muxml
is

   --  DOM tree of XML document.
   type XML_Data_Type is new Ada.Finalization.Limited_Controlled with record
      Doc : DOM.Core.Document;
   end record;

   --  Parse the contents of given file into the DOM data structure. The XML
   --  data is validated against the built-in system policy XML schema.
   procedure Parse
     (Data : out XML_Data_Type;
      File :     String);

   --  Write the given DOM data structure to an XML file.
   procedure Write
     (Data : XML_Data_Type;
      File : String);

   Processing_Error : exception;

private

   --  Free XML document.
   overriding
   procedure Finalize (Object : in out XML_Data_Type);

end Muxml;
