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

private with DOM.Core;

with Muxml;

package Merge.Conditionals
is

   --  Expand all conditionals in the specified policy.
   procedure Expand (Policy : Muxml.XML_Data_Type);

private

   --  Recursively evalute all conditionals of given parent node.
   procedure Evaluate
     (Policy : Muxml.XML_Data_Type;
      Parent : DOM.Core.Node);

end Merge.Conditionals;
