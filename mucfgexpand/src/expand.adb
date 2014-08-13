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

with Mulog;

with Expand.Pre_Checks;
with Expand.Post_Checks;
with Expanders;

package body Expand
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String)
   is
   begin
      Pre_Checks.Register_All;
      Mulog.Log
        (Msg => "Registered pre-checks" & Pre_Checks.Get_Count'Img);
      Post_Checks.Register_All;
      Mulog.Log
        (Msg => "Registered post-checks" & Post_Checks.Get_Count'Img);
      Expanders.Register_All;
      Mulog.Log
        (Msg => "Registered expanders" & Expanders.Get_Count'Img);

      Pre_Checks.Run (Data => Policy);
      Expanders.Run (Data => Policy);
      Post_Checks.Run (Data => Policy);

      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_A,
         Data => Policy);

      Pre_Checks.Clear;
      Expanders.Clear;
      Post_Checks.Clear;

   exception
      when others =>
         Pre_Checks.Clear;
         Expanders.Clear;
         Post_Checks.Clear;
         raise;
   end Run;

end Expand;
