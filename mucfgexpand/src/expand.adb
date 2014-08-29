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

with Stage1.Pre_Checks;
with Stage2.Pre_Checks;
with Stage2.Expansion;

with Expand.Post_Checks;

package body Expand
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String)
   is
   begin
      Stage1.Pre_Checks.Register_All;
      Mulog.Log (Msg => "Registered stage 1 pre-checks"
                 & Stage1.Pre_Checks.Get_Count'Img);
      Stage2.Pre_Checks.Register_All;
      Mulog.Log (Msg => "Registered stage 2 pre-checks"
                 & Stage2.Pre_Checks.Get_Count'Img);
      Stage2.Expansion.Register_All;
      Mulog.Log (Msg => "Registered stage 2 expanders"
                 & Stage2.Expansion.Get_Count'Img);

      Post_Checks.Register_All;
      Mulog.Log (Msg => "Registered post-checks" & Post_Checks.Get_Count'Img);

      Mulog.Log (Msg => "STAGE 1 processing");
      Stage1.Pre_Checks.Run (Data => Policy);
      Mulog.Log (Msg => "STAGE 2 processing");
      Stage2.Pre_Checks.Run (Data => Policy);
      Stage2.Expansion.Run (Data => Policy);
      Post_Checks.Run (Data => Policy);

      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_A,
         Data => Policy);

      Stage1.Pre_Checks.Clear;
      Stage2.Pre_Checks.Clear;
      Stage2.Expansion.Clear;
      Post_Checks.Clear;

   exception
      when others =>
         Stage1.Pre_Checks.Clear;
         Stage2.Pre_Checks.Clear;
         Stage2.Expansion.Clear;
         Post_Checks.Clear;
         raise;
   end Run;

end Expand;
