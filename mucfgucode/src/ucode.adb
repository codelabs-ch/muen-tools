--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Directories;
with Ada.Direct_IO;

with Interfaces;

with DOM.Core.Nodes;

with Mulog;
with Muxml.Utils;
with Mutools.OS;
with Mutools.Utils;
with Mutools.Constants;
with Mutools.XML_Utils;

package body Ucode
is

   --  Adjust primary processor signature of the given update bundle to the
   --  value specified.
   procedure Adjust_Primary_Processor_Signature
     (File_Path : String;
      Signature : Interfaces.Unsigned_32);

   -------------------------------------------------------------------------

   procedure Adjust_Primary_Processor_Signature
     (File_Path : String;
      Signature : Interfaces.Unsigned_32)
   is
      use Interfaces;

      package Uint32_IO is new Ada.Direct_IO
        (Element_Type => Unsigned_32);

      Old_Sig, Old_Checksum, New_Checksum : Unsigned_32;

      Diff : Integer_64;
      FD   : Uint32_IO.File_Type;
   begin
      Uint32_IO.Open
        (File => FD,
         Mode => Uint32_IO.Inout_File,
         Name => File_Path);

      --  See Intel SDM Vol. 3A, "9.11.1 Microcode Update"
      --  Table 10-7.
      --  Note: The index is the logical record position of Element_Type width,
      --  not bytes and it starts at index 1 not 0.
      Uint32_IO.Set_Index
        (File => FD,
         To   => 4);
      Uint32_IO.Read
        (File => FD,
         Item => Old_Sig);

      Diff := Integer_64 (Old_Sig) - Integer_64 (Signature);

      if Diff /= 0 then
         Uint32_IO.Set_Index
           (File => FD,
            To   => 4);
         Uint32_IO.Write
           (File => FD,
            Item => Signature);
         Uint32_IO.Set_Index
           (File => FD,
            To   => 5);
         Uint32_IO.Read
           (File => FD,
            Item => Old_Checksum);
         New_Checksum := Unsigned_32'Mod (Integer_64 (Old_Checksum) + Diff);
         Uint32_IO.Set_Index
           (File => FD,
            To   => 5);
         Uint32_IO.Write
           (File => FD,
            Item => New_Checksum);

         Mulog.Log (Msg => "Setting primary processor signature to "
           & Mutools.Utils.To_Hex (Number => Unsigned_64 (Signature)) & ", was "
           & Mutools.Utils.To_Hex (Number => Unsigned_64 (Old_Sig)));
      end if;

      Uint32_IO.Close (File => FD);
   end Adjust_Primary_Processor_Signature;

   -------------------------------------------------------------------------

   procedure Run
     (Policy     : String;
      Ucode_Dir  : String;
      Output_Dir : String)
   is
      Data : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing policy '" & Policy & "'");

      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_Src,
                   File => Policy);

      declare
         Sig : constant String := Muxml.Utils.Get_Attribute
             (Doc   => Data.Doc,
              XPath => "/system/hardware/processor/cpuid"
              & "[@leaf='16#0000_0001#']",
              Name  => "eax");
         Sig_C : constant String := Mutools.Utils.To_Hex
             (Number    => Interfaces.Unsigned_64'Value (Sig),
              Normalize => False);
         Filename : constant String := Sig_C & ".ucode";
         Path     : constant String := Output_Dir & "/" & Filename;
      begin
         Mulog.Log (Msg => "Processor signature is " & Sig);
         if Ada.Directories.Exists (Name => Path) then
            Mulog.Log (Msg => "Deleting existing file '" & Path & "'");
            Ada.Directories.Delete_File (Name => Path);
         end if;
         Mutools.OS.Execute
           (Command => "iucode_tool --strict-checks -v " & Ucode_Dir
            & " -s 0x" & Sig_C & " -w " & Path);

         if Ada.Directories.Exists (Name => Path) then
            Adjust_Primary_Processor_Signature
              (File_Path => Path,
               Signature => Interfaces.Unsigned_32'Value (Sig));

            Mutools.OS.Execute (Command => "iucode_tool -l " & Path);
            declare
               use type DOM.Core.Node;
               use type Ada.Directories.File_Size;

               Node : DOM.Core.Node :=
                 Muxml.Utils.Get_Element
                   (Doc   => Data.Doc,
                    XPath =>
                      "/system/memory/memory[@type='kernel_microcode']");
               Size : Ada.Directories.File_Size :=
                 Ada.Directories.Size (Name => Path);
            begin
               if Node /= null then
                  Mulog.Log
                    (Msg => "Removing existing microcode memory region");
                  Node := DOM.Core.Nodes.Remove_Child
                    (N         => Muxml.Utils.Get_Element
                       (Doc   => Data.Doc,
                        XPath => "/system/memory"),
                     Old_Child => Node);
               end if;
               Mulog.Log
                 (Msg => "Adding file-backed memory region for MCU, size"
                  & Size'Img & " bytes");
               Size := Size - (Size mod (-Mutools.Constants.Page_Size));
               Mutools.XML_Utils.Add_Memory_Region
                 (Policy      => Data,
                  Name        => "microcode",
                  Address     => "",
                  Size        =>
                    Mutools.Utils.To_Hex
                      (Number => Interfaces.Unsigned_64 (Size)),
                  Alignment   => "16#1000#",
                  Caching     => "WB",
                  Memory_Type => "kernel_microcode",
                  File_Name   => Filename,
                  File_Offset => "none");
            end;
            Muxml.Write
              (Data => Data,
               Kind => Muxml.Format_Src,
               File => Policy);
            Mulog.Log (Msg => "Policy '" & Policy & "' updated");
         else
            Mulog.Log (Msg => "No matching microcode update");
         end if;
      end;
   end Run;

end Ucode;
