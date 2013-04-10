with Ada.Text_IO;
with Ada.Command_Line;

with SK.Utils;

with Skc.Subjects;

procedure Skconfig
is
   --  Print config tool usage.
   procedure Print_Usage;
   procedure Print_Usage
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name & " <binary> <memory address>");
   end Print_Usage;

   Binary : Skc.Subjects.Binary_Type;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Print_Usage;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   declare
      Path : constant String := Ada.Command_Line.Argument (1);
      Addr : constant SK.Word64
        := SK.Word64'Value (Ada.Command_Line.Argument (2));
   begin
      Ada.Text_IO.Put_Line (Item => "Subject '" & Path & "'");
      Binary := Skc.Subjects.Read (Binary => Path);

      Ada.Text_IO.Put_Line (Item => "  Entry " & SK.Utils.To_Hex
                            (Item => Binary.Entry_Point));
      Ada.Text_IO.Put_Line (Item => "  Stack " & SK.Utils.To_Hex
                            (Item => Binary.Stack_Address));
      Skc.Subjects.Write (XML_File => Path & ".xml",
                          Subject  => Binary);
      Ada.Text_IO.Put_Line (Item => "Wrote XML spec to '" & Path & ".xml'");

      Skc.Subjects.Write_Memory_Layout
        (XML_File      => Path & "_mem.xml",
         Binary        => Path,
         Start_Address => Addr);
      Ada.Text_IO.Put_Line
        (Item => "Wrote XML memory layout to '" & Path & "_mem.xml'");
   end;
end Skconfig;
