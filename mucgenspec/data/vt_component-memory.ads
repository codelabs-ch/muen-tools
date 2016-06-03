package Vt_Component.Memory
is

   Lowmem_Address    : constant := 16#0002_0000#;
   Lowmem_Size       : constant := 16#0008_0000#;
   Lowmem_Executable : constant Boolean := False;
   Lowmem_Writable   : constant Boolean := True;

   Ram_Address    : constant := 16#0100_0000#;
   Ram_Size       : constant := 16#1000_0000#;
   Ram_Executable : constant Boolean := True;
   Ram_Writable   : constant Boolean := True;

   Initramfs_Address    : constant := 16#9000_0000#;
   Initramfs_Size       : constant := 16#0040_0000#;
   Initramfs_Executable : constant Boolean := False;
   Initramfs_Writable   : constant Boolean := False;

end Vt_Component.Memory;
