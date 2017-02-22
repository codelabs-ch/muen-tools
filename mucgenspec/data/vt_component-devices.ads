pragma Style_Checks (Off);

package Vt_Component.Devices
is

   Storage_Device_Ctrl_Irq1 : constant := 120;
   Storage_Device_Ctrl_Irq2 : constant := 121;

   Storage_Device_Mmio1_Address    : constant := 16#1000_f000#;
   Storage_Device_Mmio1_Size       : constant := 16#4000#;
   Storage_Device_Mmio1_Executable : constant Boolean := False;
   Storage_Device_Mmio1_Writable   : constant Boolean := True;

   Storage_Device_Mmio2_Address    : constant := 16#2000_f000#;
   Storage_Device_Mmio2_Size       : constant := 16#1000#;
   Storage_Device_Mmio2_Executable : constant Boolean := False;
   Storage_Device_Mmio2_Writable   : constant Boolean := True;

   Storage_Device_Port1_Start : constant := 16#03f8#;
   Storage_Device_Port1_End   : constant := 16#03ff#;

   Storage_Device_Port2_Start : constant := 16#02f8#;
   Storage_Device_Port2_End   : constant := 16#02ff#;

   Storage_Device_Port3_Start : constant := 16#03e8#;
   Storage_Device_Port3_End   : constant := 16#03ef#;

   Usb_Device_Irq : constant := 22;

   Usb_Device_Mmio_Address    : constant := 16#1000#;
   Usb_Device_Mmio_Size       : constant := 16#5000#;
   Usb_Device_Mmio_Executable : constant Boolean := False;
   Usb_Device_Mmio_Writable   : constant Boolean := True;

end Vt_Component.Devices;
