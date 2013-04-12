with SK.Utils;

package body Skp.Validators
is

   use type SK.Word64;

   -------------------------------------------------------------------------

   procedure Validate (Policy : Policy_Type)
   is
      One_Megabyte   : constant SK.Word64 := 16#100000#;
      VMCS_Area_Size : constant SK.Word64
        := SK.Word64 (Policy.Subjects.Length) * SK.Page_Size;
   begin
      if Policy.Vmxon_Address mod SK.Page_Size /= 0 then
         raise Validation_Error with "Invalid VMXON address "
           & SK.Utils.To_Hex (Item => Policy.Vmxon_Address)
           & " - address must be 4k aligned";
      end if;

      if Policy.Vmxon_Address > (One_Megabyte - SK.Page_Size) then
         raise Validation_Error with "Invalid VMXON address "
           & SK.Utils.To_Hex (Item => Policy.Vmxon_Address)
           & " - address must be below 1m";
      end if;

      if Policy.Vmcs_Start_Address mod SK.Page_Size /= 0 then
         raise Validation_Error with "Invalid VMCS start address "
           & SK.Utils.To_Hex (Item => Policy.Vmcs_Start_Address)
           & " - address must be 4k aligned";
      end if;

      if Policy.Vmcs_Start_Address + VMCS_Area_Size > One_Megabyte then
         raise Validation_Error with "Invalid VMCS start address "
           & SK.Utils.To_Hex (Item => Policy.Vmcs_Start_Address)
           & " - address must be below 1m - 4k *" & Policy.Subjects.Length'Img;
      end if;

      Validate_Kernel (K => Policy.Kernel);
   end Validate;

   -------------------------------------------------------------------------

   procedure Validate_Device (D : Device_Type)
   is
   begin
      Validate_Mem_Layout (L => D.Memory_Layout);
   end Validate_Device;

   -------------------------------------------------------------------------

   procedure Validate_Kernel (K : Kernel_Type)
   is
   begin
      Validate_Mem_Layout (L => K.Memory_Layout);
   end Validate_Kernel;

   -------------------------------------------------------------------------

   procedure Validate_Mem_Layout (L : Memory_Layout_Type)
   is
      Pos : Memregion_Package.Cursor := L.First;
   begin
      while Memregion_Package.Has_Element (Position => Pos) loop
         Validate_Mem_Region (R => Memregion_Package.Element
                              (Position => Pos));
         Memregion_Package.Next (Position => Pos);
      end loop;
   end Validate_Mem_Layout;

   -------------------------------------------------------------------------

   procedure Validate_Mem_Region (R : Memory_Region_Type)
   is
   begin
      if R.Size mod R.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region size "
           & SK.Utils.To_Hex (Item => R.Size)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => R.Alignment);
      end if;

      if R.Physical_Address mod R.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region physical address "
           & SK.Utils.To_Hex (Item => R.Physical_Address)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => R.Alignment);
      end if;

      if R.Virtual_Address mod R.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region virtual address "
           & SK.Utils.To_Hex (Item => R.Virtual_Address)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => R.Alignment);
      end if;
   end Validate_Mem_Region;

end Skp.Validators;
