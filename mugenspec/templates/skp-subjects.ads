with SK;

package Skp.Subjects
is

   type VMX_Controls_Type is record
      Exec_Pin    : SK.Word32;
      Exec_Proc   : SK.Word32;
      Exec_Proc2  : SK.Word32;
      Exit_Ctrls  : SK.Word32;
      Entry_Ctrls : SK.Word32;
   end record;

   function Get_CPU_Id (Subject_Id : Skp.Subject_Id_Type) return Skp.CPU_Range;

   function Get_PML4_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_EPT_Pointer
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_VMCS_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_IO_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_MSR_Bitmap_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_MSR_Store_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_MSR_Count
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word32;

   function Get_Stack_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_Entry_Point
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64;

   function Get_VMX_Controls
     (Subject_Id : Skp.Subject_Id_Type)
      return VMX_Controls_Type;

   function Get_CR0 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CR0_Mask (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CR4 (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CR4_Mask (Subject_Id : Skp.Subject_Id_Type) return SK.Word64;

   function Get_CS_Access (Subject_Id : Skp.Subject_Id_Type) return SK.Word32;

   function Get_Exception_Bitmap
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word32;

end Skp.Subjects;
