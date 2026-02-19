package body Skp.Subjects
  with
    SPARK_Mode => On
is

   type Subject_Spec_Type is record
      CPU_ID               : CPU_Range;
      GPR_0                : SK.Bit_64_Type;
      ELR_EL2              : SK.Bit_64_Type;
      VTTBR_Address        : SK.Bit_48_Type;
      Default_Cacheability : Boolean;
      Trap_WFI_Instruction : Boolean;
      Trap_WFE_Instruction : Boolean;
      Interrupt_Support    : Boolean;
   end record;

   type Subject_Spec_Array is array (Global_Subject_ID_Type)
     of Subject_Spec_Type;

   Subject_Specs : constant Subject_Spec_Array
     := (__subjects__);

   -------------------------------------------------------------------------

   function Get_CPU_ID
     (Subject_ID : Global_Subject_ID_Type)
      return CPU_Range
   is (Subject_Specs (Subject_ID).CPU_ID);

   -------------------------------------------------------------------------

   function Get_General_Purpose_Register
     (Subject_ID : Global_Subject_ID_Type;
      Idx        : Natural)
      return SK.Bit_64_Type
   is (if Idx = 0 then Subject_Specs (Subject_ID).GPR_0 else 0);

   -------------------------------------------------------------------------

   function Get_Exception_Link_Register_EL2
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Bit_64_Type
   is (Subject_Specs (Subject_ID).ELR_EL2);

   -------------------------------------------------------------------------

   function Get_VTTBR_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Bit_48_Type
   is (Subject_Specs (Subject_ID).VTTBR_Address);

   -------------------------------------------------------------------------

   function Has_Default_Cacheability
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean
   is (Subject_Specs (Subject_ID).Default_Cacheability);

   -------------------------------------------------------------------------

   function Traps_WFI_Instruction
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean
   is (Subject_Specs (Subject_ID).Trap_WFI_Instruction);

   -------------------------------------------------------------------------

   function Traps_WFE_Instruction
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean
   is (Subject_Specs (Subject_ID).Trap_WFE_Instruction);

   -------------------------------------------------------------------------

   function Accepts_Interrupts
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean
   is (Subject_Specs (Subject_ID).Interrupt_Support);

end Skp.Subjects;
