with SK;

--D @Interface
--D This package contains subject specifications as defined by the system
--D policy. The given values define the configurable part of the subject
--D state for each subject and establish their initial state according to
--D the policy.
package Skp.Subjects
  with
    SPARK_Mode => On
is

   function Get_CPU_ID
     (Subject_ID : Global_Subject_ID_Type)
      return CPU_Range;

   function Get_General_Purpose_Register
     (Subject_ID : Global_Subject_ID_Type;
      Idx        : Natural)
      return SK.Bit_64_Type;

   function Get_Exception_Link_Register_EL2
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Bit_64_Type;

   function Get_VTTBR_Address
     (Subject_ID : Global_Subject_ID_Type)
      return SK.Bit_48_Type;

   function Has_Default_Cacheability
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean;

   function Traps_WFI_Instruction
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean;

   function Traps_WFE_Instruction
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean;

   function Accepts_Interrupts
     (Subject_ID : Global_Subject_ID_Type)
      return Boolean;

end Skp.Subjects;
