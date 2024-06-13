with Interfaces;

--D @Interface
--D This package contains subject specifications as defined by the system
--D policy. The given values define the configurable part of the subject
--D state for each subject and establish their initial state according to
--D the policy.
package SKP.Subjects
  with
    SPARK_Mode => On
is

   use Interfaces;

   type Subject_Category_Type is (Linux, Native);

   function Get_CPU_ID
     (Subject_ID : Global_Subject_ID_Type)
      return CPU_Range;

   function Get_Subject_Category
     (Subject_ID : Global_Subject_ID_Type)
      return Subject_Category_Type;

   function Get_VTTBR_Address
     (Subject_ID : Global_Subject_ID_Type)
      return Bits_48;

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

end SKP.Subjects;
