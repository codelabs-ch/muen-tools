with Interfaces;

with System;

package SKP.IOMMU
  with
    SPARK_Mode => On
is

   use Interfaces;

   --------------------
   -- Register Types --
   --------------------
   type SMMU500_Controller_Configuration_Type
   is record
      Stream_Identifier  : Bits_15;
      Valid_Entry        : Bit;
      Context_Bank_Index : Bits_8;
      Reserved_24_31     : Bits_8;
   end record
     with
       Size      => 32,
       Bit_Order => System.Low_Order_First;

   for SMMU500_Controller_Configuration_Type
   use record
      Stream_Identifier  at 0 range  0 .. 14;
      Valid_Entry        at 0 range 15 .. 15;
      Context_Bank_Index at 0 range 16 .. 23;
      Reserved_24_31     at 0 range 24 .. 31;
   end record;

   Null_SMMU500_Controller_Configuration : constant
     SMMU500_Controller_Configuration_Type
       := (Stream_Identifier  => 16#0000#,
           Valid_Entry        =>     2#0#,
           Context_Bank_Index =>   16#00#,
           Reserved_24_31     =>   16#00#);

   type SMMU500_Context_Configuration_Type
   is record
      TTBR_Base_Address          : Bits_48;
      TTBR_Memory_Size_Offset    : Bits_6;
      TTBR_Starting_Level        : Bits_2;
      TTBR_Physical_Address_Size : Bits_3;
      VM_Identifier              : Bits_5;
   end record
     with
       Size      => 64,
       Bit_Order => System.Low_Order_First;

   for SMMU500_Context_Configuration_Type
   use record
      TTBR_Base_Address           at 0 range  0 .. 47;
      TTBR_Memory_Size_Offset     at 0 range 48 .. 53;
      TTBR_Starting_Level         at 0 range 54 .. 55;
      TTBR_Physical_Address_Size  at 0 range 56 .. 58;
      VM_Identifier               at 0 range 59 .. 63;
   end record;

   Null_SMMU500_Context_Configuration : constant
     SMMU500_Context_Configuration_Type
       := (TTBR_Base_Address          => 16#0000_0000_0000#,
           TTBR_Memory_Size_Offset    =>          2#000000#,
           TTBR_Starting_Level        =>              2#00#,
           TTBR_Physical_Address_Size =>             2#000#,
           VM_Identifier              =>             16#00#);

   ----------------------------
   -- SMMU500 Configurations --
   ----------------------------
   type SMMU_Stream_Mapping_ID_Config is new Natural range 0 ..__stream_mapping_id_max__;
   type SMMU_Context_Bank_ID_Config   is new Natural range 0 ..__context_bank_id_max__;

   type SMMU500_Controller_Configuration is
     array (SMMU_Stream_Mapping_ID_Config'Range)
     of SMMU500_Controller_Configuration_Type;

   type SMMU500_Context_Configuration is array
     (SMMU_Context_Bank_ID_Config'Range)
     of SMMU500_Context_Configuration_Type;

   SMMU_Controller_Configuration : constant SMMU500_Controller_Configuration
     := (__smmu_controller_config__)
     with
       Linker_Section => ".globalrodata";

   SMMU_Context_Configuration : constant SMMU500_Context_Configuration
     := (__smmu_context_config__)
     with
       Linker_Section => ".globalrodata";

end SKP.IOMMU;
