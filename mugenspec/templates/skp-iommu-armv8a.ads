with SK;

with System;

package Skp.IOMMU
  with
    SPARK_Mode => On
is

   --------------------
   -- Register Types --
   --------------------
   type SMMU500_Controller_Configuration_Type
   is record
      Stream_Identifier  : SK.Bit_15_Type;
      Valid_Entry        : SK.Bit_Type;
      Context_Bank_Index : SK.Bit_8_Type;
      Reserved_24_31     : SK.Bit_8_Type;
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
      TTBR_Base_Address          : SK.Bit_48_Type;
      TTBR_Memory_Size_Offset    : SK.Bit_6_Type;
      TTBR_Starting_Level        : SK.Bit_2_Type;
      TTBR_Physical_Address_Size : SK.Bit_3_Type;
      VM_Identifier              : SK.Bit_5_Type;
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
     := (__smmu_controller_config__);

   SMMU_Context_Configuration : constant SMMU500_Context_Configuration
     := (__smmu_context_config__);

end Skp.IOMMU;
