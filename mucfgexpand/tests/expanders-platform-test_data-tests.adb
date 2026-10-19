--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Platform.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with DOM.Core;
with Muxml;
with Muxml.Utils;
--  begin read only
--  end read only
package body Expanders.Platform.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test);
   procedure Test_Add_Section_Skeleton_797fa9 (Gnattest_T : in out Test) renames Test_Add_Section_Skeleton;
--  id:2.2/797fa93bd19d8580/Add_Section_Skeleton/1/0/
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/platform_skeleton.xml",
         Ref_Diff => "data/platform_skeleton.xml.diff",
         Pre      => Remove_Platform_Section'Access,
         Expander => Add_Section_Skeleton'Access);
--  begin read only
   end Test_Add_Section_Skeleton;
--  end read only


--  begin read only
   procedure Test_Resolve_Device_Aliases (Gnattest_T : in out Test);
   procedure Test_Resolve_Device_Aliases_2c46de (Gnattest_T : in out Test) renames Test_Resolve_Device_Aliases;
--  id:2.2/2c46dec9d926880c/Resolve_Device_Aliases/1/0/
   procedure Test_Resolve_Device_Aliases (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/platform_device_alias.xml",
         Ref_Diff => "data/platform_device_alias.xml.diff",
         Expander => Resolve_Device_Aliases'Access);
--  begin read only
   end Test_Resolve_Device_Aliases;
--  end read only


--  begin read only
   procedure Test_Resolve_Device_Classes (Gnattest_T : in out Test);
   procedure Test_Resolve_Device_Classes_0ae48e (Gnattest_T : in out Test) renames Test_Resolve_Device_Classes;
--  id:2.2/0ae48e2c82d97dec/Resolve_Device_Classes/1/0/
   procedure Test_Resolve_Device_Classes (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/platform_device_class.xml",
         Ref_Diff => "data/platform_device_class.xml.diff",
         Expander => Resolve_Device_Classes'Access);

      --  Test handling of empty device classes.

      Test_Utils.Expander.Run_Test
        (Filename => "obj/platform_device_class_empty.xml",
         Ref_Diff => "data/platform_device_class_empty.xml.diff",
         Pre      => Remove_Network_Adapters_Device_Class_Resources'Access,
         Expander => Resolve_Device_Classes'Access);

      --  Test handling of RMRR mapping for device classes.

      Test_Utils.Expander.Run_Test
        (Filename => "obj/platform_device_class_rmrr.xml",
         Ref_Diff => "data/platform_device_class_rmrr.xml.diff",
         Pre      => Map_Reserved_Memory_Xhci_Device_Class'Access,
         Expander => Resolve_Device_Classes'Access);
--  begin read only
   end Test_Resolve_Device_Classes;
--  end read only


--  begin read only
   procedure Test_Add_Subject_Device_Resources (Gnattest_T : in out Test);
   procedure Test_Add_Subject_Device_Resources_9f99c3 (Gnattest_T : in out Test) renames Test_Add_Subject_Device_Resources;
--  id:2.2/9f99c3b0f972885f/Add_Subject_Device_Resources/1/0/
   procedure Test_Add_Subject_Device_Resources (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/platform_add_subject_device_resources.xml",
         Ref_Diff => "data/platform_add_subject_device_resources.xml.diff",
         Pre      => Adjust_Subj_Device_Alias_Resources'Access,
         Expander => Add_Subject_Device_Resources'Access);
--  begin read only
   end Test_Add_Subject_Device_Resources;
--  end read only


--  begin read only
   procedure Test_Resolve_Mitigations (Gnattest_T : in out Test);
   procedure Test_Resolve_Mitigations_58d5c9 (Gnattest_T : in out Test) renames Test_Resolve_Mitigations;
--  id:2.2/58d5c996df743488/Resolve_Mitigations/1/0/
   procedure Test_Resolve_Mitigations (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Override
      is
         use type DOM.Core.Node;

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/policy_mitigations_override.xml");
         Resolve_Mitigations (Data => Policy);

         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations"
                       & "/mcu",
                    Name  => "minRevision") = "16#58#",
                 Message   => "Platform mcu override not applied");
         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations"
                       & "/vmExit/rsbStuffing",
                    Name  => "mode") = "fullFill32",
                 Message   => "Platform rsbStuffing override not applied");
         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations"
                       & "/vmEntry/l1dFlush",
                    Name  => "enable") = "true",
                 Message   => "Platform l1dFlush override not applied");
         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations"
                       & "/init/eibrs",
                    Name  => "enable") = "true",
                 Message   => "Non-overridden init leaf not inherited");
         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations"
                       & "/archCapabilities/require",
                    Name  => "bit") = "RDCL_NO",
                 Message   => "Non-overridden require not inherited");
         Assert (Condition => Muxml.Utils.Get_Element
                   (Doc   => Policy.Doc,
                    XPath => "/system/platform/mitigations") = null,
                 Message   => "Platform mitigations directive not removed");
      end Override;

      ----------------------------------------------------------------------

      procedure Disable
      is
         use type DOM.Core.Node;

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/policy_mitigations_disabled.xml");
         Resolve_Mitigations (Data => Policy);

         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations",
                    Name  => "enable") = "false",
                 Message   => "Platform enable=false not applied");
         Assert (Condition => Muxml.Utils.Get_Element
                   (Doc   => Policy.Doc,
                    XPath => "/system/platform/mitigations") = null,
                 Message   => "Platform mitigations directive not removed");
      end Disable;

      ----------------------------------------------------------------------

      procedure Inherit
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/policy_mitigations_override.xml");
         Muxml.Utils.Remove_Elements
           (Doc   => Policy.Doc,
            XPath => "/system/platform/mitigations");

         Resolve_Mitigations (Data => Policy);

         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations"
                       & "/mcu",
                    Name  => "minRevision") = "16#14#",
                 Message   => "Hardware facts not inherited unchanged");
      end Inherit;

      ----------------------------------------------------------------------

      procedure Override_Explicit_Enable
      is
         use type DOM.Core.Node;

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/policy_mitigations_override_enable.xml");
         Resolve_Mitigations (Data => Policy);

         Assert (Condition => Muxml.Utils.Get_Attribute
                   (Doc   => Policy.Doc,
                    XPath => "/system/hardware/processor/x86_64/mitigations",
                    Name  => "enable") = "true",
                 Message   => "Explicit platform enable not propagated");
         Assert (Condition => Muxml.Utils.Get_Element
                   (Doc   => Policy.Doc,
                    XPath => "/system/platform/mitigations") = null,
                 Message   => "Platform mitigations directive not removed");
      end Override_Explicit_Enable;

   begin
      Override;
      Disable;
      Inherit;
      Override_Explicit_Enable;
--  begin read only
   end Test_Resolve_Mitigations;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Expanders.Platform.Test_Data.Tests;
