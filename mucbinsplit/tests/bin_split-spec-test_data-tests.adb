--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Spec.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Characters.Handling;
--  begin read only
--  end read only
package body Bin_Split.Spec.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Fill_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Fill_Entry_037de5 (Gnattest_T : in out Test) renames Test_Add_Fill_Entry;
--  id:2.2/037de52ddc72e8bd/Add_Fill_Entry/1/0/
   procedure Test_Add_Fill_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.Component,
                   File => "data/test_cspec.xml");

      Add_Fill_Entry
        (Spec            => Spec,
         Logical         => "section_name",
         Writable        => True,
         Executable      => False,
         Fill_Pattern    => 16#90#,
         Size            => 16#0700#,
         Virtual_Address => 16#0400#);

      Assert
        (Condition =>
           DOM.Core.Nodes.Length
             (DOM.Core.Documents.Get_Elements_By_Tag_Name
                (Doc => Spec.Doc,
                 Tag_Name => "fill"))
             > 0,
         Message   => "Fill entry not created");
--  begin read only
   end Test_Add_Fill_Entry;
--  end read only


--  begin read only
   procedure Test_Add_File_Entry (Gnattest_T : in out Test);
   procedure Test_Add_File_Entry_f30ecc (Gnattest_T : in out Test) renames Test_Add_File_Entry;
--  id:2.2/f30eccc8f250fb29/Add_File_Entry/1/0/
   procedure Test_Add_File_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Filename : constant String := "test";
      Hash     : constant String := "3827eeabc";

      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.Component,
                   File => "data/test_cspec.xml");

      Add_File_Entry
        (Spec            => Spec,
         Logical         => "section_name",
         Writable        => True,
         Executable      => False,
         File_Name       => Filename,
         Size            => 16#0700#,
         Virtual_Address => 16#0400#,
         Hash            => Hash);

      declare
         use type DOM.Core.Node;

         File_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Spec.Doc,
              XPath => "/component/provides/memory"
              & "[@logical='section_name']/file");
         Hash_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Spec.Doc,
              XPath => "/component/provides/memory"
              & "[@logical='section_name']/hash");
      begin
         Assert (Condition => File_Node /= null,
                 Message   => "File entry not created");
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => File_Node,
                  Name => "filename") = Filename,
                 Message   => "Filename mismatch");
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => File_Node,
                  Name => "offset") = "none",
                 Message   => "File offset mismatch");

         Assert (Condition => Hash_Node /= null,
                 Message   => "Hash entry not created");
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Hash_Node,
                  Name => "value") = Hash,
                 Message   => "Hash value mismatch");
      end;
--  begin read only
   end Test_Add_File_Entry;
--  end read only


--  begin read only
   procedure Test_Get_Entry_Point (Gnattest_T : in out Test);
   procedure Test_Get_Entry_Point_60d283 (Gnattest_T : in out Test) renames Test_Get_Entry_Point;
--  id:2.2/60d2838faba0d333/Get_Entry_Point/1/0/
   procedure Test_Get_Entry_Point (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref : array (Mutools.Types.Arch_Type) of Unbounded_String
        := (Mutools.Types.ARM64  => To_Unbounded_String ("16#0042_4000#"),
            Mutools.Types.X86_64 => To_Unbounded_String ("16#cafe_0000#"));

      ----------------------------------------------------------------------

      procedure Check_Get_Entry_Point (Arch : Mutools.Types.Arch_Type)
      is
         Arch_Str : constant String
           := Ada.Characters.Handling.To_Lower (Item => Arch'Img);

         Spec : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Spec,
                      Kind => Muxml.Component,
                      File => "data/test_cspec_rip-" & Arch_Str & ".xml");
         declare
            RIP_Str : constant String := Get_Entry_Point (Spec => Spec,
                                                          Arch => Arch);
         begin
            Assert (Condition => RIP_Str = To_String (Ref (Arch)),
                    Message   => "RIP value mismatch (" & Arch_Str & ")");
         end;

         Muxml.Utils.Remove_Elements (Doc   => Spec.Doc,
                                      XPath => "/component/requires/vcpu");
         Assert (Condition => Get_Entry_Point
                  (Spec => Spec, Arch => Arch)'Length = 0,
                 Message   => "Non-present RIP mismatch (" & Arch_Str & ")");
      end Check_Get_Entry_Point;
   begin
      for A in Mutools.Types.Arch_Type loop
         Check_Get_Entry_Point (Arch => A);
      end loop;
--  begin read only
   end Test_Get_Entry_Point;
--  end read only


--  begin read only
   procedure Test_Set_Entry_Point (Gnattest_T : in out Test);
   procedure Test_Set_Entry_Point_22478f (Gnattest_T : in out Test) renames Test_Set_Entry_Point;
--  id:2.2/22478f27dbe26dbc/Set_Entry_Point/1/0/
   procedure Test_Set_Entry_Point (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      ----------------------------------------------------------------------

      procedure Check_Set_Entry_Point_ARM64
      is
         XPath : constant String
           := "/component/requires/vcpu/arm64/registers/elr_el2";
         Ref_1 : constant Interfaces.Unsigned_64 := 16#beef_cafe#;
         Ref_2 : constant Interfaces.Unsigned_64 := 16#2323_4000#;
         Spec  : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Spec,
                      Kind => Muxml.Component,
                      File => "data/test_cspec.xml");
         Set_Entry_Point (Spec        => Spec,
                          Arch        => Mutools.Types.ARM64,
                          Entry_Point => Ref_1);
         Assert (Condition => Interfaces.Unsigned_64'Value
                 (Muxml.Utils.Get_Element_Value (Doc   => Spec.Doc,
                                                 XPath => XPath)) = Ref_1,
                 Message   => "RIP value mismatch (ARM64: 1)");

         --  Test setting RIP with existing XML elements.

         Set_Entry_Point (Spec        => Spec,
                          Arch        => Mutools.Types.ARM64,
                          Entry_Point => Ref_2);
         Assert (Condition => Interfaces.Unsigned_64'Value
                 (Muxml.Utils.Get_Element_Value (Doc   => Spec.Doc,
                                                 XPath => XPath)) = Ref_2,
                 Message   => "RIP value mismatch (ARM64: 2)");
      end Check_Set_Entry_Point_ARM64;

      ----------------------------------------------------------------------

      procedure Check_Set_Entry_Point_X86_64
      is
        XPath : constant String
           := "/component/requires/vcpu/x86_64/registers/gpr/rip";

         Ref_1 : constant Interfaces.Unsigned_64 := 16#cafe_beef#;
         Ref_2 : constant Interfaces.Unsigned_64 := 16#9090_4242#;
         Spec  : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Spec,
                      Kind => Muxml.Component,
                      File => "data/test_cspec.xml");
         Set_Entry_Point (Spec        => Spec,
                          Arch        => Mutools.Types.X86_64,
                          Entry_Point => Ref_1);
         Assert (Condition => Interfaces.Unsigned_64'Value
                 (Muxml.Utils.Get_Element_Value (Doc   => Spec.Doc,
                                                 XPath => XPath)) = Ref_1,
                 Message   => "RIP value mismatch (x86_64: 1)");

         --  Test setting RIP with existing XML elements.

         Set_Entry_Point (Spec        => Spec,
                          Arch        => Mutools.Types.X86_64,
                          Entry_Point => Ref_2);
         Assert (Condition => Interfaces.Unsigned_64'Value
                 (Muxml.Utils.Get_Element_Value (Doc   => Spec.Doc,
                                                 XPath => XPath)) = Ref_2,
                 Message   => "RIP value mismatch (x86_64: 2)");
      end Check_Set_Entry_Point_X86_64;
   begin
      Check_Set_Entry_Point_X86_64;
      Check_Set_Entry_Point_ARM64;
--  begin read only
   end Test_Set_Entry_Point;
--  end read only


--  begin read only
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Create_Memory_Node_8de05b (Gnattest_T : in out Test) renames Test_Create_Memory_Node;
--  id:2.2/8de05b00e8474ef3/Create_Memory_Node/1/0/
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive (Provides_Exists : Boolean)
      is
         use type DOM.Core.Node;

         Suffix : constant String
           := (if Provides_Exists then " (1)" else " (2)");

         Spec : Muxml.XML_Data_Type;
         Node : DOM.Core.Element;
      begin
         Muxml.Parse (Data => Spec,
                      Kind => Muxml.Component,
                      File => "data/test_cspec.xml");

         if not Provides_Exists then
            Muxml.Utils.Remove_Elements
              (Doc   => Spec.Doc,
               XPath => "/component/provides");
         end if;

         Node := Create_Memory_Node
           (Spec            => Spec,
            Logical         => "foobar",
            Writable        => True,
            Executable      => False,
            Size            => 16#abcd_0000#,
            Virtual_Address => 16#dead_beef#);

         Node := Muxml.Utils.Get_Element
           (Doc   => Spec.Doc,
            XPath => "/component/provides/memory[@logical='foobar']");

         Assert (Condition => Node /= null,
                 Message   => "Memory node not created" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "logical") = "foobar",
                 Message   => "Logical name mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "writable") = "true",
                 Message   => "Writable mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "executable") = "false",
                 Message   => "Executable mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "size") = "16#abcd_0000#",
                 Message   => "Size mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress") = "16#dead_beef#",
                 Message   => "Virtual address mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "type") = "subject_binary",
                 Message   => "Type mismatch" & Suffix);

         Assert (Condition =>
                   DOM.Core.Nodes.Length
                     (Map => DOM.Core.Nodes.Attributes (N => Node)) = 6,
                 Message   => "Wrong number of attributes" & Suffix);
      end Positive;
   begin
      Positive (Provides_Exists => True);
      Positive (Provides_Exists => False);
--  begin read only
   end Test_Create_Memory_Node;
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
end Bin_Split.Spec.Test_Data.Tests;
