--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;
with Mutools.Match;
with Mutools.XML_Utils;
with Mutools.Templates;

with Spec.Utils;

with String_Templates;

package body Spec.Skp_Interrupts
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      if Mutools.XML_Utils.Is_Arm64 (Policy => Policy) then
         Write_ARMv8a (Output_Dir => Output_Dir,
                       Policy     => Policy);
      else
         Write_X86_64 (Output_Dir => Output_Dir,
                       Policy     => Policy);
      end if;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_ARMv8a
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      --  (1) extract all physical devices with GIC capability
      Physical_GIC_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device" &
             "[capabilities/capability/@name='gic']");

      --  (2) extract all physical devices with SMMU (i.c. iommu) capability
      Physical_SMMU_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device" &
             "[capabilities/capability/@name='iommu']");

      CPU_Count : constant Natural
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Policy);

      IRQ_Routing_Table    : Unbounded_String;
      Vector_Routing_Table : Unbounded_String;

      Tmpl : Mutools.Templates.Template_Type;

      --  Write IRQ information to interrupts spec.
      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Index : Natural);

      ----------------------------------------------------------------------

      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Index : Natural)
      is
         Phys_IRQ_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => IRQ,
              Name => "physical");
         Dev_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => DOM.Core.Nodes.Parent_Node (N => IRQ),
              Name => "physical");
         Dev_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/hardware/devices/device[@name='"
              & Dev_Name & "']");
         Physical_IRQ : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Dev_Node,
              XPath => "irq[@name='" & Phys_IRQ_Name & "']");
         IRQ_Number : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Physical_IRQ,
               Name => "number"));
         Subject : constant DOM.Core.Node
           := Muxml.Utils.Ancestor_Node (Node  => IRQ,
                                         Level => 3);
         Subject_ID : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Subject,
              Name => "globalId");
         Subject_Vector : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => IRQ,
              Name => "vector");
      begin
         --  (a) Routing Table - append configured interrupt
         if IRQ_Number > 26 then
            IRQ_Routing_Table := IRQ_Routing_Table & ASCII.LF &
              Indent (N => 4) & Ada.Strings.Fixed.Trim
              (IRQ_Number'Img, Ada.Strings.Left) & " => True,";
         end if;

         --  (b) Vector Table - append configured interrupt (incl.
         --  style fix for first interrupt entry)
         if Index /= 0 then
            Vector_Routing_Table := Vector_Routing_Table & ASCII.LF &
              Indent (N => 4);
         end if;

         Vector_Routing_Table := Vector_Routing_Table & Ada.Strings.Fixed.Trim
           (IRQ_Number'Img, Ada.Strings.Left) & " =>" & ASCII.LF &
           Indent (N => 4) & "  (Subject => " & Subject_ID & "," & ASCII.LF &
           Indent (N => 5) & "Vector  => " & Subject_Vector & "),";
      end Write_Interrupt;

      ----------------------------------------------------------------------

   begin
      Mulog.Log (Msg => "Writing interrupt routing spec to '"
                 & Output_Dir & "/skp-interrupts.ads'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_interrupts_armv8a_ads);

      --  Note: currently only one interrupt controller GIC-400 and one
      --  system memory management unit SMMU-500 supported
      if
        DOM.Core.Nodes.Length (Physical_GIC_Devs) = 1 and
        DOM.Core.Nodes.Length (Physical_SMMU_Devs) = 1
      then
         declare
            --  (3.a) get the physical GIC device
            Physical_GIC_Dev  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Physical_GIC_Devs,
                                      Index => 0);

            --  (3.b) extract GIC device max values
            Physical_IRQ_Max : constant Natural
              := Natural'Value (Muxml.Utils.Get_Element_Value
                                (Doc   => Physical_GIC_Dev,
                                 XPath => "capabilities/capability" &
                                   "[@name='pirq_id_max']"));
            Virtual_IRQ_Max  : constant Natural
              := Natural'Value (Muxml.Utils.Get_Element_Value
                                (Doc   => Physical_GIC_Dev,
                                 XPath => "capabilities/capability" &
                                   "[@name='virq_id_max']"));
            Virt_Maintenance_IRQ : constant Natural
              := Natural'Value (Muxml.Utils.Get_Attribute
                                (Doc   => Physical_GIC_Dev,
                                 XPath => "irq[@name='maintenance']",
                                 Name  => "number"));

            Physical_SMMU_Dev : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Physical_SMMU_Devs,
                                      Index => 0);
            SMMU_IRQ_ID       : constant Natural
              := Natural'Value (Muxml.Utils.Get_Attribute
                                (Doc   => Physical_SMMU_Dev,
                                 XPath => "irq[@name='irq']",
                                 Name  => "number"));
         begin
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__pirq_id_max__",
               Content  => Physical_IRQ_Max'Img);
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__virq_id_max__",
               Content  => Virtual_IRQ_Max'Img);
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__virtual_maintenance_irq__",
               Content  => Virt_Maintenance_IRQ'Img);
            Mutools.Templates.Replace
              (Template => Tmpl,
               Pattern  => "__pirq_id_smmu__",
               Content  => SMMU_IRQ_ID'Img);

            for I in 0 .. CPU_Count - 1 loop
               declare
                  IRQs : constant Muxml.Utils.Matching_Pairs_Type
                    := Muxml.Utils.Get_Matching
                      (XML_Data       => Policy,
                       Left_XPath     => "/system/subjects/subject[@cpu='" &
                         Ada.Strings.Fixed.Trim (I'Img, Ada.Strings.Left) &
                           "']/devices/device/irq",
                       Right_XPath    => "/system/hardware/devices/device/irq",
                       Match_Multiple => False,
                       Match          => Mutools.Match.
                         Is_Valid_Resource_Ref'Access);
               begin
                  --  (i) add base entry for current CPU (incl. style fix
                  --  for first CPU entry and indent for following CPUs)
                  if I > 0 then
                     IRQ_Routing_Table := IRQ_Routing_Table &
                       Indent (N => 3);
                     Vector_Routing_Table := Vector_Routing_Table &
                       Indent (N => 3);
                  end if;

                  IRQ_Routing_Table := IRQ_Routing_Table & Ada.Strings.
                    Fixed.Trim (I'Img, Ada.Strings.Left) & " =>" & ASCII.LF;
                  Vector_Routing_Table := Vector_Routing_Table & Ada. Strings.
                    Fixed.Trim (I'Img, Ada.Strings.Left) & " =>" & ASCII.LF;

                  --  (ii) statically enable banked SGI, maintenance and
                  --  hypervisor timer interrupts for GIC-400
                  IRQ_Routing_Table := IRQ_Routing_Table &
                    Indent (N => 3) & "  (0  .. 15 => True," & ASCII.LF &
                    Indent (N => 4) & "25 .. 26 => True,";
                  Vector_Routing_Table := Vector_Routing_Table &
                    Indent (N => 3) & "  (";

                  --  (iii) append configured interrupts
                  for K in 0 .. DOM.Core.Nodes.Length (IRQs.Left) - 1 loop
                     Write_Interrupt (IRQ   => DOM.Core.Nodes.Item
                                      (List  => IRQs.Left,
                                       Index => K),
                                      Index => K);
                  end loop;

                  --  (iv) statically enable SMMU interrupt on first core
                  --  and add indent for vector table with interrupts
                  if I = 0 then
                     IRQ_Routing_Table := IRQ_Routing_Table & ASCII.LF &
                       Indent (N => 4) & Ada.Strings.Fixed.Trim
                       (SMMU_IRQ_ID'Img, Ada.Strings.Left) & " => True,";
                  end if;

                  if DOM.Core.Nodes.Length (IRQs.Left) > 0 then
                     Vector_Routing_Table := Vector_Routing_Table & ASCII.LF &
                       Indent (N => 4);
                  end if;

                  --  (v) set others to false / null
                  IRQ_Routing_Table := IRQ_Routing_Table & ASCII.LF &
                    Indent (N => 4) & "others => False)";
                  Vector_Routing_Table := Vector_Routing_Table &
                    "others =>" & ASCII.LF & Indent (N => 4) &
                    "  (Subject => Invalid_Subject," & ASCII.LF &
                    Indent (N => 5) & "Vector  => 0))";

                  --  (vi) append separator except for last configured CPU
                  if I /= CPU_Count - 1 then
                     IRQ_Routing_Table := IRQ_Routing_Table &
                       "," & ASCII.LF;
                     Vector_Routing_Table := Vector_Routing_Table &
                       "," & ASCII.LF;
                  end if;
               end;
            end loop;
         end;

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__irq_routing_table__",
            Content  => To_String (IRQ_Routing_Table));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__vector_routing_table__",
            Content  => To_String (Vector_Routing_Table));
      end if;

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-interrupts.ads");
   end Write_ARMv8a;

   -------------------------------------------------------------------------

   procedure Write_X86_64
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;

      IRQs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data       => Policy,
           Left_XPath     => "/system/subjects/subject/devices/device/irq",
           Right_XPath    => "/system/hardware/devices/device/irq",
           Match_Multiple => False,
           Match          => Mutools.Match.Is_Valid_Resource_Ref'Access);
      IRQ_Count   : constant Natural := DOM.Core.Nodes.Length
        (List => IRQs.Right);
      Route_Count : constant Natural
        := IRQ_Count - Utils.Get_IRQ_Count
          (IRQs     => IRQs.Right,
           IRQ_Kind => Mutools.XML_Utils.IRQ_PCI_MSI);
      IOAPIC_RTE_Index_Max : constant Natural
        := Mutools.XML_Utils.Get_IOAPIC_RTE_Index_Max (Data => Policy);

      Cur_IRQ   : Positive := 1;
      Cur_Route : Positive := 1;

      IRQ_Buffer, Mask_IRQ_Buffer, Vector_Buffer : Unbounded_String;

      --  Write IRQ information to interrupts spec.
      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Owner : DOM.Core.Node;
         Index : Natural);

      ----------------------------------------------------------------------

      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Owner : DOM.Core.Node;
         Index : Natural)
      is
         use type DOM.Core.Node;
         use type Mutools.XML_Utils.IRQ_Kind;

         Phys_IRQ_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => IRQ,
              Name => "physical");
         Dev_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => DOM.Core.Nodes.Parent_Node (N => IRQ),
              Name => "physical");
         Dev_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/hardware/devices/device[@name='"
              & Dev_Name & "']");
         Physical_IRQ : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Dev_Node,
              XPath => "irq[@name='" & Phys_IRQ_Name & "']");
         IRQ_Nr : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Physical_IRQ,
               Name => "number"));
         Host_Vector : constant Natural := IRQ_Nr
           + Mutools.Constants.Host_IRQ_Remap_Offset;
         APIC_ID : constant Natural
           := Mutools.XML_Utils.To_APIC_ID
             (Policy => Policy,
              CPU_ID => Natural'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Owner,
                    Name => "cpu")));
         Subject_ID : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Owner,
              Name => "globalId");
         Subject_Vector : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => IRQ,
              Name => "vector");
         IRQ_Kind : constant Mutools.XML_Utils.IRQ_Kind
           := Mutools.XML_Utils.Get_IRQ_Kind (Dev => Dev_Node);
         Is_PCI_Dev : constant Boolean
           := IRQ_Kind /= Mutools.XML_Utils.IRQ_ISA;
      begin
         case IRQ_Kind is
            when Mutools.XML_Utils.IRQ_ISA |
                 Mutools.XML_Utils.IRQ_PCI_LSI =>

               --  IRQ routing table.

               declare
                  RTE_Idx : constant Mutools.XML_Utils.IOAPIC_RTE_Range
                    := Mutools.XML_Utils.Get_IOAPIC_RTE_Idx
                      (IRQ => Mutools.XML_Utils.IOAPIC_IRQ_Range (IRQ_Nr));
               begin
                  IRQ_Buffer := IRQ_Buffer & Indent (N => 2)
                    & Index'Img & " => IRQ_Route_Type'("
                    & ASCII.LF
                    & Indent (N => 3) & "APIC_ID   =>" & APIC_ID'Img
                    & "," & ASCII.LF
                    & Indent (N => 3) & "RTE_Idx   =>" & RTE_Idx'Img
                    & "," & ASCII.LF
                    & Indent (N => 3) & "IRQ       =>" & IRQ_Nr'Img
                    & "," & ASCII.LF
                    & Indent (N => 3)
                    & "IRQ_Mode  => " & (if Is_PCI_Dev then "Level"
                                         else "Edge")
                    & "," & ASCII.LF
                    & Indent (N => 3)
                    & "IRQ_Level => " & (if Is_PCI_Dev then "Low" else "High")
                    & "," & ASCII.LF
                    & Indent (N => 3) & "Vector    =>" & Host_Vector'Img & ")";

                  if Is_PCI_Dev then
                     if Mask_IRQ_Buffer /= Null_Unbounded_String then
                        Mask_IRQ_Buffer := Mask_IRQ_Buffer & " | ";
                     end if;
                     Mask_IRQ_Buffer := Mask_IRQ_Buffer
                       & Ada.Strings.Fixed.Trim (Source => Host_Vector'Img,
                                                 Side   => Ada.Strings.Left);
                  end if;
               end;

               if Cur_Route /= Route_Count then
                  IRQ_Buffer := IRQ_Buffer & "," & ASCII.LF;
               end if;

               Cur_Route := Cur_Route + 1;
            when Mutools.XML_Utils.IRQ_PCI_MSI =>

               --  MSI interrupts are not routed through the I/O APIC.

               null;
         end case;

         --  Vector -> subject routing table.

         Vector_Buffer := Vector_Buffer & Indent (N => 2)
           & Host_Vector'Img & " => Vector_Route_Type'("
           & ASCII.LF
           & Indent (N => 3) & "Subject => " & Subject_ID & ","
           & ASCII.LF
           & Indent (N => 3) & "Vector  => " & Subject_Vector & ")";
      end Write_Interrupt;

      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing interrupt routing spec to '"
                 & Output_Dir & "/skp-interrupts.ads'");

      for I in 0 .. DOM.Core.Nodes.Length (List => IRQs.Left) - 1 loop
         declare
            IRQ     : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => IRQs.Left,
               Index => I);
            Subject : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node (Node  => IRQ,
                                            Level => 3);
         begin
            Write_Interrupt
              (IRQ   => IRQ,
               Owner => Subject,
               Index => Cur_Route);

            if Cur_IRQ /= IRQ_Count then
               Vector_Buffer := Vector_Buffer & "," & ASCII.LF;
            end if;

            Cur_IRQ := Cur_IRQ + 1;
         end;
      end loop;

      if Route_Count = 0 then
         IRQ_Buffer := IRQ_Buffer & Indent (N => 2)
           & " others => Null_IRQ_Route";
      end if;

      if IRQ_Count > 0 then
         Vector_Buffer := Vector_Buffer & "," & ASCII.LF;
      end if;

      Vector_Buffer := Vector_Buffer & Indent (N => 2)
        & " others => Null_Vector_Route";

      if Mask_IRQ_Buffer = Null_Unbounded_String then
         Mask_IRQ_Buffer := To_Unbounded_String
           (Source => "1 .. 0");
      end if;

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_interrupts_x86_64_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__remap_offset__",
         Content  => Mutools.Constants.Host_IRQ_Remap_Offset'Img);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__rte_index_max__",
         Content  => IOAPIC_RTE_Index_Max'Img);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__routing_range__",
         Content  => "1 .." & Natural'Max (1, Route_Count)'Img);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__mask_irqs__",
         Content  => To_String (Mask_IRQ_Buffer));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__irq_routing_table__",
         Content  => To_String (IRQ_Buffer));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__vector_routing_table__",
         Content  => To_String (Vector_Buffer));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-interrupts.ads");
   end Write_X86_64;

end Spec.Skp_Interrupts;
