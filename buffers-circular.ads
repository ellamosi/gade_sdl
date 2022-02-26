generic
   type Element_Type is private;
package Buffers.Circular is

   package Appendable_Buffers is new Buffers.Appendable_Buffers (Element_Type);
   use Appendable_Buffers;

   type Circular_Buffer (Size : Positive) is new Appendable with private;

   procedure Push (Self : in out Circular_Buffer; E : Element_Type)
     with Inline;

   overriding
   procedure Append (Self : in out Circular_Buffer; E : Element_Type) renames Push;

   procedure Pop (Self : in out Circular_Buffer; E : out Element_Type)
     with Inline;

   --  procedure Commit_Reads (Self : in out Circular_Buffer);

   function Length (Self : Circular_Buffer) return Natural
     with Inline;

   function Is_Empty (Self : Circular_Buffer) return Boolean
     with Inline;

private

   type Data_Container is array (Positive range <>) of aliased Element_Type;

   type Data_Container_Access is access all Data_Container;

   type Circular_Buffer (Size : Positive) is new Appendable with record
      Data : aliased Data_Container (1 .. Size);

      Read_Index  : Positive := 1;
      Write_Index : Positive := 1;
      Count       : Natural  := 0;

      --  Could be some sort of cursor type?
      Uncommited_Write_Index : Positive;
      Uncommited_Read_Index  : Positive;
   end record;

end Buffers.Circular;
