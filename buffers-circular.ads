generic
   type Element_Type is private;
   with package Appendables is new Appendable_Buffers (Element_Type);
package Buffers.Circular is
   use Appendables;

   type Circular_Buffer (Size : Positive) is new Appendable with private;

   procedure Push (Self : in out Circular_Buffer; E : Element_Type)
     with
       Pre  => Length (Self) <= Self.Size - 1 or else raise Constraint_Error,
       Post => Length (Self)'Old + 1 = Length (Self) and then Self.Size >= Length (Self);

   overriding
   procedure Append (Self : in out Circular_Buffer; E : Element_Type) renames Push;

   procedure Pop (Self : in out Circular_Buffer; E : out Element_Type)
     with
       Post => Length (Self)'Old - 1 = Length (Self);

   function Length (Self : Circular_Buffer) return Natural
     with Inline;

   function Is_Empty (Self : Circular_Buffer) return Boolean
     with Inline;

   function Available (Self : Circular_Buffer) return Natural
     with Inline;

private

   type Data_Container is array (Positive range <>) of aliased Element_Type;

   type Data_Container_Access is access all Data_Container;

   type Circular_Buffer (Size : Positive) is new Appendable with record
      Data : aliased Data_Container (1 .. Size);

      Read_Index  : Positive := 1;
      Write_Index : Positive := 1;
      Count       : Natural  := 0;
   end record;

end Buffers.Circular;
