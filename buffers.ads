package Buffers is

   --  TODO: non integer based Count_Type rather than Positive

   generic
      type Element_Type;
   package Appendable_Buffers is

      type Appendable is interface;

      procedure Append (Self : in out Appendable; E : Element_Type) is abstract;

   end Appendable_Buffers;

private



end Buffers;
