with Bounded_Buffers;

with Gade.Audio_Buffer; use Gade.Audio_Buffer;

package Audio is

   --  TODO: Make all of this private when Gade_Audio is turned into a child package

private

   type Float_Frame is record
      Left, Right : Float;
   end record;

   function "+" (Left, Right : Float_Frame) return Float_Frame with Inline;

   function "-" (Left, Right : Float_Frame) return Float_Frame with Inline;

   function "*" (Left : Float_Frame; Right : Float)
                 return Float_Frame with Inline;

   function To_Float (S : Sample) return Float with Inline;

   package Sample_Buffers is new Bounded_Buffers (Gade.Audio_Buffer.Stereo_Sample);
   package Float_Buffers is new Bounded_Buffers (Float_Frame);

   subtype Video_Frame_Sample_Buffer is
     Sample_Buffers.Bounded_Buffer (Gade.Audio_Buffer.Maximum_Samples);

   function Data_Access (Buff : access Video_Frame_Sample_Buffer)
                         return Gade.Audio_Buffer.Audio_Buffer_Access;

   type Bounded_Buffer_Access is access all Video_Frame_Sample_Buffer;

end Audio;
