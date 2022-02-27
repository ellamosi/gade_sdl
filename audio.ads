with Buffers, Buffers.Bounded, Buffers.Circular; use Buffers;

with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with SDL.Audio.Devices;

package Audio is private

   type Float_Frame is record
      Left, Right : Float;
   end record;

   function "+" (Left, Right : Float_Frame) return Float_Frame with Inline;

   function "-" (Left, Right : Float_Frame) return Float_Frame with Inline;

   function "*" (Left : Float_Frame; Right : Float)
                 return Float_Frame with Inline;

   function To_Float (S : Sample) return Float with Inline;


   package Sample_Appendables is new Appendable_Buffers (Gade.Audio_Buffer.Stereo_Sample);
   package Sample_Buffers is new Buffers.Bounded (Gade.Audio_Buffer.Stereo_Sample, Sample_Appendables);

   package Float_Appendables is new Appendable_Buffers (Float_Frame);
   package Float_Buffers is new Buffers.Bounded (Float_Frame, Float_Appendables);
   package Circular_Float_Buffers is new Buffers.Circular (Float_Frame, Float_Appendables);

   subtype Video_Frame_Sample_Buffer is
     Sample_Buffers.Bounded_Buffer (Gade.Audio_Buffer.Maximum_Samples);

   function Data_Access (Buff : access Video_Frame_Sample_Buffer)
                         return Gade.Audio_Buffer.Audio_Buffer_Access;

   type Bounded_Buffer_Access is access all Video_Frame_Sample_Buffer;

   package Devices is new SDL.Audio.Devices
     (Frame_Type   => Float_Frame,
      Buffer_Index => Positive,
      Buffer_Type  => Float_Buffers.Data_Container);

end Audio;
