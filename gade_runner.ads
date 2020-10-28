with Gade.Interfaces;   use Gade.Interfaces;
with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with SDL.Video.Textures;

generic
   type Video_Renderer is private;
   with procedure Yield_Video_Frame
     (Renderer : in out Video_Renderer;
      Texture  : SDL.Video.Textures.Texture);
   --  Relying on yield is a bad idea as it would cause unnecessary thread
   --  blocking
package Gade_Runner is

   protected type Protected_Gade_Runner is

      procedure Create;

      procedure Load_ROM (File_Name : String);

      entry Get_Audio_Samples
        (Audio_Buffer      : out Audio_Buffer_Access;
         Requested_Samples : in  Positive;
         Actual_Samples    : out Positive);

      entry Get_Video_Frame (Renderer : in out Video_Renderer);

   private
      Audio_Samples_Ready    : Boolean := False;
      Video_Frame_Ready      : Boolean := False;
      Gade                   : Gade_Type;
      Protected_Audio_Buffer : Audio_Buffer_Access;
      Protected_Video_Buffer : RGB32_Display_Buffer_Access;
   end Protected_Gade_Runner;

end Gade_Runner;
