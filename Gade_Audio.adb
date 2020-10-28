with Gade.Video_Buffer; use Gade.Video_Buffer;

with SDL.Video.Pixels; --  use SDL.Video.Pixels;
with SDL.Video.Textures;
with SDL.Video.Renderers;
with Ada.Text_IO; use Ada.Text_IO;

package body Gade_Audio is

   Err : SoundIo_Error;
   pragma Unreferenced (Err);

   procedure Write_Float_Sample is new Write_Sample (Float);

   procedure Write_Callback
     (Out_Stream      : access SoundIo_Out_Stream;
      Frame_Count_Min : int;
      Frame_Count_Max : int)
   is
      pragma Unreferenced (Frame_Count_Min);
      Sample_Rate : constant Float := Float (Out_Stream.Sample_Rate);
      Areas             : SoundIo_Channel_Area_Ptr;
      use Soundio_Channel_Area_Ptrs;

      Context : constant Stream_Context_Access := Convert (Out_Stream.User_Data);

      Audio_Buff_Ptr : Audio_Buffer_Access;

      Pitch_Pointer : SDL.Video.Pixels.Pitch_Access.Pointer;
      Pixel_Pointer : SDL.Video.Pixels.ARGB_8888_Access.Pointer;

      function ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access is
         new Ada.Unchecked_Conversion
           (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
            Target => RGB32_Display_Buffer_Access);

      Sample_Ratio : constant int := int (Float (Samples_Second) / Sample_Rate);
      Requested_Samples : Natural;
      Actual_Samples : Natural; -- Per frame
--        SSample : Stereo_Sample;
--        Sample_Avg : Float;
      Left_Avg, Right_Avg : Float;
      idx : Interfaces.C.int := 0;
      Frame_Finished : Boolean;
      buff_idx : Natural;
      Frame_Count : int := Frame_Count_Max;

      FPS_Val : Float;
      FPS_Time_Diff : Duration;
      FPS_New_Time : Time;
   begin
      Frame_Count := Frame_Count_Max;
      Err := Outstream_Begin_Write (Out_Stream, Areas, Frame_Count);

      while idx < Frame_Count_Max loop
         Requested_Samples :=
           Natural (Sample_Ratio * (Frame_Count_Max - idx)) - Context.Acc_Count;

         --  Assigning this pointer to a variable seems to be required to not
         --  trigger a compiler bug. Getting the access type inline on a
         --  function call crashes it.
         Audio_Buff_Ptr := Context.Audio_Buff'Unchecked_Access;

         SDL.Video.Textures.Lock
           (Self    => Context.Window.Texture,
            Pixels  => Pixel_Pointer,
            Pitches => Pitch_Pointer);

         Run_For (Context.G,
                  Requested_Samples * 4,
                  Actual_Samples,
                  ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access (Pixel_Pointer),
                  Audio_Buff_Ptr,
                  Frame_Finished);

         SDL.Video.Textures.Unlock (Context.Window.Texture);

         Actual_Samples := Actual_Samples / 4;

         Context.Frame_Samples := Context.Frame_Samples + Actual_Samples;

         if Frame_Finished then
            SDL.Video.Renderers.Clear (Context.Window.Renderer);
            SDL.Video.Renderers.Copy (Context.Window.Renderer, Context.Window.Texture);
            SDL.Video.Renderers.Present (Context.Window.Renderer);

            Context.FPS_Count := Context.FPS_Count + 1;
            if Context.FPS_Count >= 120 then
               FPS_New_Time := Clock;
               FPS_Time_Diff := FPS_New_Time - Context.FPS_Time;
               FPS_Val := 120.0 / Float (FPS_Time_Diff);
               Put_Line ("FPS:" & FPS_Val'Img);
               Context.FPS_Time := FPS_New_Time;
               Context.FPS_Count := 0;
            end if;

            Context.Frame_Samples := 0;
         end if;

         buff_idx := 0;
         while buff_idx < Actual_Samples and idx < Frame_Count_Max loop
            while buff_idx < Actual_Samples and Context.Acc_Count < Integer (Sample_Ratio) loop
               Context.Acc_Left := Context.Acc_Left + Integer (Context.Audio_Buff (buff_idx).Left);
               Context.Acc_Right := Context.Acc_Right + Integer (Context.Audio_Buff (buff_idx).Right);
               Context.Acc_Count := Context.Acc_Count + 1;
               buff_idx := buff_idx + 1;
            end loop;

            if Context.Acc_Count = Integer (Sample_Ratio) then
               Left_Avg := Float (Context.Acc_Left) / Float (int (Sample'Last) * Sample_Ratio);
               Right_Avg := Float (Context.Acc_Right) / Float (int (Sample'Last) * Sample_Ratio);
               Context.Acc_Left := 0;
               Context.Acc_Right := 0;
               Context.Acc_Count := 0;

               Write_Float_Sample (Get_Area (Areas, 0), idx, Left_Avg);
               Write_Float_Sample (Get_Area (Areas, 1), idx, Right_Avg);
               Float'Write (Context.File_Stream, Left_Avg);
               Float'Write (Context.File_Stream, Right_Avg);
               idx := idx + 1;
            end if;
         end loop;
      end loop;

      Err := Outstream_End_Write (Out_Stream);
   exception
      when others => null; --  Put_Line ("Callback EXCEPTION");
   end Write_Callback;

end Gade_Audio;
