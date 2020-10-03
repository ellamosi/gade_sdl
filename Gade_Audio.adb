--  with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
--  with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Gade.Video_Buffer; use Gade.Video_Buffer;

with SDL.Video.Pixels; --  use SDL.Video.Pixels;
with SDL.Video.Textures;
with SDL.Video.Renderers;
with Ada.Text_IO; use Ada.Text_IO;

package body Gade_Audio is

   --  Pitch : constant := 4400.0;
   --  Radians_S : constant := Pitch * 2.0 / Pi;

   --  Seconds_Offset : Float := 0.0;
   Err            : SoundIo_Error;
   pragma Unreferenced (Err);

--     Signed_Size : constant := 32;
--     type Signed is range -(2 ** (Signed_Size - 1)) .. (2 ** (Signed_Size - 1)) - 1;
--     for Signed'Size use Signed_Size;

   procedure Write_Float_Sample is new Write_Sample (Float);
   --  procedure Write_Signed_Sample is new Write_Sample (Signed);

   procedure Write_Callback
     (Out_Stream      : access SoundIo_Out_Stream;
      Frame_Count_Min : int;
      Frame_Count_Max : int)
   is
      pragma Unreferenced (Frame_Count_Min);
      --  Layout            : SoundIo_Channel_Layout renames Out_Stream.Layout;
      Sample_Rate : constant Float := Float (Out_Stream.Sample_Rate);
      --  Seconds_Per_Frame : constant Float := 1.0 / Sample_Rate;
      Areas             : SoundIo_Channel_Area_Ptr;
      --  Frames_Left       : int   := Frame_Count_Max;
      use Soundio_Channel_Area_Ptrs;

      Context : constant Stream_Context_Access := Convert (Out_Stream.User_Data);

      --  Audio_Buff : aliased Audio_Buffer_Type;
      --  Audio_Buff_Ptr : constant Audio_Buffer_Access := Audio_Buff'Unchecked_Access;
      Audio_Buff_Ptr : Audio_Buffer_Access;

      Pitch_Pointer : SDL.Video.Pixels.Pitch_Access.Pointer;
      Pixel_Pointer : SDL.Video.Pixels.ARGB_8888_Access.Pointer;

      function ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access is
         new Ada.Unchecked_Conversion
           (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
            Target => RGB32_Display_Buffer_Access);

      Sample_Ratio : constant int := int (Float (Samples_Second) / Sample_Rate);
      --  Requested_Samples : constant Natural := Natural (Sample_Ratio * Frame_Count_Max) -
      --  Context.Rem_Samples / Natural (Sample_Ratio);
      Requested_Samples : Natural;
      Actual_Samples : Natural; -- Per frame
      SSample : Stereo_Sample;
      Sample_Acc : Integer;
      Sample_Avg : Float;
      idx : Interfaces.C.int := 0;
      Frame_Finished : Boolean;
      --  B : Boolean := False;
      --  J : int;
      buff_idx : Natural;
      --  Int_Sample : Signed;
      Frame_Count : int := Frame_Count_Max;
      Old_Buf, New_Buf : Double_Buffer_Range;
      --  New_Buf : constant Double_Buffer_Range := (if Context.Cur_Buff = 0 then 1 else 0);

      FPS_Val : Float;
      FPS_Time_Diff : Duration;
      FPS_New_Time : Time;
   begin
--        Put_Line
--          ("Sample_Rate" & Sample_Rate'Image & " Sample_Ratio" & Sample_Ratio'Img);
--        Put_Line
--          ("Sample_Rate" & Sample_Rate'Image & " Sample_Ratio" & Sample_Ratio'Img &
--             "MinReq" & Frame_Count_Min'Img & " MaxReq" & Frame_Count_Max'Img & " GBReq" & Requested_Samples'Img);
      --  Put_Line ("Write_Callback");
      Frame_Count := Frame_Count_Max;
      Err := Outstream_Begin_Write (Out_Stream, Areas, Frame_Count);

      while idx < Frame_Count_Max loop
         Requested_Samples :=
           Natural (Sample_Ratio * (Frame_Count_Max - idx)) - Context.Rem_Samples;

         --  Put_Line ("Run_For" & Requested_Samples'Img);

         Old_Buf := Context.Cur_Buff;
         New_Buf := (if Old_Buf = 0 then 1 else 0);
         buff_idx := 0;

         SDL.Video.Textures.Lock
           (Self    => Context.Window.Texture,
            Pixels  => Pixel_Pointer,
            Pitches => Pitch_Pointer);

         --  TODO: If we loop, WE NEED TO FLUSH AND SWITCH BUFFERS!
         Audio_Buff_Ptr := Context.Audio_Buff (New_Buf)'Unchecked_Access;

         --  Put_Line ("Requested_Samples" & Requested_Samples'Img & " (x4)");

         --  Put_Line ("BEGIN Run_For");
         Run_For (Context.G,
                  Requested_Samples * 4,
                  Actual_Samples,
                  ARGB_8888_Pointer_To_RGB32_Display_Buffer_Access (Pixel_Pointer),
                  Audio_Buff_Ptr,
                  Frame_Finished);
         --  Put_Line ("END Run_For");

         --  Put_Line ("Actual_Samples/4" & Actual_Samples'Img);

--           for i in 0 .. Actual_Samples - 1 loop
--              Put_Line (Audio_Buff_Ptr (i).Left'Img);
--           end loop;

         SDL.Video.Textures.Unlock (Context.Window.Texture);

         Actual_Samples := Actual_Samples / 4;

         Context.Frame_Samples := Context.Frame_Samples + Actual_Samples;

         --  Put_Line ("Actual_Samples" & Actual_Samples'Img & " Requested_Samples" & Actual_Samples'Img);

         if Frame_Finished then

            --  Put_Line ("New Frame");
            --  Do only if frame actually generated
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

            --  Put_Line ("Frame_Samples:" & Context.Frame_Samples'Img);
            Context.Frame_Samples := 0;
         end if;

         Frame_Count := int (Actual_Samples + Context.Rem_Samples) / Sample_Ratio;
         --  Put_Line ("Frame_Count:" & Frame_Count'Img);

         --  Mixed buffer flushing
--           if Context.Rem_Samples > 0 then -- revise condition for rem + actual >= Sample_Ratio
--              --  Put_Line ("Mixed buffer flushing (" & Context.Rem_Samples'Img & " samples)");
--
--              Sample_Acc := 0;
--              for i in Natural range 0 .. Natural (Context.Rem_Samples - 1) loop
--                 SSample.Left := Context.Audio_Buff (Old_Buf) (Context.Rem_Index + i).Left;
--                 --  Sample'Write (Context.File_Stream, SSample.Left);
--                 Sample_Acc := Integer (SSample.Left) + Sample_Acc;
--              end loop;
--
--              buff_idx := 0;
--              for i in Natural range Context.Rem_Samples .. Natural (Sample_Ratio) - 1 loop
--                 SSample.Left := Context.Audio_Buff (New_Buf) (buff_idx).Left;
--                 Sample_Acc := Integer (SSample.Left) + Sample_Acc;
--                 --  Sample'Write (Context.File_Stream, SSample.Left);
--                 buff_idx := buff_idx + 1;
--              end loop;
--
--              Sample_Avg := Float (Sample_Acc) / Float (int (Sample'Last) * Sample_Ratio);
--
--              --  Put_Line (Sample_Avg'Img);
--              Write_Float_Sample (Get_Area (Areas, 0), idx, Sample_Avg);
--              Write_Float_Sample (Get_Area (Areas, 1), idx, Sample_Avg);
--              --  Put_Line (Sample_Avg'Img);
--              Float'Write (Context.File_Stream, Sample_Avg);
--
--              idx := idx + 1;
--           end if;

--           Put_Line ("Main buffer flushing (" &
--                       buff_idx'Img & " /" & Actual_Samples'Img & "* samples) idx:" &
--                       idx'Img & " /" & Frame_Count_Max'Img);
         while buff_idx < Actual_Samples and idx < Frame_Count_Max loop
            --  SSample := Audio_Buff (Natural (idx * Sample_Ratio));
            Sample_Acc := 0;
            for i in 1 .. Sample_Ratio loop
               SSample.Left := Context.Audio_Buff (New_Buf) (buff_idx).Left;
               --  Sample'Write (Context.File_Stream, SSample.Left);
               --              if SSample.Left /= Sample'Last and SSample.Left /= Sample'First then
               --                 Put_Line ("Unexpected sample: " & SSample.Left'Img);
               --              end if;
               Sample_Acc := Integer (SSample.Left) + Sample_Acc;
               buff_idx := buff_idx + 1;
            end loop;

            Sample_Avg := Float (Sample_Acc) / Float (int (Sample'Last) * Sample_Ratio);
            --  Put (Sample_Avg'Img);

            --  Put_Line (Sample_Acc'Img & Sample_Avg'Img);

            Write_Float_Sample (Get_Area (Areas, 0), idx, Sample_Avg);
            Write_Float_Sample (Get_Area (Areas, 1), idx, Sample_Avg);
            --  Put_Line (Sample_Avg'Img);
            Float'Write (Context.File_Stream, Sample_Avg);
            --  TODO: Resample using averages
            --  Put (Sample_Acc'Img);
            --           if Context.Edge_State then
            --              Sample_Avg := 1.0;
            --              Int_Sample := Signed'Last;
            --           else
            --              Sample_Avg := -1.0;
            --              Int_Sample := Signed'First;
            --           end if;
            --
            --           while Context.Edge_Samples < 20 and idx < Frame_Count_Max loop
            --              Write_Float_Sample (Get_Area (Areas, 0), idx, Sample_Avg);
            --              Write_Float_Sample (Get_Area (Areas, 1), idx, Sample_Avg);
            --              --  Write_Signed_Sample (Get_Area (Areas, 0), idx, Int_Sample);
            --              --  Write_Signed_Sample (Get_Area (Areas, 1), idx, Int_Sample);
            --              Context.Edge_Samples := Context.Edge_Samples + 1;
            --              idx := idx + 1;
            --           end loop;
            --           if Context.Edge_Samples = 20 then
            --              --  Put_Line ("Edge switch");
            --              Context.Edge_Samples := 0;
            --              Context.Edge_State := not Context.Edge_State;
            --           end if;
            idx := idx + 1;
         end loop;
         --  Put_Line (idx'Img & " /" & Frame_Count_Max'Img);
         --  New_Line;

         --  Put_Line ("buff_idx" & buff_idx'Img & " idx" & idx'Img);

         if buff_idx < Actual_Samples then
            Context.Rem_Samples := Actual_Samples - buff_idx;
            Context.Rem_Index := buff_idx;
            --  Put_Line ("Buffer not fully flushed (" & Context.Rem_Samples'Img & " samples remain)");
         else
            --  Put_Line ("Buffer fully flushed");
            Context.Rem_Samples := 0;
            Context.Rem_Index := 0;
         end if;
         Context.Cur_Buff := New_Buf;
      end loop;

      Err := Outstream_End_Write (Out_Stream);


      --  while >= Sample_Ratio

      --  Put_Line ("Act" & Actual_Samples'Img);

      --  Write Audio

--        while idx < Frame_Count_Max loop
--           --  SSample := Audio_Buff (Natural (idx * Sample_Ratio));
--           Sample_Acc := 0;
--           for i in 0 .. Sample_Ratio - 1 loop
--              SSample.Left := Audio_Buff (Natural (idx * Sample_Ratio + i)).Left;
--              Sample_Acc := Integer (SSample.Left);
--           end loop;
--           Sample_Avg := Float (Sample_Acc) / Float (32768 * Sample_Ratio);
--           --  TODO: Resample using averages
--           --  Put (Sample_Acc'Img);
--
--           Write_Float_Sample (Get_Area (Areas, 0), idx, Sample_Avg);
--           Write_Float_Sample (Get_Area (Areas, 1), idx, Sample_Avg);
--
--           idx := idx + 1;
--        end loop;



      --  Put_Line ("Act" & idx'Img & " /" & Frame_Count_Max'Img);

      --  New_Line;

      --  while Frames_Left > 0 loop
      --    declare
      --       Frame_Count : int := Frames_Left;
      --    begin
      --       Err := Outstream_Begin_Write (Out_Stream, Areas, Frame_Count);
      --       exit when Frame_Count = 0;

      --       for Frame in 0 .. Frame_Count - 1 loop
      --          Sample := Sin
      --            ((Seconds_Offset + Float (Frame) * Seconds_Per_Frame)
      --             * Radians_S);

      --  TODO: Write Get_Area function that returns the Area at index
      --  Channel
      --          for Channel in 0 .. Layout.Channel_Count - 1 loop
      --             Write_Float_Sample
      --               (Get_Area (Areas, Channel), Frame, Sample);
      --          end loop;
      --       end loop;

      --       Seconds_Offset :=
      --         Seconds_Offset + (Seconds_Per_Frame * Float (Frame_Count));

      --       Err := Outstream_End_Write (Out_Stream);

      --       Frames_Left := Frames_Left - Frame_Count;
      --    end;
      --  end loop;

--        Samples_Elapsed := Samples_Elapsed + Natural (Frame_Count_Max);
--        Seconds_Elapsed := Samples_Elapsed / Natural (Out_Stream.Sample_Rate);
--        Put_Line
--           (Seconds_Elapsed'Img & "s (" & Samples_Elapsed'Img & " samples)");
--
--        Err := Outstream_Begin_Write (Out_Stream, Areas, Frames_Left);
--
--        while Frames_Left > 0 loop
--           declare
--              Pitch_Frame_Count : constant int := int'Min (Frames_Left, Frames_Pitch);
--           begin
--              Pitch :=
--                 Pitch_Min +
--                 Float (Seconds_Elapsed mod Pitch_Intervals) * Pitch_Step;
--              for Frame in 0 .. Pitch_Frame_Count - 1 loop
--                 Sample :=
--                    Sin
--                       ((Pitch_Seconds_Offset +
--                         Float (Frame) * Seconds_Per_Frame) *
--                        Radians_S);
--
--                 --  TODO: Write Get_Area function that returns the Area at index
--                 --  Channel
--                 for Channel in 0 .. Layout.Channel_Count - 1 loop
--                    Write_Float_Sample
--                       (Get_Area (Areas, Channel), Frame, Sample);
--                 end loop;
--              end loop;
--
--              Frames_Left := Frames_Left - Pitch_Frame_Count;
--
--              Put_Line
--                 ("Frames_Left: " & Frames_Left'Img & " Frames_Pitch" &
--                  Frames_Pitch'Img);
--              if Frames_Left > 0 or Frames_Pitch = Frames_Left then
--                 --  Pitch change
--                 Pitch_Seconds_Offset := 0.0;
--                 Put_Line ("Pitch change");
--              else
--                 --  No pitch change
--                 Pitch_Seconds_Offset :=
--                    Pitch_Seconds_Offset +
--                    (Seconds_Per_Frame * Float (Pitch_Frame_Count));
--              end if;
--           end;
--        end loop;
--
--        Err := Outstream_End_Write (Out_Stream);
   exception
      when others => null; --  Put_Line ("Callback EXCEPTION");
   end Write_Callback;

end Gade_Audio;
