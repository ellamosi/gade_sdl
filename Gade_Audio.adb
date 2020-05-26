with Ada.Numerics;                      use Ada.Numerics;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Gade_Audio is

   --  Pitch : constant := 4400.0;
   --  Radians_S : constant := Pitch * 2.0 / Pi;

   --  Seconds_Offset : Float := 0.0;
   Err            : SoundIo_Error;
   pragma Unreferenced (Err);

   Pitch_Min       : constant       := 4400.0;
   --  Pitch_Max       : constant       := 8400.0;
   Pitch_Step      : constant Float := 1000.0;
   Pitch_Intervals : constant       := 8;
   --  State should be part of user data:
   Pitch                : Float    := Pitch_Min;
   --  Pitch_Duration       : constant := 1.0;
   Seconds_Elapsed      : Natural  := 0;
   Samples_Elapsed      : Natural  := 0;
   --  Pitch_Frames_Left    : Natural  := 0;
   Pitch_Seconds_Offset : Float    := 0.0;

   procedure Write_Float_Sample is new Write_Sample (Float);

   procedure Write_Callback
     (Out_Stream      : access SoundIo_Out_Stream;
      Frame_Count_Min : int;
      Frame_Count_Max : int)
   is
      pragma Unreferenced (Frame_Count_Min);
      Layout            : SoundIo_Channel_Layout renames Out_Stream.Layout;
      Sample_Rate       : constant Float := Float (Out_Stream.Sample_Rate);
      Seconds_Per_Frame : constant Float := 1.0 / Sample_Rate;
      Areas             : SoundIo_Channel_Area_Ptr;
      Frames_Left       : int   := Frame_Count_Max;
      Sample            : Float;
      use Soundio_Channel_Area_Ptrs;

      Frames_Pitch : constant int := Out_Stream.Sample_Rate;

      Radians_S : constant Float := Pitch * 2.0 / Pi;

   begin
      Put_Line ("Write_Callback");
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

      Samples_Elapsed := Samples_Elapsed + Natural (Frame_Count_Max);
      Seconds_Elapsed := Samples_Elapsed / Natural (Out_Stream.Sample_Rate);
      Put_Line
         (Seconds_Elapsed'Img & "s (" & Samples_Elapsed'Img & " samples)");

      Err := Outstream_Begin_Write (Out_Stream, Areas, Frames_Left);

      while Frames_Left > 0 loop
         declare
            Pitch_Frame_Count : constant int := int'Min (Frames_Left, Frames_Pitch);
         begin
            Pitch :=
               Pitch_Min +
               Float (Seconds_Elapsed mod Pitch_Intervals) * Pitch_Step;
            for Frame in 0 .. Pitch_Frame_Count - 1 loop
               Sample :=
                  Sin
                     ((Pitch_Seconds_Offset +
                       Float (Frame) * Seconds_Per_Frame) *
                      Radians_S);

               --  TODO: Write Get_Area function that returns the Area at index
               --  Channel
               for Channel in 0 .. Layout.Channel_Count - 1 loop
                  Write_Float_Sample
                     (Get_Area (Areas, Channel), Frame, Sample);
               end loop;
            end loop;

            Frames_Left := Frames_Left - Pitch_Frame_Count;

            Put_Line
               ("Frames_Left: " & Frames_Left'Img & " Frames_Pitch" &
                Frames_Pitch'Img);
            if Frames_Left > 0 or Frames_Pitch = Frames_Left then
               --  Pitch change
               Pitch_Seconds_Offset := 0.0;
               Put_Line ("Pitch change");
            else
               --  No pitch change
               Pitch_Seconds_Offset :=
                  Pitch_Seconds_Offset +
                  (Seconds_Per_Frame * Float (Pitch_Frame_Count));
            end if;
         end;
      end loop;

      Err := Outstream_End_Write (Out_Stream);
   end Write_Callback;

end Gade_Audio;
