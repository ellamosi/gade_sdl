with Interfaces.C; use Interfaces.C;
with Soundio; use Soundio;

with Gade_Window; use Gade_Window;
with Gade.Interfaces; use Gade.Interfaces;

with Ada.Unchecked_Conversion;
with System;
with Ada.Streams.Stream_IO;

with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Ada.Calendar; use Ada.Calendar;

package Gade_Audio is

   type Double_Buffer_Range is range 0 .. 1;
   type Double_Audio_Buff is array (Double_Buffer_Range) of aliased Audio_Buffer_Type;

   type Stream_Context_Type is record
      Window           : Gade_Window_Type;
      G                : Gade_Type;
      Edge_Samples     : Natural := 0;
      Edge_State       : Boolean := False;
      Audio_Buff       : Double_Audio_Buff;
      Cur_Buff         : Double_Buffer_Range;
      File_Stream      : Ada.Streams.Stream_IO.Stream_Access;
      Rem_Samples : Natural := 0;
      Rem_Index   : Natural := 0;

      FPS_Count : Natural := 0;
      FPS_Time : Time;
      Frame_Samples : Natural := 0;
   end record;

   type Stream_Context_Access is access Stream_Context_Type;

   function Convert is new Ada.Unchecked_Conversion
     (Source => System.Address,
      Target => Stream_Context_Access);

   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int);
   pragma Convention (C, Write_Callback);

end Gade_Audio;
