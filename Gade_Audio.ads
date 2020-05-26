with Interfaces.C; use Interfaces.C;
with Soundio; use Soundio;

package Gade_Audio is
   procedure Write_Callback
     (Out_Stream       : access SoundIo_Out_Stream;
      Frame_Count_Min  : int;
      Frame_Count_Max  : int);
   pragma Convention (C, Write_Callback);
end Gade_Audio;
