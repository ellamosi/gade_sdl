package body Gade_Runner is

   protected body Protected_Gade_Runner is

      procedure Create is
      begin
         Create (Gade);
         Protected_Audio_Buffer := new Audio_Buffer_Type;
         --  Video buffer should be allocated by SDL?
         Protected_Video_Buffer := new RGB32_Display_Buffer;
      end Create;

      procedure Load_ROM (File_Name : String) is
      begin
         Load_ROM (Gade, File_Name);
      end Load_ROM;

      entry Get_Audio_Samples
        (Audio_Buffer      : out Audio_Buffer_Access;
         Requested_Samples : in  Positive;
         Actual_Samples    : out Positive)
        when not Video_Frame_Ready is
         Frame_Finished : Boolean;
      begin
         Run_For (Gade,
                  Requested_Samples * 4,
                  Actual_Samples,
                  Protected_Video_Buffer,
                  Protected_Audio_Buffer,
                  Frame_Finished);

         Actual_Samples := Actual_Samples / 4;

         Video_Frame_Ready := Frame_Finished;
         Audio_Buffer := Protected_Audio_Buffer;
      end Get_Audio_Samples;

      entry Get_Video_Frame (Video_Buffer : out RGB32_Display_Buffer_Access)
        when Video_Frame_Ready is
      begin
         --  Use double buffer for textures. Do all locks/unlocks here (unlock
         --  one while locking the other).
         Video_Frame_Ready := False;
         Video_Buffer := Protected_Video_Buffer;
         --  Could unlock texture here, lock on first Run_For invocation after
         --  Should use update texture instead maybe, but not supported by binding
      end Get_Video_Frame;

   end Protected_Gade_Runner;

end Gade_Runner;
