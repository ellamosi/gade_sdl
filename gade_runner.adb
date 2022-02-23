with Gade.Audio_Buffer; use Gade.Audio_Buffer;

package body Gade_Runner is
   --  TODO: Possibly rename to Renderer

   procedure Create (Runner : out Instance) is
   begin
      Runner.Last_Frame_Rendered_Ticks := 0;
   end Create;

   procedure Step
     (Runner   : in out Instance;
      G        : in out Gade_Type;
      Window   : in out Gade_Window_Type;
      Audio_IO : in out Audio.IO.Instance)
   is
      Ticks : constant Milliseconds := SDL.Timers.Ticks;
   begin
      if Ticks - Runner.Last_Frame_Rendered_Ticks >= Min_Frame_Duration then
         Generate_And_Render (G, Window, Audio_IO);
         Runner.Last_Frame_Rendered_Ticks := Ticks;
      else
         Generate_And_Discard (G, Audio_IO);
      end if;
   end Step;

   procedure Generate_And_Render
     (G        : in out Gade_Type;
      Window   : in out Gade_Window_Type;
      Audio_IO : in out Audio.IO.Instance)
   is
      procedure Generate_Frame (Buffer : RGB32_Display_Buffer_Access);
      procedure Generate_Frame (Buffer : RGB32_Display_Buffer_Access) is
      begin
         Generate (G, Buffer, Audio_IO);
      end Generate_Frame;

      procedure Render_Frame is new Gade_Window.Render_Frame (Generate_Frame);
   begin
      Render_Frame (Window);
   end Generate_And_Render;

   procedure Generate_And_Discard
     (G        : in out Gade_Type;
      Audio_IO : in out Audio.IO.Instance)
   is
      Null_Buffer : aliased RGB32_Display_Buffer;
      Buffer_Access : constant RGB32_Display_Buffer_Access
        := Null_Buffer'Unchecked_Access;
   begin
      Generate (G, Buffer_Access, Audio_IO);
   end Generate_And_Discard;

   procedure Generate
     (G            : in out Gade_Type;
      Video_Buffer : RGB32_Display_Buffer_Access;
      Audio_IO     : in out Audio.IO.Instance)
   is
      Requested_Samples : constant Natural := Samples_Frame;
      Actual_Samples    : Natural; -- Per frame
      Frame_Finished    : Boolean := False;

      procedure Generate_Samples (Audio_Buffer : Audio_Buffer_Access;
                                  Count        : out Natural);
      procedure Generate_Samples (Audio_Buffer : Audio_Buffer_Access;
                                  Count        : out Natural) is
      begin
         Run_For (G,
                  Requested_Samples,
                  Actual_Samples,
                  Video_Buffer,
                  Audio_Buffer,
                  Frame_Finished);

         Actual_Samples := Actual_Samples / 4;
         Count := Actual_Samples;
      end Generate_Samples;

      procedure Queue is new Audio.IO.Queue_Asynchronously (Generate_Samples);
   begin
      while not Frame_Finished loop
         Queue (Audio_IO);
      end loop;
   end Generate;

end Gade_Runner;
