with Gade.Interfaces;   use Gade.Interfaces;
with Gade_Window;       use Gade_Window;
with Audio.IO;
with Gade.Video_Buffer; use Gade.Video_Buffer;

with SDL.Timers; use SDL.Timers;

package Gade_Runner is

   type Instance is limited private;

   procedure Create (Runner : out Instance);

   procedure Step (Runner   : in out Instance;
                   G        : in out Gade_Type;
                   Window   : in out Gade_Window_Type;
                   Audio_IO : in out Audio.IO.Instance);

private

   Max_Frame_Rendering_Rate : constant := 60;
   Min_Frame_Duration : constant Milliseconds :=
     1000 / Max_Frame_Rendering_Rate;

   type Instance is record
      Last_Frame_Rendered_Ticks : Milliseconds;
   end record;

   procedure Generate_And_Render
     (G        : in out Gade_Type;
      Window   : in out Gade_Window_Type;
      Audio_IO : in out Audio.IO.Instance);

   procedure Generate_And_Discard
     (G        : in out Gade_Type;
      Audio_IO : in out Audio.IO.Instance);

   procedure Generate
     (G            : in out Gade_Type;
      Video_Buffer : RGB32_Display_Buffer_Access;
      Audio_IO     : in out Audio.IO.Instance);

end Gade_Runner;
