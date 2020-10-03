with Ada.Calendar;

with SDL.Video.Windows;
with SDL.Video.Textures;
with SDL.Video.Renderers;

with Gade.Interfaces; use Gade.Interfaces;

package Gade_Window is

   type Gade_Window_Type is record
      Window      : SDL.Video.Windows.Window;
      Texture     : SDL.Video.Textures.Texture;
      Renderer    : SDL.Video.Renderers.Renderer;
      Timer       : Ada.Calendar.Time;
      Frame_Count : Natural;
   end record;

   procedure Create
     (Window : out Gade_Window_Type);

   procedure Next_Frame
     (Window : in out Gade_Window_Type;
      G      : in out Gade_Type);

   procedure Finalize
     (Window : in out Gade_Window_Type);

end Gade_Window;
