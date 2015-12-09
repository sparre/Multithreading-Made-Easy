with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces;
with Mandelbrots;

with XCB;

procedure Fosdem2015 is
   C : constant XCB.Connection.T := XCB.Connection.Connect;
   Setup : constant XCB.Setup.T := XCB.Setup.Get (C);
   Screen : constant XCB.Screen.T := XCB.Screen.Root_Iterator (Setup).Data.all;
   Window : constant XCB.Window.T := XCB.Window.Generate_Id (C);
   use type Interfaces.Unsigned_32;

   protected Painter is
      procedure Initialize;
      procedure Draw (Point      : in XCB.Point.T;
                      Iterations : in Mandelbrots.Iterations_T);
   private
      GC : XCB.GC.T := XCB.GC.Generate_Id (C);
   end Painter;


   protected body Painter is
      procedure Initialize is
      begin
         XCB.GC.Create
           (C,
            Id => GC,
            Drawable => XCB.Drawable_T (Screen.Root),
            Values   =>
              ((Key => XCB.GC.Foreground,
                Value => Interfaces.Unsigned_32 (Screen.White_Pixel)),
               (Key => XCB.GC.Graphics_Exposures, Value => 0)));
      end Initialize;

      procedure Draw (Point      : in XCB.Point.T;
                      Iterations : in Mandelbrots.Iterations_T) is
         use Interfaces;
      begin
         XCB.GC.Change (C,
                        Id     => GC,
                        Values => (1 => (Key   => XCB.GC.Foreground,
                                         Value => Unsigned_32 (Iterations)
                                           * (2 ** 24 / 10_000))));
         XCB.Point.Poly (C,
                         Drawable => XCB.Drawable_T (Window),
                         Context  => GC,
                         Length   => 1,
                         Points   => (1 => (Point.X, Point.Y)));
         XCB.Connection.Flush (C);
      end Draw;
   end Painter;

   procedure Draw is new Mandelbrots.Compute_G (Painter.Draw);

   type Mandelbrot_Set_Access is access Mandelbrots.Set_T;
   procedure Free is new Ada.Unchecked_Deallocation (Mandelbrots.Set_T,
                                                     Mandelbrot_Set_Access);
   Mandelbrot_Set : Mandelbrot_Set_Access;

begin
   Painter.Initialize;
   XCB.Window.Create
     (C,
      Depth  => 0, -- copy_from_parent
      Wid    => Window,
      Parent => Screen.Root,
      X     => 0,   Y => 0,
      Width => 150, Height => 150,
      Border_Width => 0,
      Class  => XCB.Window.Input_Output,
      Visual => Screen.Root_Visual,
      Values =>
        ((Key   => XCB.Window.Back_Pixel,
          Value => Interfaces.Unsigned_32 (Screen.White_Pixel)),
         XCB.Event.Mask.To_Value ((XCB.Event.Mask.Key_Press
                                     | XCB.Event.Mask.Exposure
                                     | XCB.Event.Mask.Structure_Notify => True,
                                   others => False))));
   XCB.Window.Map (C, Window);
   XCB.Connection.Flush (C);

   loop
      declare
         E : XCB.Event.T := XCB.Event.Wait_For_Event (C);
         use type XCB.Event.T;
         Exit_Requested : Boolean := False;
      begin
         exit when E = null;
         case E.Response_Type is
            when XCB.Event.Configure_Notify =>
               Free (Mandelbrot_Set);
               Mandelbrot_Set := new Mandelbrots.Set_T'
                 (Mandelbrots.Create
                    (Image_Rectangle => (0, 0, E.Width, E.Height),
                     Bottom_Left     => (-2.0, -1.0),
                     Top_Right       => (1.0,   1.0),
                     Max_Iterations  => 10_000));
            when XCB.Event.Expose =>
               if Mandelbrot_Set /= null then
                  Draw (Mandelbrot_Set.all,
                        Rectangle        =>
                          (Interfaces.Integer_16 (E.Expose_X),
                           Interfaces.Integer_16 (E.Expose_Y),
                           E.Expose_Width, E.Expose_Height),
                        Sub_Rectangles_X => 100,
                        Sub_Rectangles_Y => 100);
               end if;
            when XCB.Event.Key_Press =>
               Exit_Requested := True;
            when XCB.Event.Error =>
               Ada.Text_IO.Put_Line ("X protocol error");
            when others =>
               null;
         end case;
         XCB.Event.Free (E);
         exit when Exit_Requested;
      end;
   end loop;

   XCB.Connection.Disconnect (C);
   Free (Mandelbrot_Set);
end Fosdem2015;
