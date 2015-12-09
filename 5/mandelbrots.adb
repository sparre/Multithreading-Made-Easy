with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

package body Mandelbrots is

   function Create (Image_Rectangle : in XCB.Rectangle.T;
                    Bottom_Left     : in Complex;
                    Top_Right       : in Complex;
                    Max_Iterations  : in Iterations_T) return Set_T is
      Bottom_Left_In_Image : constant Complex
        := (Re => Long_Float (Image_Rectangle.X),
            Im => Long_Float (Image_Rectangle.Y)
              - Long_Float (Image_Rectangle.Height));
      Top_Right_In_Image : constant Complex
        := (Re => Long_Float (Image_Rectangle.X)
              + Long_Float (Image_Rectangle.Width),
            Im => Long_Float (Image_Rectangle.Y));
   begin
      return
        (Image_Rectangle      => Image_Rectangle,
         Bottom_Left          => Bottom_Left,
         Top_Right            => Top_Right,
         Max_Iterations       => Max_Iterations,
         Bottom_Left_In_Image => Bottom_Left_In_Image,
         Top_Right_In_Image   => Top_Right_In_Image,
         Factor               =>
           (Re => (Top_Right.Re - Bottom_Left.Re)
              / (Top_Right_In_Image.Re - Bottom_Left_In_Image.Re),
            Im => (Top_Right.Im - Bottom_Left.Im)
              / (Top_Right_In_Image.Im - Bottom_Left_In_Image.Im)));
   end Create;

   package Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => XCB.Rectangle.T);
   package Queues is new Ada.Containers.Unbounded_Synchronized_Queues
     (Queue_Interfaces => Queue_Interfaces);


   procedure Compute_G (Set              : in Set_T;
                        Sub_Rectangles_X : in Sub_Rectangles_T;
                        Sub_Rectangles_Y : in Sub_Rectangles_T) is

      procedure Process (Point : in XCB.Point.T);
      procedure Process (Rectangle : in XCB.Rectangle.T);

      procedure Process (Point : in XCB.Point.T) is
         use type Interfaces.Integer_16;
         Point_In_Image : constant Complex
           := (Re => Long_Float (Point.X),
               Im => Long_Float (Set.Image_Rectangle.Y - Point.Y));
         Point_In_Complex_Plane : constant Complex
           := (Re => Set.Bottom_Left.Re
                 + (Point_In_Image.Re - Set.Bottom_Left_In_Image.Re)
                 * Set.Factor.Re,
               Im => Set.Bottom_Left.Im
                 + (Point_In_Image.Im - Set.Bottom_Left_In_Image.Im)
                 * Set.Factor.Im);
         Zn : Complex := Point_In_Complex_Plane;
         Iterations : Iterations_T := 0;
      begin
         while abs Zn < 2.0  and then Iterations <= Set.Max_Iterations loop
            Iterations := Iterations + 1;
            Zn := Zn * Zn + Point_In_Complex_Plane;
         end loop;
         Draw (Point, Iterations);
      end Process;

      procedure Process (Rectangle : in XCB.Rectangle.T) is
         use Interfaces;
      begin
         for X in Rectangle.X .. Rectangle.X + Integer_16 (Rectangle.Width)
         loop
            for Y in Rectangle.Y .. Rectangle.Y + Integer_16 (Rectangle.Height)
            loop
               Process (Point => (X, Y));
            end loop;
         end loop;
      end Process;

      task type Rectangle_Worker is
         entry Start;
      end Rectangle_Worker;

      Q : Queues.Queue;

      task body Rectangle_Worker is
         Rectangle : XCB.Rectangle.T;
         use type Ada.Containers.Count_Type;
      begin
         accept Start;
         loop
            select
               Q.Dequeue (Rectangle);
               Process (Rectangle);
            else
               exit;
            end select;
         end loop;
      end Rectangle_Worker;

      Workers : array (1 .. 4) of Rectangle_Worker;

      use type Interfaces.Unsigned_16;
      use type Interfaces.Integer_16;
      Sub_Rectangles_Width : constant Interfaces.Unsigned_16
        := Set.Image_Rectangle.Width / Sub_Rectangles_X;
      Sub_Rectangles_Height : constant Interfaces.Unsigned_16
        := Set.Image_Rectangle.Height / Sub_Rectangles_Y;
   begin -- Compute_G
      for X in 1 .. Sub_Rectangles_X loop
         for Y in 1 .. Sub_Rectangles_Y loop
            Q.Enqueue
              ((X => Set.Image_Rectangle.X
                  + Interfaces.Integer_16 ((X - 1) * Sub_Rectangles_Width),
                Y => Set.Image_Rectangle.Y
                  + Interfaces.Integer_16 ((Y - 1) * Sub_Rectangles_Height),
                Width  => Sub_Rectangles_Width,
                Height => Sub_Rectangles_Height));
         end loop;
      end loop;
      for W in Workers'Range loop
         Workers (W).Start;
      end loop;
   end Compute_G;

end Mandelbrots;
