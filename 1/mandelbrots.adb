with Interfaces;

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


   procedure Compute_G (Set : in Set_T) is

      procedure Process (Point : in XCB.Point.T);

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

      use Interfaces;
   begin -- Compute_G
      for X in Set.Image_Rectangle.X ..
        Set.Image_Rectangle.X + Integer_16 (Set.Image_Rectangle.Width) loop
         for Y in Set.Image_Rectangle.Y ..
           Set.Image_Rectangle.Y + Integer_16 (Set.Image_Rectangle.Height) loop
            Process (Point => (X, Y));
         end loop;
      end loop;
   end Compute_G;

end Mandelbrots;
