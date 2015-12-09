with Ada.Numerics.Long_Complex_Types;
with Interfaces;
with XCB;

package Mandelbrots is

   use Ada.Numerics.Long_Complex_Types;

   type Iterations_T is new Natural; -- a number of iterations

   type Set_T (<>) is private;

   function Create (Image_Rectangle : in XCB.Rectangle.T;
                    Bottom_Left     : in Complex;
                    Top_Right       : in Complex;
                    Max_Iterations  : in Iterations_T) return Set_T
     with Inline;

   subtype Sub_Rectangles_T is
     Interfaces.Unsigned_16 range 1 .. Interfaces.Unsigned_16'Last;

   generic
      with procedure Draw (Point      : in XCB.Point.T;
                           Iterations : in Iterations_T);
   procedure Compute_G (Set              : in Set_T;
                        Sub_Rectangles_X : in Sub_Rectangles_T;
                        Sub_Rectangles_Y : in Sub_Rectangles_T);

private
   type Set_T is record
      Image_Rectangle      : XCB.Rectangle.T;
      Bottom_Left          : Complex;
      Top_Right            : Complex;
      Max_Iterations       : Iterations_T;
      Bottom_Left_In_Image : Complex;
      Top_Right_In_Image   : Complex;
      Factor               : Complex;
   end record;
end Mandelbrots;
