with Interfaces;

generic
   type Key_T is (<>);
package XCB_Value_List_G is
   type Key_Value_Pair_T is record
      Key    : Key_T;
      Value  : Interfaces.Unsigned_32;
   end record;
   type T is array (Positive range <>) of Key_Value_Pair_T;

   type Mask_T is array (Key_T) of Boolean;
   pragma Pack (Mask_T);
   pragma Warnings (Off, Mask_T); -- unused bits

   function Mask_Of (Values : in T) return Interfaces.Unsigned_32;
   pragma Inline (Mask_Of);

   type Low_Level_T is array (Positive range <>) of Interfaces.Unsigned_32;
   function To_C (Values : in T) return Low_Level_T;
   pragma Inline (To_C);

end XCB_Value_List_G;
