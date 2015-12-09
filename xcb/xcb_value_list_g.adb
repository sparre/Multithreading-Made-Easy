with Ada.Unchecked_Conversion;

package body XCB_Value_List_G is

   function Mask_Of (Values : in T) return Interfaces.Unsigned_32 is
      Result   : Mask_T := (others => False);
      Last_Key : Key_T := Key_T'First;
      function To_Integer is
         new Ada.Unchecked_Conversion (Mask_T, Interfaces.Unsigned_32);
   begin
      for K in Values'Range loop
         pragma Assert (K = Values'First or else Values (K).Key > Last_Key);
         Result (Values (K).Key) := True;
         Last_Key := Values (K).Key;
      end loop;
      return To_Integer (Result);
   end Mask_Of;

   function To_C (Values : in T) return Low_Level_T is
      Result : Low_Level_T (Values'Range);
   begin
      for K in Values'Range loop
         Result (K) := Values (K).Value;
      end loop;
      return Result;
   end To_C;

end XCB_Value_List_G;
