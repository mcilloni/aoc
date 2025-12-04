--  AoC 2025 Day 1

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

procedure P01 is
   type Dial_Value is mod 100;

   Malformed_Input : exception;
   Wrong_Arguments : exception;

   function Parse_Instruction
      (Line : String) return Integer is
   begin
      if Line'Length < 2 then
         raise Malformed_Input;
      end if;

      return (case Line (Line'First) is
         when 'R' => 1,
         when 'L' => -1,
         when others => raise Malformed_Input
      ) * Integer'Value (Line (Line'First + 1 .. Line'Last));
   end Parse_Instruction;

   procedure Solve
      (F : File_Type;
       Part1_Result : out Integer;
       Part2_Result : out Integer)
   with
      Pre => Is_Open (F) and then not End_Of_File (F)
   is
      --  The initial dial value is 50.
      Dial : Dial_Value := 50;
   begin
      Part1_Result := 0;
      Part2_Result := 0;

      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
            Modulus : constant Integer := Integer (Dial_Value'Modulus);

            New_Value : Integer;
            Offset : Integer;
            AbsOffset : Integer;
            IsAtZero : Integer;
         begin
            --  ignore empty lines
            if Line'Length /= 0 then
               Offset := Parse_Instruction (Line);
               AbsOffset := abs Offset;

               New_Value := Integer (Dial) + Offset;

               IsAtZero := (if Dial = 0 then 1 else 0);

               Part1_Result := Part1_Result + IsAtZero;

               if Offset > 0 then
                  --  moving right
                  Part2_Result := Part2_Result + New_Value / Modulus;
               else
                  if Dial = 0 then
                     Part2_Result := Part2_Result + AbsOffset / Modulus;
                  elsif AbsOffset >= Integer (Dial) then
                     Part2_Result := Part2_Result + (AbsOffset - Integer (Dial)) / Modulus + 1;
                  end if;
               end if;

               Dial := Dial_Value'Mod (New_Value);
            end if;
         end;
      end loop;
   end Solve;

   F : File_Type;
   Code : Exit_Status := Success;

   Part1_Result : Integer := 0;
   Part2_Result : Integer := 0;
begin
   begin
      if Argument_Count /= 1 then
         raise Wrong_Arguments;
      end if;

      Open (F, In_File, Argument (1));

      Solve (F, Part1_Result, Part2_Result);

      Put_Line ("Part 1: " & Integer'Image (Part1_Result));
      Put_Line ("Part 2: " & Integer'Image (Part2_Result));
   exception
      when Malformed_Input =>
         Put_Line (Standard_Error, "error: malformed input");
         Code := Failure;

      when Wrong_Arguments =>
         Put_Line (Standard_Error, "error: wrong number of arguments");
         Put_Line (Standard_Error, "usage: p01 <input>");
         Code := Failure;

      when E : others =>
         Put_Line (Standard_Error, "error: " & Exception_Message (E));
         Code := Failure;
   end;

   if Is_Open (F) then
      Close (F);
   end if;

   Set_Exit_Status (Code);
end P01;
