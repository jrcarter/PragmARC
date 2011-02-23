-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- History:
-- 2004 Apr 01     J. Carter          V1.3--Use Images for image functions
-- 2001 May 01     J. Carter          V1.2--Added expanded Image capabilities
-- 2000 Jul 01     J. Carter          V1.1--Added Split
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_Io;

with PragmARC.Images.Image;
with PragmARC.Mixed_Case;

use Ada;
use Ada.Characters.Handling;
use Ada.Strings;
use Ada.Strings.Fixed;
package body PragmARC.Date_Handler is
   procedure Split (Seconds : in out Calendar.Day_Duration; Hour : out Hour_Number; Minute : out Minute_Number) is
      Seconds_Per_Minute : constant := 60;
      Minutes_Per_Hour   : constant := 60;
      Seconds_Per_Hour   : constant := Minutes_Per_Hour * Seconds_Per_Minute;
   begin -- Split
      if Seconds >= Calendar.Day_Duration'Last then
         Seconds := 0.0;
         Hour    := 0;
         Minute  := 0;
         
         return;
      end if;
      
      Hour := Integer'Max (Integer (Seconds / Seconds_Per_Hour - 0.5), Hour_Number'First);
      Seconds := Seconds - Calendar.Day_Duration (Hour) * Seconds_Per_Hour;
      Minute := Integer'Max (Integer (Seconds / Seconds_Per_Minute - 0.5), Minute_Number'First);
      Seconds := Seconds - Calendar.Day_Duration (Minute) * Seconds_Per_Minute;
   end Split;
   
   procedure Split (Date    : in     Calendar.Time;
                    Year    :    out Calendar.Year_Number;
                    Month   :    out Calendar.Month_Number;
                    Day     :    out Calendar.Day_Number;
                    Hour    :    out Hour_Number;
                    Minute  :    out Minute_Number;
                    Seconds :    out Minute_Duration)
   is
      Secs : Calendar.Day_Duration;
   begin -- Split
      Calendar.Split (Date => Date, Year => Year, Month => Month, Day => Day, Seconds => Secs);
      Split (Seconds => Secs, Hour => Hour, Minute => Minute);
      Seconds := Secs;
   end Split;

   function Image (Value : Natural; Width : Natural := 0; Zero_Fill : Boolean := True) return String;
   -- Create a decimal image of Value with the specified width and zero-filling.
   -- If Width = 0 or the minimum length of the image >= Width, returns the minimum-length image.
   -- Otherwise, returns a string of length Width containing the image right justified.
   -- If Zero_Fill, the remaining characters of the result are set to '0', otherwise they are set to ' '.
   
   function Image (Value : Natural; False_Width : Natural; True_Width : Natural; Zero_Fill : Boolean) return String;
   -- if Zero_Fill, returns Image (Value, True_Width, Zero_Fill).
   -- Otherwise, returns Image (Value, False_Width, Zero_Fill).
   
   function Image (Value : Natural; Width : Natural := 0; Zero_Fill : Boolean := True) return String is
      -- null;
   begin -- Image
      return Images.Image (Value, Images.Field (Width), Zero_Fill);
   end Image;
   
   function Image (Value : Natural; False_Width : Natural; True_Width : Natural; Zero_Fill : Boolean) return String is
      Width : Natural := False_Width;
   begin -- Image
      if Zero_Fill then
         Width := True_Width;
      end if;
      
      return Image (Value, Width, Zero_Fill);
   end Image;
   
   function Year_Image_Short (Year : Positive; Zero_Fill : Boolean := True) return String is
      Century : constant := 100;
   begin -- Year_Image_Short
      return Image (Year rem Century, 0, 2, Zero_Fill);
   end Year_Image_Short;
   
   function Year_Image_Long (Year : Positive; Zero_Fill : Boolean := True; Width : Positive := 4) return String is
      -- null;
   begin -- Year_Image_Long
      return Image (Year, Width, Zero_Fill);
   end Year_Image_Long;

   function Month_Image_Numeric (Month : Calendar.Month_Number; Zero_Fill : Boolean := True) return String is
      -- null;
   begin -- Month_Image_Numeric
      return Image (Month, 0, 2, Zero_Fill);
   end Month_Image_Numeric;
   
   function Month_Image_Alpha (Month : Calendar.Month_Number; Casing : Case_Selection := Mixed; Name : Name_List) return String is
      Result : String := To_String (Name (Month) );
   begin -- Month_Image_Alpha
      case Casing is
      when Lower =>
         Result := To_Lower (Result);
      when Mixed =>
         Result := Mixed_Case (Result);
      when Upper =>
         Result := To_Upper (Result);
      when As_Is =>
         null;
      end case;
      
      return Result;
   end Month_Image_Alpha;
   
   function Day_Image (Day : Calendar.Day_Number; Zero_Fill : Boolean := True) return String is
      -- null;
   begin -- Day_Image
      return Image (Day, 0, 2, Zero_Fill);
   end Day_Image;

   function Hour_Image_12 (Hour : Hour_Number; AM_PM : AM_PM_List := Default_AM_PM_Name; Zero_Fill : Boolean := True)
   return String is
      AM_PM_Name : constant String := To_String (AM_PM (AM_PM_ID'Val (Boolean'Pos (Hour > 11) ) ) );
      
      Local_Hour : Natural := Hour;
   begin -- Hour_Image_12
      if Hour <= 0 then
         Local_Hour := 12;
      elsif Hour > 12 then
         Local_Hour := Hour - 12;
      else
         null;
      end if;
      
      return Image (Local_Hour, 0, 2, Zero_Fill) & AM_PM_Name;
   end Hour_Image_12;
   
   function Hour_Image_24 (Hour : Hour_Number; Zero_Fill : Boolean := True) return String is
      -- null;
   begin -- Hour_Image_24
      return Image (Hour, 0, 2, Zero_Fill);
   end Hour_Image_24;

   function Minute_Image (Minute : Minute_Number; Zero_Fill : Boolean := True) return String is
      -- null;
   begin -- Minute_Image
      return Image (Minute, 0, 2, Zero_Fill);
   end Minute_Image;

   function Seconds_Image (Seconds : Minute_Duration; Zero_Fill : Boolean := True; Aft : Natural := 0) return String is
      Result : String (1 .. 100 + Aft);
      Start  : Natural;
      
      package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);
      use Duration_IO;
   begin -- Seconds_Image
      if Seconds >= Minute_Duration'Last then
         return Seconds_Image (Minute_Duration'First, Zero_Fill, Aft);
      end if;
      
      if Aft <= 0 then
         return Image (Integer (Seconds), 0, 2, Zero_Fill);
      end if;
      
      Put (To => Result, Item => Seconds, Aft => Aft, Exp => 0);
      
      Start := Index_Non_Blank (Result);
      
      if Start > Result'First and Seconds < 10.0 and Zero_Fill then
         Start := Start - 1;
         Result (Start) := '0';
      end if;
      
      return Result (Start .. Result'Last);
   end Seconds_Image;
   
   function Image (Date : Calendar.Time) return String is
      Year    : Calendar.Year_Number;
      Month   : Calendar.Month_Number;
      Day     : Calendar.Day_Number;
      Hour    : Hour_Number;
      Minute  : Minute_Number;
      Seconds : Calendar.Day_Duration;
      
      Date_Separator : constant Character := ' ';
      Time_Separator : constant Character := ':';
   begin -- Image
      Split (Date => Date, Year => Year, Month => Month, Day => Day, Hour => Hour, Minute => Minute, Seconds => Seconds);
      
      return Year_Image_Long (Year)    & Date_Separator &
             Month_Image_Short (Month) & Date_Separator &
             Day_Image (Day)           & Date_Separator &
             Hour_Image_24 (Hour)      & Time_Separator &
             Minute_Image (Minute)     & Time_Separator &
             Seconds_Image (Seconds, Aft => 2);
   end Image;

   -- Day of week algorithm by Zeller, ACTA MATHEMATICA #7, Stockholm, 1887 (in German)
   function Day_Of_Week (Year : Positive; Month : Calendar.Month_Number; Day : Calendar.Day_Number) return Day_Name is
      Local_Year  : Natural  := Year;
      Local_Month : Positive := Month;
      Century     : Natural;
      Position    : Integer;
   begin -- Day_Of_Week
      if Local_Month < 3 then
         Local_Month := Local_Month + 12;
         Local_Year := Local_Year - 1;
      end if;

      Century := Local_Year / 100;
      Local_Year := Local_Year rem 100;
      Position := Day + (26 * (Local_Month + 1) ) / 10 + Local_Year + Local_Year / 4 + Century / 4 - 2 * Century;
      Position := Position mod 7;

      if Position = 0 then
         Position := 7;
      end if;

      Position := Position - 1;

      return Day_Name'Val (Position);
   end Day_Of_Week;

   function Day_Of_Week (Date : Calendar.Time) return Day_Name is
      Year    : Calendar.Year_Number;
      Month   : Calendar.Month_Number;
      Day     : Calendar.Day_Number;
      Seconds : Calendar.Day_Duration;
   begin -- Day_Of_Week
      Calendar.Split (Date => Date, Year => Year, Month => Month, Day => Day, Seconds => Seconds);

      return Day_Of_Week (Year => Year, Month => Month, Day => Day);
   end Day_Of_Week;

   function Leap_Year (Year : Positive) return Boolean is
   begin -- Leap_Year
      return (Year rem 100 /= 0 and Year rem 4 = 0) or (Year rem 400 = 0);
   end Leap_Year;

   function Leap_Year (Date : Calendar.Time) return Boolean is
   begin -- Leap_Year
      return Leap_Year (Calendar.Year (Date) );
   end Leap_Year;

   function Days_In_Month (Year : Positive; Month : Calendar.Month_Number) return Calendar.Day_Number is
      type Day_Set is array (Calendar.Month_Number) of Calendar.Day_Number;

      Normal : constant Day_Set := (01 => 31, 02 => 28, 03 => 31, 04 => 30, 05 => 31, 06 => 30,
                                    07 => 31, 08 => 31, 09 => 30, 10 => 31, 11 => 30, 12 => 31);

      Result : Calendar.Day_Number := Normal (Month);
   begin -- Days_In_Month
      if Month = 2 and Leap_Year (Year) then -- Feb is special
         Result := Result + 1;
      end if;

      return Result;
   end Days_In_Month;

   function Days_In_Month (Date : Calendar.Time) return Calendar.Day_Number is
      Year    : Calendar.Year_Number;
      Month   : Calendar.Month_Number;
      Day     : Calendar.Day_Number;
      Seconds : Calendar.Day_Duration;
   begin -- Days_In_Month
      Calendar.Split (Date => Date, Year => Year, Month => Month, Day => Day, Seconds => Seconds);

      return Days_In_Month (Year, Month);
   end Days_In_Month;
end PragmARC.Date_Handler;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.