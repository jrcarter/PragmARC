-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provides an image function for type Calendar.Time, day of the week functions, leap-year
-- functions, days-in-month functions, and functions to split Seconds to Hours, Minutes, and
-- Seconds
--
-- History:
-- 2001 May 01     J. Carter          V1.2--Added expanded Image capabilities
-- 2000 Jul 01     J. Carter          V1.1--Added Split
-- 2000 May 01     J. Carter          V1.0--Initial release
--
with Ada.Calendar;
with Ada.Strings.Unbounded;

use Ada;
use Ada.Strings.Unbounded;
package PragmARC.Date_Handler is
   subtype Hour_Number     is Integer range 0 .. 23;
   subtype Minute_Number   is Integer range 0 .. 59;
   subtype Minute_Duration is Calendar.Day_Duration range 0.0 .. 60.0;
   
   procedure Split (Seconds : in out Calendar.Day_Duration; Hour : out Hour_Number; Minute : out Minute_Number);
   -- Splits Seconds into Hour, Minute, and Seconds within Minute
   -- If Seconds = Calendar.Day_Duration'Last, all results are zero (00:00:00.000 of next day)
   --
   -- Postcondition: after[Seconds] in Minute_Duration
   
   procedure Split (Date    : in     Calendar.Time;
                    Year    :    out Calendar.Year_Number;
                    Month   :    out Calendar.Month_Number;
                    Day     :    out Calendar.Day_Number;
                    Hour    :    out Hour_Number;
                    Minute  :    out Minute_Number;
                    Seconds :    out Minute_Duration);
   -- Splits Date using Split to obtain Hour, Minute, and Seconds within Minute
   
   -- Image operations:
   
   -- Image functions with a Zero_Fill parameter may have extra leading characters in the result.
   -- If Zero_Fill, these will be '0'; otherwise they will be ' '.

   -- Provide the image of any year CE:
   function Year_Image_Short (Year : Positive; Zero_Fill : Boolean := True) return String;
   -- Returns the decimal image of Year rem 100.
   -- If Zero_Fill, result will always be 2 characters long. Otherwise, result will be 1 or 2 characters long.
   
   function Year_Image_Long  (Year : Positive; Zero_Fill : Boolean := True; Width : Positive := 4) return String;
   -- Returns the decimal image of Year. Result will be large enough to hold this image, or Width characters long,
   -- whichever is larger.

   type Case_Selection is (Lower, Mixed, Upper, As_Is); -- Determines the formatting of month names

   type Name_List is array (Calendar.Month_Number) of Unbounded_String; -- Used to provide month names

   Default_Short_Name : constant Name_List := (01 => To_Unbounded_String ("Jan"), 02 => To_Unbounded_String ("Feb"),
                                               03 => To_Unbounded_String ("Mar"), 04 => To_Unbounded_String ("Apr"),
                                               05 => To_Unbounded_String ("May"), 06 => To_Unbounded_String ("Jun"),
                                               07 => To_Unbounded_String ("Jul"), 08 => To_Unbounded_String ("Aug"),
                                               09 => To_Unbounded_String ("Sep"), 10 => To_Unbounded_String ("Oct"),
                                               11 => To_Unbounded_String ("Nov"), 12 => To_Unbounded_String ("Dec") );
                                               -- Default "short" month names
                                               
   Default_Long_Name  : constant Name_List := (01 => To_Unbounded_String ("January"),
                                               02 => To_Unbounded_String ("February"),
                                               03 => To_Unbounded_String ("March"),
                                               04 => To_Unbounded_String ("April"),
                                               05 => To_Unbounded_String ("May"),
                                               06 => To_Unbounded_String ("June"),
                                               07 => To_Unbounded_String ("July"),
                                               08 => To_Unbounded_String ("August"),
                                               09 => To_Unbounded_String ("September"),
                                               10 => To_Unbounded_String ("October"),
                                               11 => To_Unbounded_String ("November"),
                                               12 => To_Unbounded_String ("December") );
                                               -- Default "long" month names

   function Month_Image_Numeric (Month : Calendar.Month_Number; Zero_Fill : Boolean := True) return String;
   -- Returns the decimal image of Month.
   -- If Zero_Fill, result will always be 2 characters long. Otherwise, result will be 1 or 2 characters long.
   
   function Month_Image_Alpha (Month : Calendar.Month_Number; Casing : Case_Selection := Mixed; Name : Name_List) return String;
   -- Returns To_String (Name (Month) ), formatted as directed by Casing.
   
   -- Renamings for default month names:
   function Month_Image_Short
      (Month : Calendar.Month_Number; Casing : Case_Selection := Mixed; Name : Name_List := Default_Short_Name)
   return String renames Month_Image_Alpha;
   function Month_Image_Long
      (Month : Calendar.Month_Number; Casing : Case_Selection := Mixed; Name : Name_List := Default_Long_Name)
   return String renames Month_Image_Alpha;

   function Day_Image (Day : Calendar.Day_Number; Zero_Fill : Boolean := True) return String;
   -- Returns the decimal image of Day.
   -- If Zero_Fill, result will always be 2 characters long. Otherwise, result will be 1 or 2 characters long.

   type AM_PM_ID is (AM, PM);
   type AM_PM_List is array (AM_PM_ID) of Unbounded_String;

   Default_AM_PM_Name : constant AM_PM_List := (AM => To_Unbounded_String (" am"), PM => To_Unbounded_String (" pm") );

   function Hour_Image_12 (Hour : Hour_Number; AM_PM : AM_PM_List := Default_AM_PM_Name; Zero_Fill : Boolean := True) return String;
   -- If Hour = 0, Image is "12". If Hour in 1 .. 12, Image is image of Hour. Otherwise, Image is image of Hour - 12.
   -- If Hour < 12, returns Image & To_String (AM_PM (AM) ). Otherwise, returns Image & To_String (AM_PM (PM) ).
   -- If Zero_Fill, Image will always be 2 characters long. Otherwise, Image will be 1 or 2 characters long.
   
   function Hour_Image_24 (Hour : Hour_Number; Zero_Fill : Boolean := True) return String;
   -- Returns the decimal image of Hour.
   -- If Zero_Fill, result will always be 2 characters long. Otherwise, result will be 1 or 2 characters long.

   function Minute_Image (Minute : Minute_Number; Zero_Fill : Boolean := True) return String;
   -- Returns the decimal image of Minute.
   -- If Zero_Fill, result will always be 2 characters long. Otherwise, result will be 1 or 2 characters long.

   function Seconds_Image (Seconds : Minute_Duration; Zero_Fill : Boolean := True; Aft : Natural := 0) return String;
   -- Returns the decimal image of Seconds, with Aft digits after the decimal point.
   -- If Aft = 0, result will not contain a decimal point.
   -- If Aft = 0 and Zero_Fill, result will always be 2 characters long.
   -- If Aft = 0 and not Zero_Fill, result will be 1 or 2 characters long.
   -- If Aft > 0 and Zero_Fill, the portion of result left of the decimal point will always be 2 characters long.
   -- Otherwise, the portion of result left of the decimal point may be 1 or 2 characters long.
   -- If Seconds = Minute_Duration'Last, returns the image of 0.0.
   
   function Image (Date : Calendar.Time) return String;
   -- Splits Date into Year, Month, Day, Hour, Minute, and Seconds, then
   -- returns the 23-character string resulting from
   --    Year_Image_Long (Year)    & ' ' &
   --    Month_Image_Short (Month) & ' ' &
   --    Day_Image (Day)           & ' ' &
   --    Hour_Image_24 (Hour)      & ':' &
   --    Minute_Image (Minute)     & ':' &
   --    Seconds_Image (Seconds, Aft => 2);

   type Day_Name is (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

   function Day_Of_Week (Year : Positive; Month : Calendar.Month_Number; Day : Calendar.Day_Number) return Day_Name;
   function Day_Of_Week (Date : Calendar.Time) return Day_Name;
   -- These 2 functions provide the day of the week for a given date
   -- Year is interpreted as being in the Common Era

   function Leap_Year (Year : Positive) return Boolean;
   function Leap_Year (Date : Calendar.Time) return Boolean;
   -- These 2 functions return True if the given year is a leap year; False otherwise
   -- Year is interpreted as being in the Common Era

   function Days_In_Month (Year : Positive; Month : Calendar.Month_Number) return Calendar.Day_Number;
   function Days_In_Month (Date : Calendar.Time) return Calendar.Day_Number;
   -- These 2 functions return the number of days in the given month for the given year
   -- Year is interpreted as being in the Common Era
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