/*
  RMLib: Nonvisual support classes used by multiple R&M Software programs
  Copyright (C) 2008-2014  Rick Parrish, R&M Software

  This file is part of RMLib.

  RMLib is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  any later version.

  RMLib is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with RMLib.  If not, see <http://www.gnu.org/licenses/>.
*/
using System;
using System.Text;
using System.Threading;
using System.Globalization;
using System.Collections.Generic;

namespace RandM.RMLib
{
    /// <summary>
    /// A static class for manipulating a console window
    /// Compatibility with the Borland Pascal CRT unit was attempted, along with a few new additions
    /// </summary>
    static public class Crt
    {
        /* Private variables */
        static private Queue<char> _KeyBuf = new Queue<char>();
        static private object _Lock = new object();
        static private int _NormAttr = LightGray;
        static private NativeMethods.COORD _ScreenSize = new NativeMethods.COORD();
        static private IntPtr _StdInputHandle = IntPtr.Zero;
        static private IntPtr _StdOutputHandle = IntPtr.Zero;
        static private int _TextAttr = LightGray;
        static private int _TextMode = CO80;

        /*  Color Constants
          ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
         Use these color constants with SetPalette, SetAllPalette, TextColor, and
         TextBackground:
        */
        public const int Black = 0;
        public const int Blue = 1;
        public const int Green = 2;
        public const int Cyan = 3;
        public const int Red = 4;
        public const int Magenta = 5;
        public const int Brown = 6;
        public const int LightGray = 7;
        public const int DarkGray = 8;
        public const int LightBlue = 9;
        public const int LightGreen = 10;
        public const int LightCyan = 11;
        public const int LightRed = 12;
        public const int LightMagenta = 13;
        public const int Yellow = 14;
        public const int White = 15;
        public const int Blink = 128;

        /*   Constants Used as Parameters to TextMode (Crt unit)
          ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
        */
        public const int BW40 = 0;       // 40x25 B/W         ¦   CGA
        public const int CO40 = 1;       // 40x25 Color       ¦   CGA
        public const int BW80 = 2;       // 80x25 B/W         ¦   CGA
        public const int CO80 = 3;       // 80x25 Color       ¦   CGA
        public const int Mono = 7;       // 80x25 B/W on MDA  ¦   HGC
        public const int Font8x8 = 256;  // 43-/50-line mode  ¦   EGA/VGA
        public const int C40 = CO40;     // 3.0 compatibility
        public const int C80 = CO80;     // 3.0 compatibility

        #region ReadKey() Key Mappings
        static private int[] TKeys = {
                 0,    0,    0,    0,    0,    0,    0,    0,    8,    9,    0,    0,    0,   13,    0,    0,  // 0..15
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   27,    0,    0,    0,    0,  // 16..31
                32, 1073, 1081, 1079, 1071, 1075, 1072, 1077, 1080,    0,    0,    0,    0, 1082, 1083,    0,  // 32..47
                48,   49,   50,   51,   52,   53,   54,   55,   56,   57,    0,    0,    0,    0,    0,    0,  // 48..63
                 0,   97,   98,   99,  100,  101,  102,  103,  104,  105,  106,  107,  108,  109,  110,  111,  // 64..79
               112,  113,  114,  115,  116,  117,  118,  119,  120,  121,  122,    0,    0,    0,    0,    0,  // 80..95
              1082, 1079, 1080, 1081, 1075, 1076, 1077, 1071, 1072, 1073,   42,   43,    0,   45,   46,   47,  // 96..111
              1059, 1060, 1061, 1062, 1063, 1064, 1065, 1066, 1067, 1068, 1133, 1134,    0,    0,    0,    0,  // 112..127
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 128..143
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 144..159
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 160..175
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   59,   61,   44,   45,   46,   47,  // 176..191
                96,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 192..207
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   91,   92,   93,   39,    0,  // 208..223
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 223..239
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0}; // 240..255

        static private int[] TAltKeys = {
                 0,    0,    0,    0,    0,    0,    0,    0, 1008,    0,    0,    0,    0,    0,    0,    0,  // 0..15
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 16..31
                 0, 1153, 1161, 1159, 1151, 1155, 1152, 1157, 1160,    0,    0,    0,    0, 1162, 1163,    0,  // 32..47
              1129, 1120, 1121, 1122, 1123, 1124, 1125, 1126, 1127, 1128,    0,    0,    0,    0,    0,    0,  // 48..63
                 0, 1030, 1048, 1046, 1032, 1018, 1033, 1034, 1035, 1023, 1036, 1037, 1038, 1050, 1049, 1024,  // 64..79
              1025, 1016, 1019, 1031, 1020, 1022, 1047, 1017, 1045, 1021, 1044,    0,    0,    0,    0,    0,  // 80..95
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 1055, 1078,    0, 1074,    0, 1164,  // 96..111
              1104, 1105, 1106, 1107, 1108, 1109, 1110, 1111, 1112, 1113, 1139, 1140,    0,    0,    0,    0,  // 112..127
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 128..143
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 144..159
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 160..175
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 1039, 1131, 1051, 1130, 1052, 1053,  // 176..191
              1041,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 192..207
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, 1026, 1043, 1027, 1040,    0,  // 208..223
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 223..239
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0}; // 240..255

        static private int[] TCtrlKeys = {
                 0,    0,    0,    0,    0,    0,    0,    0,  127, 1148,    0,    0,    0,   10,    0,    0,  // 0..15
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 16..31
                32, 1132, 1118, 1117, 1119, 1115, 1141, 1116, 1145,    0,    0,    0,    0, 1004, 1006,    0,  // 32..47
                 0,    0, 1003,    0,    0,    0,   30,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 48..63
                 0,    1,    2,    3,    4,    5,    6,    7,    8,    9,   10,   11,   12,   13,   14,   15,  // 64..79
                16,   17,   18,   19,   20,   21,   22,   23,   24,   25,   26,    0,    0,    0,    0,    0,  // 80..95
              1004, 1117, 1145, 1118, 1115, 1143, 1116, 1119, 1141, 1132, 1150, 1144,    0, 1142, 1006, 1149,  // 96..111
              1094, 1095, 1096, 1097, 1098, 1099, 1100, 1101, 1102, 1103, 1137, 1138,    0,    0,    0,    0,  // 112..127
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 128..143
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 144..159
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 160..175
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   31,    0,    0,  // 176..191
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 192..207
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   27,   28,   29,    0,    0,  // 208..223
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 223..239
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0}; // 240..255

        static private int[] TShiftKeys = {
                 0,    0,    0,    0,    0,    0,    0,    0,    8, 1015,    0,    0,    0,   13,    0,    0,  // 0..15
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   27,    0,    0,    0,    0,  // 16..31
                32, 1073, 1081, 1079, 1071, 1075, 1072, 1077, 1080,    0,    0,    0,    0, 1005, 1007,    0,  // 32..47
                41,   33,   64,   35,   36,   37,   94,   38,   42,   40,    0,    0,    0,    0,    0,    0,  // 48..63
                 0,   65,   66,   67,   68,   69,   70,   71,   72,   73,   74,   75,   76,   77,   78,   79,  // 64..79
                80,   81,   82,   83,   84,   85,   86,   87,   88,   89,   90,    0,    0,    0,    0,    0,  // 80..95
                48,   49,   50,   51,   52,   53,   54,   55,   56,   57,   42,   43,    0,   45,   46,   47,  // 96..111
              1084, 1085, 1086, 1087, 1088, 1089, 1090, 1091, 1092, 1093, 1135, 1136,    0,    0,    0,    0,  // 112..127
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 128..143
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 144..159
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 160..175
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,   58,   43,   60,   95,   62,   63,  // 176..191
               126,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 192..207
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  123,  124,  125,   34,    0,  // 208..223
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,  // 223..239
                 0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0}; // 240..255
        #endregion

        static Crt()
        {
            CheckBreak = true;
            CheckEof = false;
            DirectVideo = true;
            LastMode = CO80;
            WindMax = 79 + (24 << 8);
            WindMin = 0;

            // Get the standard input/output handles
            if (OSUtils.IsWindows)
            {
                _StdInputHandle = NativeMethods.GetStdHandle(NativeMethods.STD_INPUT_HANDLE);
                _StdOutputHandle = NativeMethods.GetStdHandle(NativeMethods.STD_OUTPUT_HANDLE);
            }

            // Get the console screen buffer info and eliminate the scroll buffer
            if (OSUtils.IsWindows)
            {
                NativeMethods.CONSOLE_SCREEN_BUFFER_INFO ConsoleScreenBufferInfo = new NativeMethods.CONSOLE_SCREEN_BUFFER_INFO();
                NativeMethods.GetConsoleScreenBufferInfo(_StdOutputHandle, out ConsoleScreenBufferInfo);
                _ScreenSize.X = (short)(ConsoleScreenBufferInfo.srWindow.Right - ConsoleScreenBufferInfo.srWindow.Left + 1);
                _ScreenSize.Y = (short)(ConsoleScreenBufferInfo.srWindow.Bottom - ConsoleScreenBufferInfo.srWindow.Top + 1);
                NativeMethods.SetConsoleScreenBufferSize(_StdOutputHandle, _ScreenSize);
            }
            else
            {
                try
                {
                    _ScreenSize.X = (short)Console.WindowWidth;
                    _ScreenSize.Y = (short)Console.WindowHeight;
                    Console.SetBufferSize(_ScreenSize.X, _ScreenSize.Y);
                }
                catch (NotImplementedException)
                {
                    // Can't do that
                }
            }

            // Find the WindMin/WindMax records
            WindMin = 0;
            WindMax = (_ScreenSize.X - 1) | ((_ScreenSize.Y - 1) << 8);

            // Determine the starting text attribute
            _NormAttr = GetAttrAt(WhereX(), WhereY());

            // Initialize the text attribute
            NormVideo();
        }

        /// <summary>
        /// Associates a text file with the CRT window.
        /// </summary>
        /// <remarks>
        /// AssignCrt works exactly like the Assign standard procedure except that no
        /// file name is specified. Instead, the text file is associated with the CRT.
        /// 
        /// This allows faster output (and input) than would normally be possible using
        /// standard output (or input).
        /// </remarks>
        /// <param name="F">The text file to associate with the CRT window</param>
        static public void AssignCrt(object f)
        {
            lock (_Lock)
            {
                // Not going to implement
            }
        }

        /// <summary>
        /// Controls user termination of an application using the CRT window.
        /// </summary>
        /// <remarks>
        /// When CheckBreak is True, the user can terminate the application at any time
        /// by
        ///  - choosing the Close command on the CRT window's Control menu
        ///  - double-clicking the window's Control-menu box
        ///  - pressing Alt+F4
        ///    pressing Ctrl-Break
        ///
        /// The user can also press Ctrl+C or Ctrl+Break at any time to halt the
        /// application and force the CRT window into its inactive state.
        ///
        /// All of these features are disabled when CheckBreak is False.
        ///
        /// At run time, Crt stores the old Ctrl-Break interrupt vector, $1B, in a
        /// global pointer called SaveInt1B.
        /// </remarks>
        static public bool CheckBreak { get; set; }

        /// <summary>
        /// Controls the end-of-file character checking in the CRT window.
        /// </summary>
        /// <remarks>
        /// When CheckEOF is True, an end-of-file marker is generated when the user
        /// presses Ctrl+Z while reading from a file assigned to the CRT window.
        ///
        /// When CheckEOF is False, pressing Ctrl+Z has no effect.
        ///
        /// CheckEOF is False by default.
        /// </remarks>
        static public bool CheckEof { get; set; }

        /// <summary>
        /// Clears all characters from the cursor position to the start of the line
        /// without moving the cursor.
        /// </summary>
        /// <remarks>
        /// All character positions are set to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the current cursor
        /// position to the left edge becomes the background color.
        ///
        /// ClrBol is window-relative.
        /// </remarks>
        static public void ClrBol()
        {
            lock (_Lock)
            {
                FastWrite(new string(' ', WhereX()), WindMinX + 1, WhereYA(), _TextAttr);
            }
        }

        /// <summary>
        /// Clears the active window from the cursor's current line to the start of the window
        /// </summary>
        /// <remarks>
        /// Sets all character positions from the cursor's current line to the start of the window
        /// to blanks with the currently defined text attributes. Thus, if TextBackground is not
        /// black, the entire screen becomes the background color. This also applies to characters 
        /// cleared by ClrEol, InsLine, and DelLine, and to empty lines created by scrolling.
        ///
        /// ClrBos is window-relative.
        /// </remarks>
        static public void ClrBos()
        {
            lock (_Lock)
            {
                // Clear rows before current row
                ScrollUpWindow(WhereY() - 1);
                ScrollDownWindow(WhereY() - 1);
                // Clear start of current row
                ClrBol();
            }
        }

        /// <summary>
        /// Clears all characters from the cursor position to the end of the line
        /// without moving the cursor.
        /// </summary>
        /// <remarks>
        /// All character positions are set to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the current cursor
        /// position to the right edge becomes the background color.
        ///
        /// ClrEol is window-relative.
        /// </remarks>
        static public void ClrEol()
        {
            lock (_Lock)
            {
                FastWrite(new string(' ', (WindMaxX + 1) - WhereX() + 1), WhereXA(), WhereYA(), _TextAttr);
            }
        }

        /// <summary>
        /// Clears the active window from the cursor's current line to the end of the window
        /// </summary>
        /// <remarks>
        /// Sets all character positions from the cursor's current line to the end of the window
        /// to blanks with the currently defined text attributes. Thus, if TextBackground is not
        /// black, the entire screen becomes the background color. This also applies to characters 
        /// cleared by ClrEol, InsLine, and DelLine, and to empty lines created by scrolling.
        ///
        /// ClrEos is window-relative.
        /// </remarks>
        static public void ClrEos()
        {
            lock (_Lock)
            {
                // Clear rows after current row
                ScrollDownWindow(WindRows - WhereY());
                ScrollUpWindow(WindRows - WhereY());
                // Clear rest of current row
                ClrEol();
            }
        }

        /// <summary>
        /// Clears all characters from the cursor position's current line
        /// without moving the cursor.
        /// </summary>
        /// <remarks>
        /// All character positions are set to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the current cursor
        /// position's line becomes the background color.
        ///
        /// ClrLine is window-relative.
        /// </remarks>
        static public void ClrLine()
        {
            lock (_Lock)
            {
                FastWrite(new string(' ', WindCols), WindMinX + 1, WhereYA(), _TextAttr);
            }
        }

        /// <summary>
        /// Clears the active windows and returns the cursor to the upper-left corner.
        /// </summary>
        /// <remarks>
        /// Sets all character positions to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the entire screen becomes
        /// the background color. This also applies to characters cleared by ClrEol,
        /// InsLine, and DelLine, and to empty lines created by scrolling.
        ///
        /// ClrScr is window-relative.
        /// </remarks>
        static public void ClrScr()
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    ScrollUpWindow(WindRows);
                }
                else
                {
                    try
                    {
                        Console.Clear();
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                    }
                }
                GotoXY(1, 1);
            }
        }

        /// <summary>
        /// Sets the foreground colour to be the same as the background colour
        /// </summary>
        static public void Conceal()
        {
            lock (_Lock)
            {
                TextColor((_TextAttr & 0xF0) >> 4);
            }
        }

        /// <summary>
        /// Delays a specified number of milliseconds.
        /// </summary>
        /// <remarks>
        /// Ms specifies the number of milliseconds to wait.
        ///
        /// Delay is an approximation, so the delay period will not last exactly Ms
        /// milliseconds.
        /// </remarks>
        /// <param name="milliseconds">The number of milliseconds to wait</param>
        static public void Delay(int milliseconds)
        {
            Thread.Sleep(milliseconds);
        }

        static public void DelChar()
        {
            DelChar(1);
        }

        static public void DelChar(int count)
        {
            for (int i = WhereXA(); i <= WindMinX + WindCols - count; i++)
            {
                FastWrite(GetCharAt(i + count, WhereYA()).ToString(), i, WhereYA(), GetAttrAt(i + count, WhereYA()));
            }
            for (int i = WindMinX + WindCols + 1 - count; i <= WindMinX + WindCols; i++)
            {
                FastWrite(" ", i, WhereYA(), _TextAttr);
            }
        }

        /// <summary>
        /// Deletes the line containing the cursor.
        /// </summary>
        /// <remarks>
        /// The line containing the cursor is deleted, and all lines below are moved one
        /// line up (using the BIOS scroll routine). A new line is added at the bottom.
        ///
        /// All character positions are set to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the new line becomes the
        /// background color.
        /// </remarks>
        static public void DelLine()
        {
            lock (_Lock)
            {
                DelLine(1);
            }
        }

        /// <summary>
        /// Deletes the line containing the cursor.
        /// </summary>
        /// <remarks>
        /// The line containing the cursor is deleted, and all lines below are moved one
        /// line up (using the BIOS scroll routine). A new line is added at the bottom.
        ///
        /// All character positions are set to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the new line becomes the
        /// background color.
        /// </remarks>
        /// <param name="count">The number of lines to delete</param>
        static public void DelLine(int count)
        {
            lock (_Lock)
            {
                ScrollUpCustom(WindMinX + 1, WhereYA(), WindMaxX + 1, WindMaxY + 1, count, ' ', _TextAttr);
            }
        }

        /// <summary>
        /// Enables and disables direct memory access for Write and WriteLn statements
        /// that output to the screen
        /// </summary>
        /// <remarks>
        /// When DirectVideo is True, Writes and WriteLns to files associated with the
        /// CRT will store characters directly in video memory, instead of calling the
        /// BIOS to display them.
        ///
        /// When DirectVideo is False, all characters are written through BIOS calls, a
        /// significantly slower process.
        ///
        /// DirectVideo always defaults to True.
        ///
        /// If you want characters displayed through BIOS calls, set DirectVideo to
        /// False at the beginning of your program and after each call to TextMode.
        /// </remarks>
        static public bool DirectVideo { get; set; }

        static public void FastWrite(string text, int column, int row, int foregroundColour, int backgroundColour)
        {
            lock (_Lock)
            {
                FastWrite(text, column, row, foregroundColour | (backgroundColour << 4));
            }
        }

        /// <summary>
        /// Writes a string of text at the desired X/Y coordinate with the given text attribute.
        /// 
        /// FastWrite is not window-relative, and it does not wrap text that goes beyond the right edge of the screen.
        /// </summary>
        /// <param name="text">The text to write</param>
        /// <param name="column">The 1-based column to start the text</param>
        /// <param name="row">The 1-based row to start the text</param>
        /// <param name="attribute">The text attribute to colour the text</param>
        static public void FastWrite(string text, int column, int row, int attribute)
        {
            lock (_Lock)
            {
                if ((column <= _ScreenSize.X) && (row <= _ScreenSize.Y))
                {
                    if (OSUtils.IsWindows)
                    {
                        NativeMethods.CHAR_INFO[] Buffer = new NativeMethods.CHAR_INFO[text.Length];
                        for (int i = 0; i < text.Length; i++)
                        {
                            Buffer[i].Attributes = (ushort)attribute;
                            Buffer[i].AsciiChar = text[i];
                        }

                        NativeMethods.COORD BufferCoord = new NativeMethods.COORD();
                        BufferCoord.X = 0;
                        BufferCoord.Y = 0;

                        NativeMethods.COORD BufferSize = new NativeMethods.COORD();
                        BufferSize.X = (short)text.Length;
                        BufferSize.Y = 1;

                        NativeMethods.SMALL_RECT WriteRegion = new NativeMethods.SMALL_RECT();
                        WriteRegion.Bottom = (short)(row - 1);
                        WriteRegion.Left = (short)(column - 1);
                        WriteRegion.Right = (short)((column - 1) + text.Length);
                        WriteRegion.Top = (short)(row - 1);

                        NativeMethods.WriteConsoleOutput(_StdOutputHandle, Buffer, BufferSize, BufferCoord, ref WriteRegion);
                    }
                    else
                    {
                        try
                        {
                            int OldX = Console.CursorLeft;
                            int OldY = Console.CursorTop;
                            ConsoleColor OldFore = Console.ForegroundColor;
                            ConsoleColor OldBack = Console.BackgroundColor;

                            Console.SetCursorPosition(column - 1, row - 1);
                            //Console.ForegroundColor = 
                            GetConsoleColour(attribute % 16);
                            //Console.BackgroundColor = 
                            GetConsoleColour(attribute / 16);

                            Console.Write(text);
                            // TODO Keep text in buffer for SaveScreen() and RestoreScreen()

                            Console.SetCursorPosition(OldX, OldY);
                            Console.ForegroundColor = OldFore;
                            Console.BackgroundColor = OldBack;
                        }
                        catch (NotImplementedException)
                        {
                            // Can't do that
                        }

                    }
                }
            }
        }

        static public void FillScreen(char ch)
        {
            string Line = new string(ch, ScreenCols);
            for (int Y = 1; Y <= ScreenRows; Y++)
            {
                FastWrite(Line, 1, Y, _TextAttr);
            }
        }

        /// <summary>
        /// Retrieves the text attribute at the given X/Y coordinate.
        /// 
        /// GetAttrAt is not window-relative.
        /// </summary>
        /// <param name="column">The 1-based column to look at</param>
        /// <param name="row">The 1-based row to look at</param>
        /// <returns>The text attribute at the given X/Y coordinate</returns>
        static public int GetAttrAt(int column, int row)
        {
            lock (_Lock)
            {
                if ((column <= _ScreenSize.X) && (row <= _ScreenSize.Y))
                {
                    if (OSUtils.IsWindows)
                    {
                        NativeMethods.COORD ReadCoord = new NativeMethods.COORD();
                        ReadCoord.X = (short)(column - 1);
                        ReadCoord.Y = (short)(row - 1);

                        ushort[] Attribute = new ushort[1];
                        uint NumberOfAttrsRead = 0;
                        NativeMethods.ReadConsoleOutputAttribute(_StdOutputHandle, Attribute, 1, ReadCoord, out NumberOfAttrsRead);
                        return Attribute[0];
                    }
                    else
                    {
                        return LightGray;
                    }
                }
                else
                {
                    return LightGray;
                }
            }
        }

        /// <summary>
        /// Retrieves the character at the given X/Y coordinate.
        /// 
        /// GetCharAt is not window-relative.
        /// </summary>
        /// <param name="column">The 1-based column to look at</param>
        /// <param name="row">The 1-based row to look at</param>
        /// <returns>The character at the given X/Y coordinate</returns>
        static public char GetCharAt(int column, int row)
        {
            lock (_Lock)
            {
                if ((column <= _ScreenSize.X) && (row <= _ScreenSize.Y))
                {
                    if (OSUtils.IsWindows)
                    {
                        NativeMethods.COORD ReadCoord = new NativeMethods.COORD();
                        ReadCoord.X = (short)(column - 1);
                        ReadCoord.Y = (short)(row - 1);

                        StringBuilder Character = new StringBuilder();
                        uint NumberOfCharsRead = 0;
                        NativeMethods.ReadConsoleOutputCharacter(_StdOutputHandle, Character, 1, ReadCoord, out NumberOfCharsRead);
                        return Character[0];
                    }
                    else
                    {
                        return ' ';
                    }
                }
                else
                {
                    return ' ';
                }
            }
        }

        static private ConsoleColor GetConsoleColour(int AColour)
        {
            lock (_Lock)
            {
                switch (AColour)
                {
                    case 0: return ConsoleColor.Black;
                    case 1: return ConsoleColor.DarkBlue;
                    case 2: return ConsoleColor.DarkGreen;
                    case 3: return ConsoleColor.DarkCyan;
                    case 4: return ConsoleColor.DarkRed;
                    case 5: return ConsoleColor.DarkMagenta;
                    case 6: return ConsoleColor.DarkYellow;
                    case 7: return ConsoleColor.Gray;
                    case 8: return ConsoleColor.DarkGray;
                    case 9: return ConsoleColor.Blue;
                    case 10: return ConsoleColor.Green;
                    case 11: return ConsoleColor.Cyan;
                    case 12: return ConsoleColor.Red;
                    case 13: return ConsoleColor.Magenta;
                    case 14: return ConsoleColor.Yellow;
                    case 15: return ConsoleColor.White;
                }

                return ConsoleColor.Black;
            }
        }

        /// <summary>
        /// Moves the cursor to the given coordinates within the virtual screen.
        /// </summary>
        /// <remarks>
        /// The upper-left corner of the virtual screen corresponds to (1, 1).
        /// 
        /// GotoXY is window-relative.
        /// </remarks>
        /// <param name="column">The 1-based column to move to</param>
        /// <param name="row">The 1-based row to move to</param>
        static public void GotoXY(int column, int row)
        {
            lock (_Lock)
            {
                if ((column > 0) && (row > 0))
                {
                    int X = column - 1 + WindMinX;
                    int Y = row - 1 + WindMinY;
                    if ((X <= WindMaxX) && (Y <= WindMaxY))
                    {
                        if (OSUtils.IsWindows)
                        {
                            NativeMethods.COORD CursorPosition = new NativeMethods.COORD();
                            CursorPosition.X = (short)X;
                            CursorPosition.Y = (short)Y;

                            NativeMethods.SetConsoleCursorPosition(_StdOutputHandle, CursorPosition);
                        }
                        else
                        {
                            try
                            {
                                Console.SetCursorPosition(X, Y);
                            }
                            catch (NotImplementedException)
                            {
                                // Can't do that
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Hides the console window (restore it with Show())
        /// </summary>
        static public void HideConsole()
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    IntPtr hWnd = NativeMethods.GetConsoleWindow();
                    if (hWnd != IntPtr.Zero)
                    {
                        NativeMethods.ShowWindow(hWnd, NativeMethods.SW_HIDE);
                    }
                }
            }
        }

        static public void HideCursor()
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.CONSOLE_CURSOR_INFO CCI;
                    if (NativeMethods.GetConsoleCursorInfo(_StdOutputHandle, out CCI))
                    {
                        CCI.Visible = false;
                        NativeMethods.SetConsoleCursorInfo(_StdOutputHandle, ref CCI);
                    }
                }
                else
                {
                    try
                    {
                        Console.CursorVisible = false;
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                    }
                }
            }
        }

        /// <summary>
        /// Selects high-intensity characters.
        /// </summary>
        /// <remarks>
        /// There is a Byte variable in Crt TextAttr that is used to hold the current
        /// video attribute. HighVideo sets the high intensity bit of TextAttr's
        /// fore-ground color, thus mapping colors 0-7 onto colors 8-15.
        /// </remarks>
        static public void HighVideo()
        {
            lock (_Lock)
            {
                _TextAttr |= 0x08;
            }
        }

        static public void InsChar(char ch)
        {
            InsChar(ch, 1);
        }

        static public void InsChar(char ch, int count)
        {
            // First make room for the new char(s)
            for (int i = WindMinX + WindCols; i >= WhereXA() + count; i--)
            {
                FastWrite(GetCharAt(i - count, WhereYA()).ToString(), i, WhereYA(), GetAttrAt(i - count, WhereYA()));
            }
            // Then write the new char(s)
            for (int i = WhereXA(); i < WhereXA() + count; i++)
            {
                FastWrite(ch.ToString(), i, WhereYA(), _TextAttr);
            }
        }

        /// <summary>
        /// Inserts an empty line at the cursor position.
        /// </summary>
        /// <remarks>
        /// All lines below the inserted line are moved down one line, and the bottom
        /// line scrolls off the screen (using the BIOS scroll routine).
        ///
        /// All character positions are set to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the new line becomes the
        /// background color.
        /// 
        /// InsLine is window-relative.
        /// </remarks>
        static public void InsLine()
        {
            lock (_Lock)
            {
                InsLine(1);
            }
        }

        /// <summary>
        /// Inserts an empty line at the cursor position.
        /// </summary>
        /// <remarks>
        /// All lines below the inserted line are moved down one line, and the bottom
        /// line scrolls off the screen (using the BIOS scroll routine).
        ///
        /// All character positions are set to blanks with the currently defined text
        /// attributes. Thus, if TextBackground is not black, the new line becomes the
        /// background color.
        /// 
        /// InsLine is window-relative.
        /// </remarks>
        /// <param name="count">The number of lines to insert</param>
        static public void InsLine(int count)
        {
            lock (_Lock)
            {
                ScrollDownCustom(WindMinX + 1, WhereYA(), WindMaxX + 1, WindMaxY + 1, count, ' ', _TextAttr);
            }
        }

        /// <summary>
        /// Determines if a key has been pressed on the keyboard.
        /// </summary>
        /// The key can be read using the ReadKey function.
        /// <remarks>
        /// </remarks>
        /// <returns>True If key has been pressed False If key has not been pressed</returns>
        static public bool KeyPressed()
        {
            lock (_Lock)
            {
                // Check if we have a key in the buffer already
                if (_KeyBuf.Count > 0) return true;

                uint NumberOfEvents = 0;
                do
                {
                    if (OSUtils.IsWindows)
                    {
                        NativeMethods.GetNumberOfConsoleInputEvents(_StdInputHandle, out NumberOfEvents);
                        if (NumberOfEvents > 0)
                        {
                            NativeMethods.INPUT_RECORD[] Buffer = new NativeMethods.INPUT_RECORD[1];
                            uint NumberOfEventsRead = 0;
                            if (NativeMethods.ReadConsoleInput(_StdInputHandle, Buffer, 1, out NumberOfEventsRead))
                            {
                                if ((Buffer[0].EventType == NativeMethods.KEY_EVENT) && (Buffer[0].KeyEvent.bKeyDown))
                                {
                                    bool Alt = (NativeMethods.GetKeyState(NativeMethods.VirtualKeyStates.VK_MENU) & 0x8000) == 0x8000;
                                    bool Ctrl = (NativeMethods.GetKeyState(NativeMethods.VirtualKeyStates.VK_CONTROL) & 0x8000) == 0x8000;
                                    bool Shift = (NativeMethods.GetKeyState(NativeMethods.VirtualKeyStates.VK_SHIFT) & 0x8000) == 0x8000;

                                    if ((Buffer[0].KeyEvent.wVirtualKeyCode >= 65) && (Buffer[0].KeyEvent.wVirtualKeyCode <= 90))
                                    {
                                        bool CapsLock = (NativeMethods.GetKeyState(NativeMethods.VirtualKeyStates.VK_CAPITAL) & 0x0001) == 0x0001;
                                        Shift ^= CapsLock;
                                    }

                                    if ((Buffer[0].KeyEvent.wVirtualKeyCode >= 96) && (Buffer[0].KeyEvent.wVirtualKeyCode <= 105))
                                    {
                                        bool NumLock = (NativeMethods.GetKeyState(NativeMethods.VirtualKeyStates.VK_NUMLOCK) & 0x0001) == 0x0001;
                                        Shift |= NumLock;
                                    }

                                    int Key = 0;
                                    if (Alt)
                                    {
                                        Key = TAltKeys[Buffer[0].KeyEvent.wVirtualKeyCode];
                                    }
                                    else if (Ctrl)
                                    {
                                        Key = TCtrlKeys[Buffer[0].KeyEvent.wVirtualKeyCode];
                                    }
                                    else if (Shift)
                                    {
                                        Key = TShiftKeys[Buffer[0].KeyEvent.wVirtualKeyCode];
                                    }
                                    else
                                    {
                                        Key = TKeys[Buffer[0].KeyEvent.wVirtualKeyCode];
                                    }

                                    if (Key >= 1000)
                                    {
                                        _KeyBuf.Enqueue((char)0);
                                        _KeyBuf.Enqueue((char)(Key - 1000));
                                    }
                                    else if (Key > 0)
                                    {
                                        _KeyBuf.Enqueue((char)Key);
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        try
                        {
                            NumberOfEvents = (uint)(Console.KeyAvailable ? 1 : 0);
                            if (NumberOfEvents > 0)
                            {
                                ConsoleKeyInfo CKI = Console.ReadKey(true);
                                bool Alt = (CKI.Modifiers & ConsoleModifiers.Alt) == ConsoleModifiers.Alt;
                                bool Ctrl = (CKI.Modifiers & ConsoleModifiers.Control) == ConsoleModifiers.Control;
                                bool Shift = (CKI.Modifiers & ConsoleModifiers.Shift) == ConsoleModifiers.Shift;

                                if (((int)CKI.Key >= 65) && ((int)CKI.Key <= 90))
                                {
                                    try
                                    {
                                        Shift ^= Console.CapsLock;
                                    }
                                    catch (NotSupportedException)
                                    {
                                        // Looks like mono doesn't support this
                                    }
                                }

                                if (((int)CKI.Key >= 96) && ((int)CKI.Key <= 105))
                                {
                                    Shift |= Console.NumberLock;
                                }

                                int Key = 0;
                                if (Alt)
                                {
                                    Key = TAltKeys[(int)CKI.Key];
                                }
                                else if (Ctrl)
                                {
                                    Key = TCtrlKeys[(int)CKI.Key];
                                }
                                else if (Shift)
                                {
                                    Key = TShiftKeys[(int)CKI.Key];
                                }
                                else
                                {
                                    Key = TKeys[(int)CKI.Key];
                                }

                                if (Key >= 1000)
                                {
                                    _KeyBuf.Enqueue((char)0);
                                    _KeyBuf.Enqueue((char)(Key - 1000));
                                }
                                else if (Key > 0)
                                {
                                    _KeyBuf.Enqueue((char)Key);
                                }
                            }
                        }
                        catch (NotImplementedException)
                        {
                            // Can't do that
                            NumberOfEvents = 0;
                        }
                    }
                } while (NumberOfEvents > 0);

                return false;
            }
        }

        /// <summary>
        /// Each time TextMode is called, the current video mode is stored in LastMode.
        /// </summary>
        /// <remarks>
        /// Also, LastMode is initialized at program startup to the then-active video mode.
        /// </remarks>
        static public int LastMode { get; set; }

        /// <summary>
        /// Selects low intensity characters.
        /// </summary>
        /// <remarks>
        /// There is a Byte variable in Crt--TextAttr--that holds the current video
        /// attribute. LowVideo clears the high-intensity bit of TextAttr's foreground
        /// color, thus mapping colors 8 to 15 onto colors 0 to 7.
        /// </remarks>
        static public void LowVideo()
        {
            lock (_Lock)
            {
                _TextAttr &= 0xF7;
            }
        }

        /// <summary>
        /// Selects the original text attribute read from the cursor location at startup.
        /// </summary>
        /// <remarks>
        /// There is a Byte variable in Crt--TextAttr--that holds the current video
        /// attribute. NormVideo restores TextAttr to the value it had when the program
        /// was started.
        /// </remarks>
        static public void NormVideo()
        {
            lock (_Lock)
            {
                TextAttr = _NormAttr;
            }
        }

        /// <summary>
        /// Turns off the computer's internal speaker.
        /// </summary>
        static public void NoSound()
        {
            lock (_Lock)
            {
                // Not going to implement
            }
        }

        /// <summary>
        /// Reads a character from the keyboard.
        /// </summary>
        /// <remarks>
        /// The character is not echoed to the screen.
        /// </remarks>
        /// <returns>Returns a character or an extended scan code.</returns>
        static public char ReadKey()
        {
            lock (_Lock)
            {
                while (!KeyPressed()) Delay(1);
                return _KeyBuf.Dequeue();
            }
        }

        /// <summary>
        /// Reads a line of text from the keyboard.
        /// </summary>
        /// <returns>Returns a string of text</returns>
        static public void ReadLn(out string text)
        {
            lock (_Lock)
            {
                // TODO This needs a lot of work

                text = "";

                char Ch = ReadKey();
                while (Ch != '\r')
                {
                    if (Ch == '\b')
                    {
                        if (text.Length > 0)
                        {
                            Write("\b \b");
                            text = text.Substring(0, text.Length - 1);
                        }
                    }
                    else if (((byte)Ch >= 32) && ((byte)Ch <= 126))
                    {
                        Write(Ch.ToString());
                        text += Ch;
                    }

                    Ch = ReadKey();
                }
                WriteLn();
            }
        }

        static public void RestoreScreen(NativeMethods.CHAR_INFO[] buffer, int left, int top, int right, int bottom)
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.COORD BufferCoord = new NativeMethods.COORD();
                    BufferCoord.X = 0;
                    BufferCoord.Y = 0;

                    NativeMethods.COORD BufferSize = new NativeMethods.COORD();
                    BufferSize.X = (short)(right - left + 1);
                    BufferSize.Y = (short)(bottom - top + 1);

                    NativeMethods.SMALL_RECT WriteRegion = new NativeMethods.SMALL_RECT();
                    WriteRegion.Left = (short)(left - 1);
                    WriteRegion.Top = (short)(top - 1);
                    WriteRegion.Right = (short)(right - 1);
                    WriteRegion.Bottom = (short)(bottom - 1);

                    NativeMethods.WriteConsoleOutput(_StdOutputHandle, buffer, BufferSize, BufferCoord, ref WriteRegion);
                }
                else
                {
                    if (buffer != null)
                    {
                        // TODO Restore from buffer
                    }
                }
            }
        }

        /// <summary>
        /// Reverses the foreground and background text attributes
        /// </summary>
        static public void ReverseVideo()
        {
            lock (_Lock)
            {
                TextAttr = ((_TextAttr & 0xF0) >> 4) | ((_TextAttr & 0x0F) << 4);
            }
        }

        static public NativeMethods.CHAR_INFO[] SaveScreen(int left, int top, int right, int bottom)
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.COORD BufferSize = new NativeMethods.COORD();
                    BufferSize.X = (short)(right - left + 1);
                    BufferSize.Y = (short)(bottom - top + 1);

                    NativeMethods.CHAR_INFO[] Buffer = new NativeMethods.CHAR_INFO[BufferSize.X * BufferSize.Y];

                    NativeMethods.COORD BufferCoord = new NativeMethods.COORD();
                    BufferCoord.X = 0;
                    BufferCoord.Y = 0;

                    NativeMethods.SMALL_RECT ReadRegion = new NativeMethods.SMALL_RECT();
                    ReadRegion.Left = (short)(left - 1);
                    ReadRegion.Top = (short)(top - 1);
                    ReadRegion.Right = (short)(right - 1);
                    ReadRegion.Bottom = (short)(bottom - 1);

                    NativeMethods.ReadConsoleOutput(_StdOutputHandle, Buffer, BufferSize, BufferCoord, ref ReadRegion);
                    return Buffer;
                }
                else
                {
                    // TODO Save from buffer
                    return null;
                }
            }
        }

        static public int ScreenCols
        {
            get { return _ScreenSize.X; }
        }

        static public int ScreenRows
        {
            get { return _ScreenSize.Y; }
        }

        /// <summary>
        /// Scrolls the given window down the given number of lines (leaving blank lines at the top), filling the void with the given character with the given text attribute
        /// </summary>
        /// <param name="AX1">The 0-based left column of the window</param>
        /// <param name="AY1">The 0-based top row of the window</param>
        /// <param name="AX2">The 0-based right column of the window</param>
        /// <param name="AY2">The 0-based bottom row of the window</param>
        /// <param name="ALines">The number of lines to scroll</param>
        /// <param name="ACh">The character to fill the void with</param>
        /// <param name="AAttr">The text attribute to fill the void with</param>
        static private void ScrollDownCustom(int AX1, int AY1, int AX2, int AY2, int ALines, char ACh, int AAttr)
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.COORD DestinationOrigin = new NativeMethods.COORD();
                    DestinationOrigin.X = (short)AX1;
                    DestinationOrigin.Y = (short)(AY1 + ALines);

                    NativeMethods.CHAR_INFO Fill = new NativeMethods.CHAR_INFO();
                    Fill.AsciiChar = ACh;
                    Fill.Attributes = (ushort)AAttr;

                    NativeMethods.SMALL_RECT ScrollRectangle = new NativeMethods.SMALL_RECT();
                    ScrollRectangle.Bottom = (short)(AY2 - ALines);
                    ScrollRectangle.Left = (short)AX1;
                    ScrollRectangle.Right = (short)AX2;
                    ScrollRectangle.Top = (short)AY1;

                    NativeMethods.ScrollConsoleScreenBuffer(_StdOutputHandle, ref ScrollRectangle, IntPtr.Zero, DestinationOrigin, ref Fill);
                }
                else
                {
                    try
                    {
                        Console.MoveBufferArea(AX1, AY1, AX2, AY2 - ALines, AX1, AY1 + ALines, ' ', GetConsoleColour(_TextAttr % 16), GetConsoleColour(_TextAttr / 16));
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                    }
                }
            }
        }

        /// <summary>
        /// Scrolls the screen down the given number of lines (leaving blanks at the top)
        /// </summary>
        /// <param name="count">The number of lines to scroll</param>
        static public void ScrollDownScreen(int count)
        {
            lock (_Lock)
            {
                ScrollDownCustom(0, 0, _ScreenSize.X - 1, _ScreenSize.Y - 1, count, ' ', _TextAttr);
            }
        }

        /// <summary>
        /// Scrolls the current window down the given number of lines (leaving blanks at the top)
        /// </summary>
        /// <param name="count">The number of lines to scroll</param>
        static public void ScrollDownWindow(int count)
        {
            lock (_Lock)
            {
                ScrollDownCustom(WindMinX, WindMinY, WindMaxX, WindMaxY, count, ' ', _TextAttr);
            }
        }

        /// <summary>
        /// Scrolls the given window up the given number of lines (leaving blank lines at the bottom), filling the void with the given character with the given text attribute
        /// </summary>
        /// <param name="AX1">The 0-based left column of the window</param>
        /// <param name="AY1">The 0-based top row of the window</param>
        /// <param name="AX2">The 0-based right column of the window</param>
        /// <param name="AY2">The 0-based bottom row of the window</param>
        /// <param name="ALines">The number of lines to scroll</param>
        /// <param name="ACh">The character to fill the void with</param>
        /// <param name="AAttr">The text attribute to fill the void with</param>
        static private void ScrollUpCustom(int AX1, int AY1, int AX2, int AY2, int ALines, char ACh, int AAttr)
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.SMALL_RECT ClipRectangle = new NativeMethods.SMALL_RECT();
                    ClipRectangle.Bottom = (short)AY2;
                    ClipRectangle.Left = (short)AX1;
                    ClipRectangle.Right = (short)AX2;
                    ClipRectangle.Top = (short)AY1;

                    NativeMethods.CHAR_INFO Fill = new NativeMethods.CHAR_INFO();
                    Fill.AsciiChar = ACh;
                    Fill.Attributes = (ushort)AAttr;

                    NativeMethods.SMALL_RECT ScrollRectangle = ClipRectangle;

                    NativeMethods.COORD DestinationOrigin = new NativeMethods.COORD();
                    DestinationOrigin.X = (short)AX1;
                    DestinationOrigin.Y = (short)(AY1 - ALines);
                    NativeMethods.ScrollConsoleScreenBuffer(_StdOutputHandle, ref ScrollRectangle, ref ClipRectangle, DestinationOrigin, ref Fill);
                }
                else
                {
                    try
                    {
                        Console.MoveBufferArea(AX1, AY1, AX2, AY2, AX1, AY1 - ALines, ' ', GetConsoleColour(_TextAttr % 16), GetConsoleColour(_TextAttr / 16));
                    }
                    catch (NotImplementedException)
                    {
                        try
                        {
                            int OldX = Console.CursorLeft;
                            int OldY = Console.CursorTop;

                            Console.SetCursorPosition(0, _ScreenSize.Y - 1);
                            for (int i = 0; i < ALines; i++) Console.WriteLine();

                            Console.SetCursorPosition(OldX, OldY);
                        }
                        catch (NotImplementedException)
                        {
                            // Can't do that
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Scrolls the screen up the given number of lines (leaving blanks at the bottom)
        /// </summary>
        /// <param name="count">The number of lines to scroll</param>
        static public void ScrollUpScreen(int count)
        {
            lock (_Lock)
            {
                ScrollUpCustom(0, 0, _ScreenSize.X - 1, _ScreenSize.Y - 1, count, ' ', _TextAttr);
            }
        }

        /// <summary>
        /// Scrolls the current window up the given number of lines (leaving blanks at the bottom)
        /// </summary>
        /// <param name="count">The number of lines to scroll</param>
        static public void ScrollUpWindow(int count)
        {
            lock (_Lock)
            {
                ScrollUpCustom(WindMinX, WindMinY, WindMaxX, WindMaxY, count, ' ', _TextAttr);
            }
        }

        public static void SetBlinkRate(int milliseconds)
        {
            lock (_Lock)
            {
                // TODO
            }
        }

        public static void SetIcon(IntPtr iconHandle)
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    try
                    {
                        NativeMethods.SetConsoleIcon(iconHandle);
                    }
                    catch (NotSupportedException)
                    {
                        // Ignore
                    }
                }
            }
        }

        static public void SetTitle(string text)
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.SetConsoleTitle(text);
                }
                else
                {
                    try
                    {
                        Console.Title = text;
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                    }
                }
            }
        }

        static public void SetWindowSize(int columns, int rows)
        {
            lock (_Lock)
            {
                // Set the new console screen buffer size
                _ScreenSize.X = (short)columns;
                _ScreenSize.Y = (short)rows;
                if (OSUtils.IsWindows)
                {
                    NativeMethods.SetConsoleScreenBufferSize(_StdOutputHandle, _ScreenSize);
                }
                else
                {
                    try
                    {
                        Console.SetBufferSize(_ScreenSize.X, _ScreenSize.Y);
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                    }
                }

                // And make the actual window size match the new buffer size
                NativeMethods.SMALL_RECT ConsoleWindow = new NativeMethods.SMALL_RECT();
                ConsoleWindow.Left = 0;
                ConsoleWindow.Top = 0;
                ConsoleWindow.Right = (short)(columns - 1);
                ConsoleWindow.Bottom = (short)(rows - 1);
                if (OSUtils.IsWindows)
                {
                    NativeMethods.SetConsoleWindowInfo(_StdOutputHandle, true, ref ConsoleWindow);
                }
                else
                {
                    try
                    {
                        Console.SetWindowSize(_ScreenSize.X, _ScreenSize.Y);
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                    }

                }

                // Update the WindMin/WindMax records
                WindMin = 0;
                WindMax = (_ScreenSize.X - 1) | ((_ScreenSize.Y - 1) << 8);
            }
        }

        /// <summary>
        /// Shows the console window (useful after calling Hide())
        /// </summary>
        static public void ShowConsole()
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    IntPtr hWnd = NativeMethods.GetConsoleWindow();
                    if (hWnd != IntPtr.Zero)
                    {
                        bool b = NativeMethods.ShowWindow(hWnd, NativeMethods.SW_SHOWNORMAL);
                    }
                }
            }
        }

        static public void ShowCursor()
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.CONSOLE_CURSOR_INFO CCI;
                    if (NativeMethods.GetConsoleCursorInfo(_StdOutputHandle, out CCI))
                    {
                        CCI.Visible = true;
                        NativeMethods.SetConsoleCursorInfo(_StdOutputHandle, ref CCI);
                    }
                }
                else
                {
                    try
                    {
                        Console.CursorVisible = true;
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                    }
                }
            }
        }

        /// <summary>
        /// Starts the internal speaker.
        /// </summary>
        /// <remarks>
        /// Hz specifies the frequency of the emitted sound in hertz. The speaker
        /// continues until explicitly turned off by a call to NoSound.
        /// </remarks>
        /// <param name="frequency">The frequency of the emitted sound in hertz</param>
        static public void Sound(int frequency)
        {
            lock (_Lock)
            {
                // Not going to implement
            }
        }

        /// <summary>
        /// Stores currently selected text attributes
        /// </summary>
        /// <remarks>
        /// The text attributes are normally set through calls to TextColor and
        /// TextBackground.
        ///
        /// However, you can also set them by directly storing a value in TextAttr.
        /// </remarks>
        static public int TextAttr
        {
            get { return _TextAttr; }
            set
            {
                lock (_Lock)
                {
                    _TextAttr = value;
                    if (!OSUtils.IsWindows)
                    {
                        try
                        {
                            Console.ForegroundColor = GetConsoleColour(_TextAttr % 16);
                            Console.BackgroundColor = GetConsoleColour(_TextAttr / 16);
                        }
                        catch (NotImplementedException)
                        {
                            // Can't do that
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Selects the background color.
        /// </summary>
        /// <remarks>
        /// Color is an integer expression in the range 0..7, corresponding to one of
        /// the first eight text color constants. There is a byte variable in
        /// Crt--TextAttr--that is used to hold the current video attribute.
        /// TextBackground sets bits 4-6 of TextAttr to Color.
        ///
        /// The background of all characters subsequently written will be in the
        /// specified color.
        /// </remarks>
        /// <param name="colour">The colour to set the background to</param>
        static public void TextBackground(int colour)
        {
            lock (_Lock)
            {
                TextAttr = (_TextAttr & 0x0F) | ((colour & 0x0F) << 4);
            }
        }

        /// <summary>
        /// Selects the foreground character color.
        /// </summary>
        /// <remarks>
        /// Color is an integer expression in the range 0..15, corresponding to one of
        /// the text color constants defined in Crt.
        ///
        /// There is a byte-type variable Crt--TextAttr--that is used to hold the
        /// current video attribute. TextColor sets bits 0-3 to Color. If Color is
        /// greater than 15, the blink bit (bit 7) is also set; otherwise, it is
        /// cleared.
        ///
        /// You can make characters blink by adding 128 to the color value. The Blink
        /// constant is defined for that purpose; in fact, for compatibility with Turbo
        /// Pascal 3.0, any Color value above 15 causes the characters to blink. The
        /// foreground of all characters subsequently written will be in the specified
        /// color.
        /// </remarks>
        /// <param name="colour">The colour to set the foreground to</param>
        static public void TextColor(int colour)
        {
            lock (_Lock)
            {
                TextAttr = (_TextAttr & 0xF0) | (colour & 0x0F);
            }
        }

        /// <summary>
        /// Selects a specific text mode.
        /// </summary>
        /// <remarks>
        /// When TextMode is called, the current window is reset to the entire screen,
        /// DirectVideo is set to True, CheckSnow is set to True if a color mode was
        /// selected, the current text attribute is reset to normal corresponding to a
        /// call to NormVideo, and the current video is stored in LastMode. In addition,
        /// LastMode is initialized at program startup to the then-active video mode.
        /// </remarks>
        /// <param name="mode"></param>
        static public void TextMode(int mode)
        {
            lock (_Lock)
            {
                LastMode = _TextMode;
                _TextMode = mode;
            }
        }

        /// <summary>
        /// Returns the CP's X coordinate of the current cursor location.
        /// </summary>
        /// <remarks>
        /// WhereX is window-specific.
        /// </remarks>
        /// <returns>The 1-based column of the window the cursor is currently in</returns>
        static public int WhereX()
        {
            lock (_Lock)
            {
                return WhereXA() - WindMinX;
            }
        }

        /// <summary>
        /// Returns the CP's X coordinate of the current cursor location.
        /// </summary>
        /// <remarks>
        /// WhereXA is not window-specific.
        /// </remarks>
        /// <returns>The 1-based column of the screen the cursor is currently in</returns>
        static public int WhereXA()
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.CONSOLE_SCREEN_BUFFER_INFO CSBI = new NativeMethods.CONSOLE_SCREEN_BUFFER_INFO();
                    NativeMethods.GetConsoleScreenBufferInfo(_StdOutputHandle, out CSBI);
                    return CSBI.dwCursorPosition.X + 1;
                }
                else
                {
                    try
                    {
                        return Console.CursorLeft + 1;
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                        return 1;
                    }
                }
            }
        }

        /// <summary>
        /// Returns the CP's Y coordinate of the current cursor location.
        /// </summary>
        /// <remarks>
        /// WhereY is window-specific.
        /// </remarks>
        /// <returns>The 1-based row of the window the cursor is currently in</returns>
        static public int WhereY()
        {
            lock (_Lock)
            {
                return WhereYA() - WindMinY;
            }
        }

        /// <summary>
        /// Returns the CP's Y coordinate of the current cursor location.
        /// </summary>
        /// <remarks>
        /// WhereYA is now window-specific.
        /// </remarks>
        /// <returns>The 1-based row of the screen the cursor is currently in</returns>
        static public int WhereYA()
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.CONSOLE_SCREEN_BUFFER_INFO CSBI = new NativeMethods.CONSOLE_SCREEN_BUFFER_INFO();
                    NativeMethods.GetConsoleScreenBufferInfo(_StdOutputHandle, out CSBI);
                    return CSBI.dwCursorPosition.Y + 1;
                }
                else
                {
                    try
                    {
                        return Console.CursorTop + 1;
                    }
                    catch (NotImplementedException)
                    {
                        // Can't do that
                        return 1;
                    }
                }
            }
        }

        /// <summary>
        /// The number of columns found in the currently defined window
        /// </summary>
        static public int WindCols
        {
            get
            {
                lock (_Lock)
                {
                    return WindMaxX - WindMinX + 1;
                }
            }
        }

        /// <summary>
        /// The 0-based lower right coordinate of the current window
        /// </summary>
        static public int WindMax { get; set; }

        /// <summary>
        /// The 0-based left column of the current window
        /// </summary>
        static public int WindMaxX
        {
            get
            {
                lock (_Lock)
                {
                    return (WindMax & 0x00FF);
                }
            }
        }

        /// <summary>
        /// The 0-based right column of the current window
        /// </summary>
        static public int WindMaxY
        {
            get
            {
                lock (_Lock)
                {
                    return ((WindMax & 0xFF00) >> 8);
                }
            }
        }

        /// <summary>
        /// The 0-based upper left coordinate of the current window
        /// </summary>
        static public int WindMin { get; set; }

        /// <summary>
        /// The 0-based top row of the current window
        /// </summary>
        static public int WindMinX
        {
            get
            {
                lock (_Lock)
                {
                    return (WindMin & 0x00FF);
                }
            }
        }

        /// <summary>
        /// The 0-based bottom row of the current window
        /// </summary>
        static public int WindMinY
        {
            get
            {
                lock (_Lock)
                {
                    return ((WindMin & 0xFF00) >> 8);
                }
            }
        }

        /// <summary>
        /// Defines a text window on the screen.
        /// </summary>
        /// <remarks>
        /// X1 and Y1 are the coordinates of the upper left corner of the window, and X2
        /// and Y2 are the coordinates of the lower right corner. The upper left corner
        /// of the screen corresponds to (1, 1). The minimum size of a text window is
        /// one column by one line. If the coordinates are invalid in any way, the call
        /// to Window is ignored.
        ///
        /// The default window is (1, 1, 80, 25) in 25-line mode, and (1, 1, 80, 43) in
        /// 43-line mode, corresponding to the entire screen.
        ///
        /// All screen coordinates (except the window coordinates themselves) are
        /// relative to the current window. For instance, GotoXY(1, 1) will always
        /// position the cursor in the upper left corner of the current window.
        ///
        /// Many Crt procedures and functions are window-relative, including ClrEol,
        /// ClrScr, DelLine, GotoXY, InsLine, WhereX, WhereY, Read, Readln, Write,
        /// Writeln.
        ///
        /// WindMin and WindMax store the current window definition. A call to the
        /// Window procedure always moves the cursor to (1, 1).
        /// </remarks>
        /// <param name="left">The 1-based left column of the window</param>
        /// <param name="top">The 1-based top row of the window</param>
        /// <param name="right">The 1-based right column of the window</param>
        /// <param name="bottom">The 1-based bottom row of the window</param>
        static public void Window(int left, int top, int right, int bottom)
        {
            lock (_Lock)
            {
                if ((left > 0) && (top > 0) && (left <= right) && (top <= bottom))
                {
                    left--;
                    top--;
                    right--;
                    bottom--;
                    if ((right < _ScreenSize.X) && (bottom < _ScreenSize.Y))
                    {
                        WindMin = left + (top << 8);
                        WindMax = right + (bottom << 8);
                        GotoXY(1, 1);
                    }
                }
            }
        }

        /// <summary>
        /// The number of rows found in the currently defined window
        /// </summary>
        static public int WindRows
        {
            get
            {
                lock (_Lock)
                {
                    return WindMaxY - WindMinY + 1;
                }
            }
        }

        /// <summary>
        /// Writes a given line of text to the screen.
        /// </summary>
        /// <remarks>
        /// Text is wrapped if it exceeds the right edge of the window
        /// </remarks>
        /// <param name="text">The text to print to the screen</param>
        static public void Write(string text)
        {
            lock (_Lock)
            {
                int X = WhereX();
                int Y = WhereY();
                StringBuilder Buf = new StringBuilder();

                for (int i = 0; i < text.Length; i++)
                {
                    bool DoGoto = false;

                    if (text[i] == '\b')
                    {
                        // Backspace, need to flush buffer before moving cursor
                        FastWrite(Buf.ToString(), WhereXA(), WhereYA(), _TextAttr);
                        X += Buf.Length;
                        if (X > 1) X -= 1;
                        DoGoto = true;

                        Buf = new StringBuilder();
                    }
                    else if (text[i] == '\n')
                    {
                        // Line feed, need to flush buffer before moving cursor
                        FastWrite(Buf.ToString(), WhereXA(), WhereYA(), _TextAttr);
                        X += Buf.Length;
                        Y += 1;
                        DoGoto = true;

                        Buf = new StringBuilder();
                    }
                    else if (text[i] == '\r')
                    {
                        // Carriage return, need to flush buffer before moving cursor
                        FastWrite(Buf.ToString(), WhereXA(), WhereYA(), _TextAttr);
                        X = 1;
                        DoGoto = true;

                        Buf = new StringBuilder();
                    }
                    else
                    {
                        // Append character to buffer
                        Buf.Append(text[i]);

                        // Check if we've passed the right edge of the window
                        if ((X + Buf.Length) > WindCols)
                        {
                            // We have, need to flush buffer before moving cursor
                            FastWrite(Buf.ToString(), WhereXA(), WhereYA(), _TextAttr);
                            Buf = new StringBuilder();

                            X = 1;
                            Y += 1;
                            DoGoto = true;
                        }
                    }

                    // Check if we've passed the bottom edge of the window
                    if (Y > WindRows)
                    {
                        // We have, need to scroll the window one line
                        Y = WindRows;
                        ScrollUpWindow(1);
                        DoGoto = true;
                    }

                    if (DoGoto) GotoXY(X, Y);
                }

                // Flush remaining text in buffer if we have any
                if (Buf.Length > 0)
                {
                    FastWrite(Buf.ToString(), WhereXA(), WhereYA(), _TextAttr);
                    X += Buf.Length;
                    GotoXY(X, Y);
                }
            }
        }

        /// <summary>
        /// Writes a carriage return and line feed.
        /// </summary>
        static public void WriteLn()
        {
            lock (_Lock)
            {
                Write("\r\n");
            }
        }

        /// <summary>
        /// Writes a given line of text to the screen, followed by a carriage return and line feed.
        /// </summary>
        /// <remarks>
        /// Text is wrapped if it exceeds the right edge of the window
        /// </remarks>
        /// <param name="text">The text to print to the screen</param>
        static public void WriteLn(string text)
        {
            lock (_Lock)
            {
                Write(text + "\r\n");
            }
        }

        /// <summary>
        /// Try to set the console font size to characters with the given X and Y size
        /// </summary>
        /// <param name="width">The horizontal size</param>
        /// <param name="height">The vertical size</param>
        /// <returns>True if the size was found and set, False if the size was not available</returns>
        public static bool SetFontSize(int width, int height)
        {
            lock (_Lock)
            {
                if (OSUtils.IsWindows)
                {
                    try
                    {
                        NativeMethods.CONSOLE_FONT[] fonts = new NativeMethods.CONSOLE_FONT[NativeMethods.GetNumberOfConsoleFonts()];
                        if (fonts.Length > 0) NativeMethods.GetConsoleFontInfo(_StdOutputHandle, false, (uint)fonts.Length, fonts);

                        for (int i = 0; i < fonts.Length; i++)
                        {
                            fonts[i].dim = NativeMethods.GetConsoleFontSize(_StdOutputHandle, fonts[i].index);
                            if ((fonts[i].dim.X == width) && (fonts[i].dim.Y == height))
                            {
                                NativeMethods.SetConsoleFont(_StdOutputHandle, fonts[i].index);
                                return true;
                            }
                        }
                    }
                    catch (EntryPointNotFoundException)
                    {
                        // Not supported on this version of windows
                    }
                }

                return false;
            }
        }
    }

    static public class CharacterMask
    {
        public const string All = "`1234567890-=\\qwertyuiop[]asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+|QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>? ";
        public const string Alpha = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM";
        public const string AlphaWithSpace = Alpha + " ";
        public const string Numeric = "1234567890";
        public const string NumericWithSpace = Numeric + " ";
        public const string Alphanumeric = Alpha + Numeric;
        public const string AlphanumericWithSpace = Alphanumeric + " ";
        public const string FileName = "1234567890-=\\qwertyuiop[]asdfghjkl;'zxcvbnm,.~!@#$%^&*()_+QWERTYUIOP{}ASDFGHJKL:ZXCVBNM ";
        public const string FileNameWithWildcard = "1234567890-=\\qwertyuiop[]asdfghjkl;'zxcvbnm,.~!@#$%^&()_+QWERTYUIOP{}ASDFGHJKL:ZXCVBNM? ";
    }
}
