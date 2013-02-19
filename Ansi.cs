/*
  RMLib: Nonvisual support classes used by multiple R&M Software programs
  Copyright (C) 2008-2013  Rick Parrish, R&M Software

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
using System.Collections.Generic;
using System.Globalization;

namespace RandM.RMLib
{
    /// <summary>
    /// The Ansi class is used for parsing and displaying ANSI formatted text to the
    /// console window.  Relies on the Crt unit for display.
    /// </summary>
    static public class Ansi
    {
        static private int _AnsiAttr = 7;
        static private string _AnsiBuffer = "0";
        static private Queue<int> _AnsiParams = new Queue<int>();
        static private AnsiParserState _AnsiParserState = AnsiParserState.None;
        static private object _Lock = new object();
        static private int _SavedX = 0;
        static private int _SavedY = 0;

        static public event EventHandler ESC5nEvent = null;
        static public event EventHandler ESC6nEvent = null;
        static public event EventHandler ESC255nEvent = null;
        static public event EventHandler ESCQEvent = null;

        static private int[] ANSI_COLORS = { 0, 4, 2, 6, 1, 5, 3, 7 };
        static private char ESC = '\x1B';

        static private void AnsiCommand(char ACommand)
        {
            lock (_Lock)
            {
                int Colour = 0;
                int X = 0;
                int Y = 0;
                int Z = 0;

                switch (ACommand)
                {
                    case 'A': // CSI n A - Moves the cursor n (default 1) cells up. If the cursor is already at the edge of the screen, this has no effect.
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Y = Math.Max(1, Crt.WhereY() - Y);
                        Crt.GotoXY(Crt.WhereX(), Y);
                        break;
                    case 'B': // CSI n B - Moves the cursor n (default 1) cells down. If the cursor is already at the edge of the screen, this has no effect.
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Y = Math.Min(Crt.WindRows, Crt.WhereY() + Y);
                        Crt.GotoXY(Crt.WhereX(), Y);
                        break;
                    case 'C': // CSI n C - Moves the cursor n (default 1) cells right. If the cursor is already at the edge of the screen, this has no effect.
                        X = Math.Max(1, _AnsiParams.Dequeue());
                        X = Math.Min(Crt.WindCols, Crt.WhereX() + X);
                        Crt.GotoXY(X, Crt.WhereY());
                        break;
                    case 'D': // CSI n D - Moves the cursor n (default 1) cells left. If the cursor is already at the edge of the screen, this has no effect.
                        X = Math.Max(1, _AnsiParams.Dequeue());
                        X = Math.Max(1, Crt.WhereX() - X);
                        Crt.GotoXY(X, Crt.WhereY());
                        break;
                    case 'E': // CSI n E - Moves cursor to beginning of the line n (default 1) lines down.
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Y = Math.Min(Crt.WindRows, Crt.WhereY() + Y);
                        Crt.GotoXY(1, Y);
                        break;
                    case 'F': // CSI n F - Moves cursor to beginning of the line n (default 1) lines up.
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Y = Math.Max(1, Crt.WhereY() - Y);
                        Crt.GotoXY(1, Y);
                        break;
                    case 'f': // CSI y ; x f or CSI ; x f or CSI y ; f - Moves the cursor to row y, column x. The values are 1-based, and default to 1 (top left corner) if omitted. A sequence such as CSI ;5f is a synonym for CSI 1;5f as well as CSI 17;f is the same as CSI 17f and CSI 17;1f
                        AnsiCommand('H'); // Cheat and just call the more common H equivalent
                        break;
                    case 'G': // CSI n G - Moves the cursor to column n.
                        X = Math.Max(1, _AnsiParams.Dequeue());
                        if ((X >= 1) && (X <= Crt.WindCols))
                        {
                            Crt.GotoXY(X, Crt.WhereY());
                        }
                        break;
                    case 'H': // CSI y ; x H or CSI ; x H or CSI y ; H - Moves the cursor to row y, column x. The values are 1-based, and default to 1 (top left corner) if omitted. A sequence such as CSI ;5H is a synonym for CSI 1;5H as well as CSI 17;H is the same as CSI 17H and CSI 17;1H
                        while (_AnsiParams.Count < 2) _AnsiParams.Enqueue(1); // Make sure we have enough parameters
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        X = Math.Max(1, _AnsiParams.Dequeue());
                        Crt.GotoXY(X, Y);
                        break;
                    case 'h': // CSI n h
                        // n = 7 enables auto line wrap when writing to last column of screen (which is on by default so we ignore the sequence)
                        X = _AnsiParams.Dequeue();
                        switch (X)
                        {
                            case 7: /* Ignore */ break;
                            case 25: Crt.ShowCursor(); break;
                        }
                        break;
                    case 'J': // CSI n J - Clears part of the screen. If n is zero (or missing), clear from cursor to end of screen. If n is one, clear from cursor to beginning of the screen. If n is two, clear entire screen (and moves cursor to upper left on MS-DOS ANSI.SYS).
                        switch (_AnsiParams.Dequeue())
                        {
                            case 0: Crt.ClrEos(); break;
                            case 1: Crt.ClrBos(); break;
                            case 2: Crt.ClrScr(); break;
                        }
                        break;
                    case 'K': // CSI n K - Erases part of the line. If n is zero (or missing), clear from cursor to the end of the line. If n is one, clear from cursor to beginning of the line. If n is two, clear entire line. Cursor position does not change.
                        switch (_AnsiParams.Dequeue())
                        {
                            case 0: Crt.ClrEol(); break;
                            case 1: Crt.ClrBol(); break;
                            case 2: Crt.ClrLine(); break;
                        }
                        break;
                    case 'L': // CSI n L - Insert n new lines, pushing the current line and those below it down
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Crt.InsLine(Y);
                        break;
                    case 'l': // CSI n l
                        // n = 7 disables auto line wrap when writing to last column of screen (we dont support this)
                        X = _AnsiParams.Dequeue();
                        switch (X)
                        {
                            case 7: /* Ignore */ break;
                            case 25: Crt.HideCursor(); break;
                        }
                        break;
                    case 'M': // CSI n M - Delete n lines, pulling the lines below the deleted lines up
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Crt.DelLine(Y);
                        break;
                    case 'm': // CSI n [;k] m - Sets SGR parameters. After CSI can be zero or more parameters separated with ;. With no parameters, CSI m is treated as CSI 0 m (reset / normal), which is typical of most of the ANSI escape sequences.
                        while (_AnsiParams.Count > 0)
                        {
                            X = _AnsiParams.Dequeue();
                            switch (X)
                            {
                                case 0: // Reset / Normal (all attributes off)
                                    Crt.NormVideo();
                                    break;
                                case 1: // Intensity: Bold
                                    Crt.HighVideo();
                                    break;
                                case 2: // Intensity: Faint (not widely supported)
                                    break;
                                case 3: // Italic: on (not widely supported)
                                    break;
                                case 4: // Underline: Single
                                    break;
                                case 5: // Blink: Slow (< 150 per minute)
                                    Crt.TextAttr |= Crt.Blink;
                                    Crt.SetBlinkRate(500);
                                    break;
                                case 6: // Blink: Rapid (>= 150 per minute)
                                    Crt.TextAttr |= Crt.Blink;
                                    Crt.SetBlinkRate(250);
                                    break;
                                case 7: // Image: Negative (swap foreground and background)
                                    Crt.ReverseVideo();
                                    break;
                                case 8: // Conceal (not widely supported)
                                    _AnsiAttr = Crt.TextAttr;
                                    Crt.Conceal();
                                    break;
                                case 21: // Underline: Double (not widely supported)
                                    break;
                                case 22: //	Intensity: Normal (not widely supported)
                                    Crt.LowVideo();
                                    break;
                                case 24: // Underline: None
                                    break;
                                case 25: // Blink: off
                                    Crt.TextAttr &= ~Crt.Blink;
                                    break;
                                case 27: // Image: Positive (handle the same as negative
                                    Crt.ReverseVideo();
                                    break;
                                case 28: // Reveal (conceal off)
                                    Crt.TextAttr = _AnsiAttr;
                                    break;
                                case 30: // Set foreground color, normal intensity
                                case 31:
                                case 32:
                                case 33:
                                case 34:
                                case 35:
                                case 36:
                                case 37:
                                    Colour = ANSI_COLORS[X - 30];
                                    if (Crt.TextAttr % 16 > 7) Colour += 8;
                                    Crt.TextColor(Colour);
                                    break;
                                case 40: // Set background color, normal intensity
                                case 41:
                                case 42:
                                case 43:
                                case 44:
                                case 45:
                                case 46:
                                case 47:
                                    Colour = ANSI_COLORS[X - 40];
                                    Crt.TextBackground(Colour);
                                    break;
                            }
                        }
                        break;
                    case 'n': // CSI X n
                        //       n = 5 Device status report (reply with \x1B[0n to report "ready, no malfunction detected")
                        //       n = 6 Reports the cursor position to the application as (as though typed at the keyboard) ESC[n;mR, where n is the row and m is the column. (May not work on MS-DOS.)
                        //       n = 255 Reports the bottom right cursor position (essentially the screen size)
                        X = _AnsiParams.Dequeue();
                        switch (X)
                        {
                            case 5: RaiseESC5nEvent(); break;
                            case 6: RaiseESC6nEvent(); break;
                            case 255: RaiseESC255nEvent(); break;
                        }
                        break;
                    case 'Q': // CSI cp ; x ; y Q - NON-STANDARD fTelnet EXTENSION - Changes the current font to CodePage=cp, Width=x, Height=y
                        while (_AnsiParams.Count < 3) _AnsiParams.Enqueue(0); // Make sure we have enough parameters
                        X = _AnsiParams.Dequeue();
                        Y = _AnsiParams.Dequeue();
                        Z = _AnsiParams.Dequeue();
                        RaiseESCQEvent(); // TODO Should pass parameters to function
                        break;
                    case 'S': // CSI n S - Scroll whole page up by n (default 1) lines. New lines are added at the bottom. (not ANSI.SYS)
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Crt.ScrollUpScreen(Y);
                        break;
                    case 's': // CSI s - Saves the cursor position.
                        _SavedX = Crt.WhereX();
                        _SavedY = Crt.WhereY();
                        break;
                    case 'T': // CSI n T - Scroll whole page down by n (default 1) lines. New lines are added at the top. (not ANSI.SYS)
                        Y = Math.Max(1, _AnsiParams.Dequeue());
                        Crt.ScrollDownWindow(Y);
                        break;
                    case 'u': // CSI u - Restores the cursor position.
                        if ((_SavedX > 0) && (_SavedY > 0)) Crt.GotoXY(_SavedX, _SavedY);
                        break;
                }
            }
        }

        static public string ClrBol()
        {
            return ESC + "[1K";
        }

        static public string ClrBos()
        {
            return ESC + "[1J";
        }

        static public string ClrEol()
        {
            return ESC + "[K";
        }

        static public string ClrEos()
        {
            return ESC + "[J";
        }

        static public string ClrLine()
        {
            return ESC + "[2K";
        }

        static public string ClrScr()
        {
            return ESC + "[2J";
        }

        static public string CursorDown(int count)
        {
            return ESC + "[" + count.ToString() + "B";
        }

        static public string CursorLeft(int count)
        {
            return ESC + "[" + count.ToString() + "D";
        }

        static public string CursorPosition()
        {
            return CursorPosition(Crt.WhereXA(), Crt.WhereYA());
        }

        static public string CursorPosition(int column, int row)
		{
			return ESC + "[" + row + ";" + column + "R";	
		}

        static public string CursorRestore()
        {
            return ESC + "[u";
        }

        static public string CursorRight(int count)
        {
            return ESC + "[" + count.ToString() + "C";
        }

        static public string CursorSave()
        {
            return ESC + "[s";
        }

        static public string CursorUp(int count)
        {
            return ESC + "[" + count.ToString() + "A";
        }

        static public string GotoX(int column)
        {
            if (column == 1)
            {
                return CursorLeft(255);
            }
            else if (column > 1)
            {
                return CursorLeft(255) + CursorRight(column - 1);
            }
            else
            {
                return "";
            }
        }

        static public string GotoXY(int column, int row)
        {
            return ESC + "[" + row.ToString() + ";" + column.ToString() + "f";
        }

        static public string GotoY(int row)
        {
            if (row == 1)
            {
                return CursorUp(255);
            }
            else if (row > 1)
            {
                return CursorUp(255) + CursorDown(row - 1);
            }
            else
            {
                return "";
            }
        }

        private static void RaiseESC5nEvent()
        {
            EventHandler Handler = ESC5nEvent;
            if (Handler != null)
            {
                Handler(null, EventArgs.Empty);
            }
        }

        private static void RaiseESC6nEvent()
        {
            EventHandler Handler = ESC6nEvent;
            if (Handler != null)
            {
                Handler(null, EventArgs.Empty);
            }
        }

        private static void RaiseESC255nEvent()
        {
            EventHandler Handler = ESC255nEvent;
            if (Handler != null)
            {
                Handler(null, EventArgs.Empty);
            }
        }

        private static void RaiseESCQEvent()
        {
            EventHandler Handler = ESCQEvent;
            if (Handler != null)
            {
                Handler(null, EventArgs.Empty);
            }
        }

        static public string TextAttr(int attribute)
        {
            return TextColor(attribute % 16) + TextBackground(attribute / 16);
        }

        static public string TextBackground(int colour)
        {
            while (colour >= 8) colour -= 8;
            return ESC + "[" + (40 + ANSI_COLORS[colour]).ToString() + "m";
        }

        static public string TextColor(int colour)
        {
            switch (colour % 16)
            {
                case 0:
                case 1:
                case 2:
                case 3:
                case 4:
                case 5:
                case 6:
                case 7: return ESC + "[0;" + (30 + ANSI_COLORS[colour % 16]).ToString() + "m" + TextBackground(Crt.TextAttr / 16);
                case 8:
                case 9:
                case 10:
                case 11:
                case 12:
                case 13:
                case 14:
                case 15: return ESC + "[1;" + (30 + ANSI_COLORS[(colour % 16) - 8]).ToString() + "m";
            }

            return "";
        }

        static public void Write(string text)
        {
            lock (_Lock)
            {
                string Buffer = "";
                int Value = 0;

                for (int i = 0; i < text.Length; i++)
                {
                    if (text[i] == ESC)
                    {
                        _AnsiParserState = AnsiParserState.Escape;
                    }
                    else if ((_AnsiParserState == AnsiParserState.Escape) && (text[i] == '['))
                    {
                        _AnsiParserState = AnsiParserState.Bracket;
                        _AnsiBuffer = "0";
                        _AnsiParams.Clear();
                    }
                    else if (_AnsiParserState == AnsiParserState.Bracket)
                    {
                        if ("?=<> ".IndexOf(text[i]) != -1)
                        {
                            // Ignore these characters
                        }
                        else if ("0123456789".IndexOf(text[i]) != -1)
                        {
                            _AnsiBuffer += text[i];
                        }
                        else if (text[i] == ';')
                        {
                            if (!int.TryParse(_AnsiBuffer, out Value)) Value = 0;
                            _AnsiParams.Enqueue(Value);
                            _AnsiBuffer = "0";
                        }
                        else
                        {
                            Crt.Write(Buffer);
                            Buffer = "";

                            if (!int.TryParse(_AnsiBuffer, out Value)) Value = 0;
                            _AnsiParams.Enqueue(Value);
                            AnsiCommand(text[i]);
                            _AnsiParserState = AnsiParserState.None;
                        }
                    }
                    else
                    {
                        Buffer += text[i];
                    }
                }

                Crt.Write(Buffer);
            }
        }
    }

    /// <summary>
    /// The possible states the ANSI parser may find itself in
    /// </summary>
    public enum AnsiParserState
    {
        /// <summary>
        /// The default data state
        /// </summary>
        None,

        /// <summary>
        /// The last received character was an ESC
        /// </summary>
        Escape,

        /// <summary>
        /// The last received character was a [
        /// </summary>
        Bracket
    }
}
