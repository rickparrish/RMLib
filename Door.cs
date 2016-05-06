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
using System.IO;
using System.Text;
using System.Threading;
using System.Globalization;
using System.Text.RegularExpressions;
using System.Net.Sockets;
using System.Linq;

// TODO Need to handle telnet/rlogin negotiation
namespace RandM.RMLib
{
    /// <summary>
    /// Please note that the Door class is not thread safe.  If:
    ///   1) Anybody ever uses this class, and
    ///   2) A use-case for a thread safe Door class is presented
    /// then I'll be happy to consider adding the required locking to make it thread safe.
    /// </summary>
    public static class Door
    {
        /// <summary>
        /// Contains information that was read from the dropfile (if a dropfile was read)
        /// Not all dropfiles containt he same information, so only a subset of this data is guaranteed to exist
        /// </summary>
        public static TDropInfo DropInfo = new TDropInfo();

        /// <summary>
        /// Contains information about the last key that was read by the doorkit
        /// </summary>
        public static TLastKey LastKey = new TLastKey();

        /// <summary>
        /// The prompt that gets displayed when the More() method is called
        /// </summary>
        public static TMOREPrompt MOREPrompt = new TMOREPrompt();

        /// <summary>
        /// Contains information about the current session
        /// </summary>
        public static TSession Session = new TSession();

        private static TcpConnection _Connection = null;

        #region Standard R&M Door functions

        /// <summary>
        /// Instantiates and initializes the doorkit with default values
        /// </summary>
        static Door()
        {
            LocalEcho = false;
            PipeWrite = true;
            SethWrite = false;
            StripLF = true;
            StripNull = true;

            OnHangUp = DefaultOnHangUp;
            OnHangUp = DefaultOnHangUp;
            OnLocalLogin = DefaultOnLocalLogin;
            OnStatusBar = DefaultOnStatusBar;
            OnTimeOut = DefaultOnTimeOut;
            OnTimeUp = DefaultOnTimeUp;
            OnUsage = DefaultOnUsage;

            try
            {
                // Try to make the output look like it does on Windows
                Console.OutputEncoding = Encoding.GetEncoding("Windows-1252");
            }
            catch (Exception)
            {
                // Ignore, the user will just be stuck with whatever the default is
            }
        }

        /// <summary>
        /// Checks for a carrier.  Always returns true for local sessions
        /// </summary>
        /// <returns>True if local or carrier exists, false if no carrier exists</returns>
        public static bool Carrier
        {
            get { return (Local || _Connection.Connected); }
        }

        /// <summary>
        /// Clears the local and remote input buffers
        /// </summary>
        public static void ClearBuffers()
        {
            while (Crt.KeyPressed())
                Crt.ReadKey();

            if (!Local)
            {
                _Connection.ReadString();
            }
        }

        /// <summary>
        /// Closes the socket connection, which will disconnect the remote user
        /// </summary>
        public static void Disconnect()
        {
            if (!Local)
            {
                _Connection.ShutdownOnClose = true;
                _Connection.Close();
                _Connection = null;
            }

            DropInfo.SocketHandle = -1;
        }

        /// <summary>
        /// Clears all text to the end of the line
        /// </summary>
        public static void ClrEol()
        {
            Write(Ansi.ClrEol());
        }

        /// <summary>
        /// Clears all text on the screen
        /// </summary>
        public static void ClrScr()
        {
            Write(Ansi.ClrScr());
        }

        /// <summary>
        /// Moves the cursor down the screen
        /// </summary>
        /// <param name="count">The number of lines to move the cursor down</param>
        public static void CursorDown(int count)
        {
            Write(Ansi.CursorDown(count));
        }

        /// <summary>
        /// Moves the cursor to the left on the screen
        /// </summary>
        /// <param name="count">The number of columns to move the cursor left</param>
        public static void CursorLeft(int count)
        {
            Write(Ansi.CursorLeft(count));
        }

        /// <summary>
        /// Restores the previously saved cursor position
        /// </summary>
        /// <seealso cref="CursorSave"/>
        public static void CursorRestore()
        {
            Write(Ansi.CursorRestore());
        }

        /// <summary>
        /// Moves the cursor to the right on the screen
        /// </summary>
        /// <param name="count">The number of columns to move the cursor right</param>
        public static void CursorRight(int count)
        {
            Write(Ansi.CursorRight(count));
        }

        /// <summary>
        /// Saves the current cursor position
        /// </summary>
        /// <seealso cref="CursorRestore"/>
        public static void CursorSave()
        {
            Write(Ansi.CursorSave());
        }

        /// <summary>
        /// Moves the cursor up the screen
        /// </summary>
        /// <param name="count">The number of rows to move the cursor up</param>
        public static void CursorUp(int count)
        {
            Write(Ansi.CursorUp(count));
        }

        /// <summary>
        /// Displays a file (ANSI, ASCII, Text) on screen, optionally pausing every x lines
        /// </summary>
        /// <param name="fileName">The file to display</param>
        /// <param name="linesBeforePause">The number of lines to display before pausing.  0 causes no pauses</param>
        public static void DisplayFile(string fileName, int linesBeforePause)
        {
            if (File.Exists(fileName))
            {
                string[] Lines = FileUtils.FileReadAllLines(fileName, RMEncoding.Ansi);
                for (int i = 0; i < Lines.Length; i++)
                {
                    Write(Lines[i]);
                    if (i != Lines.Length - 1)
                    {
                        WriteLn();
                    }

                    if ((linesBeforePause > 0) && ((i + 1) % linesBeforePause == 0))
                    {
                        More();
                    }
                }
            }
        }

        private static void DoEvents()
        {
            TimeSpan Dif = DateTime.Now.Subtract(Session.EventsTime);
            if ((Session.Events) && (Dif.TotalMilliseconds > 1000))
            {
                //Check For Hangup
                if ((!Carrier) && (OnHangUp != null))
                {
                    OnHangUp(null, EventArgs.Empty);
                }

                // Check For Time Up
                if ((SecondsLeft < 1) && (OnTimeUp != null))
                {
                    OnTimeUp(null, EventArgs.Empty);
                }

                // Check For Idle Timeout
                if ((Session.DoIdleCheck) && (SecondsIdle > Session.MaxIdle) && (OnTimeOut != null))
                {
                    OnTimeOut(null, EventArgs.Empty);
                }

                // Check For Time Up Warning
                // TODO Fix this so it's not dependent on a per-second check
                if ((SecondsLeft % 60 == 1) && (SecondsLeft / 60 <= 5) && (OnTimeUpWarning != null))
                {
                    OnTimeUpWarning(null, EventArgs.Empty);
                }

                // Check For Idle Timeout Warning
                // TODO Fix this so it's not dependent on a per-second check
                if ((Session.DoIdleCheck) && ((Session.MaxIdle - SecondsIdle) % 60 == 1) && ((Session.MaxIdle - SecondsIdle) / 60 <= 5) && (OnTimeOutWarning != null))
                {
                    OnTimeOutWarning(null, EventArgs.Empty);
                }

                // Update Status Bar
                if (OnStatusBar != null)
                {
                    OnStatusBar(null, EventArgs.Empty);
                }

                Session.EventsTime = DateTime.Now;
            }
        }

        /// <summary>
        /// Draws a box on the screen with a customizable position, colour, and border
        /// </summary>
        /// <param name="left">Column for the left side of the box</param>
        /// <param name="top">Row for the top of the box</param>
        /// <param name="right">Column for the right side of the box</param>
        /// <param name="bottom">Row for the bottom of the box</param>
        /// <param name="foregroundColour">Foreground colour for the </param>
        /// <param name="backgroundColour"></param>
        /// <param name="borderStyle"></param>
        public static void DrawBox(int left, int top, int right, int bottom, int foregroundColour, int backgroundColour, CrtPanel.BorderStyle borderStyle)
        {
            // Characters for the box
            char TopLeft = '\0';
            char TopRight = '\0';
            char BottomLeft = '\0';
            char BottomRight = '\0';
            char TopBottom = '\0';
            char LeftRight = '\0';

            // Determine which character set to use
            switch (borderStyle)
            {
                case CrtPanel.BorderStyle.Single:
                    TopLeft = (char)218;
                    TopRight = (char)191;
                    BottomLeft = (char)192;
                    BottomRight = (char)217;
                    TopBottom = (char)196;
                    LeftRight = (char)179;
                    break;
                case CrtPanel.BorderStyle.Double:
                    TopLeft = (char)201;
                    TopRight = (char)187;
                    BottomLeft = (char)200;
                    BottomRight = (char)188;
                    TopBottom = (char)205;
                    LeftRight = (char)186;
                    break;
                case CrtPanel.BorderStyle.DoubleH:
                case CrtPanel.BorderStyle.SingleV:
                    TopLeft = (char)213;
                    TopRight = (char)184;
                    BottomLeft = (char)212;
                    BottomRight = (char)190;
                    TopBottom = (char)205;
                    LeftRight = (char)179;
                    break;
                case CrtPanel.BorderStyle.DoubleV:
                case CrtPanel.BorderStyle.SingleH:
                    TopLeft = (char)214;
                    TopRight = (char)183;
                    BottomLeft = (char)211;
                    BottomRight = (char)189;
                    TopBottom = (char)196;
                    LeftRight = (char)186;
                    break;
            }

            // Save current text colour and cursor position
            int SavedAttr = Crt.TextAttr;
            Write(Ansi.CursorSave());

            // Apply new text colour
            TextColor(foregroundColour);
            TextBackground(backgroundColour);

            // Draw top row
            GotoXY(left, top);
            Write(TopLeft.ToString());
            Write(new string(TopBottom, right - left - 1));
            Write(TopRight.ToString());

            // Draw middle rows
            for (int Line = top + 1; Line < bottom; Line++)
            {
                GotoXY(left, Line);
                Write(LeftRight.ToString());
                Write(new string(' ', right - left - 1));
                Write(LeftRight.ToString());
            }

            // Draw bottom row
            GotoXY(left, bottom);
            Write(BottomLeft.ToString());
            Write(new string(TopBottom, right - left - 1));
            Write(BottomRight.ToString());

            // Restore original text colour and cursor position
            Write(Ansi.CursorRestore());
            TextAttr(SavedAttr);
        }

        /// <summary>
        /// Move the cursor to the given x coordinate on the current line
        /// </summary>
        /// <param name="column">The 1-based x coordinate to move to</param>
        public static void GotoX(int column)
        {
            Write(Ansi.GotoX(column));
        }

        /// <summary>
        /// Move the cursor to the given x,y coordinate
        /// </summary>
        /// <param name="column">The 1-based x coordinate</param>
        /// <param name="row">The 1-based y coordinate</param>
        public static void GotoXY(int column, int row)
        {
            Write(Ansi.GotoXY(column, row));
        }

        /// <summary>
        /// Move the cursor to the given y coordinate on the current column
        /// </summary>
        /// <param name="row">The 1-based y coordinate to move to</param>
        public static void GotoY(int row)
        {
            Write(Ansi.GotoY(row));
        }

        /// <summary>
        /// Checks whether a key has been pressed on either the local or remote side
        /// </summary>
        /// <returns>True if a keypress is waiting, false if no keypress is available</returns>
        public static bool KeyPressed()
        {
            if (Local)
            {
                return Crt.KeyPressed();
            }
            else
            {
                DoEvents();
                return (Crt.KeyPressed() || _Connection.CanRead());
            }
        }

        /// <summary>
        /// Checks whether the door is running in local mode
        /// </summary>
        public static bool Local
        {
            get
            {
                return ((DropInfo.ComType == 0) || (DropInfo.SocketHandle == -1));
            }
        }

        /// <summary>
        /// Determines whether local echo is enabled
        /// </summary>
        public static bool LocalEcho { get; set; }

        /// <summary>
        /// Displays a MORE prompt and waits for a keypress.  Erased the prompt after a key is pressed
        /// </summary>
        public static void More()
        {
            string Line = "";
            int LineLength = 0;

            switch (DropInfo.Emulation)
            {
                case DoorEmulationType.ASCII:
                    Line = MOREPrompt.ASCII;
                    LineLength = MOREPrompt.ASCII.Length;
                    break;

                case DoorEmulationType.ANSI:
                    Line = MOREPrompt.ANSI;
                    LineLength = MOREPrompt.ANSILength;
                    break;
            }

            int OldAttr = Crt.TextAttr;

            Write(Line);
            ReadKey();

            CursorLeft(LineLength);
            Write("|00" + new string(' ', LineLength));
            CursorLeft(LineLength);

            TextAttr(OldAttr);
        }

        /// <summary>
        /// Initializes the socket (when not in local mode)
        /// </summary>
        /// <returns>True if the socket was opened, false if it was not</returns>
        public static bool Open()
        {
            if (Local)
            {
                return true;
            }
            else
            {
                switch (DropInfo.ComType)
                {
                    case 0: return true; // Local
                    case 1: return false; // Serial
                    case 2: _Connection = new TelnetConnection(); break;
                    case 3: _Connection = new RLoginConnection(); break;
                    case 4: _Connection = new WebSocketConnection(false); break;
                }
                _Connection.Open(DropInfo.SocketHandle);
                _Connection.ShutdownOnClose = false;

                return _Connection.Connected;
            }
        }

        /// <summary>
        /// Transforms pipe sequences to ansi sequences in the input string
        /// </summary>
        /// <param name="AText">The string with pipe sequences</param>
        /// <returns>A string with ansi sequences in place of pipe sequences</returns>
        private static string PipeToAnsi(string AText)
        {
            if (AText.Contains("|"))
            {
                // Replace the colour codes
                for (int i = 0; i < 255; i++)
                {
                    string Code = "|" + i.ToString("X2");
                    if (AText.Contains(Code))
                    {
                        AText = AText.Replace(Code, Ansi.TextAttr(i));
                        if (!AText.Contains("|")) break;
                    }
                }
            }
            return AText;
        }

        /// <summary>
        /// Determines whether pipe sequences should automatically be transformed to ansi sequences
        /// </summary>
        public static bool PipeWrite { get; set; }

        /// <summary>
        /// Reads a byte from either the local or remote side with no translation
        /// </summary>
        /// <returns>A byte if one was available, or null if no key was waiting</returns>
        public static byte? ReadByte()
        {
            byte? B = null;
            LastKey.Location = DoorKeyLocation.None;
            do
            {
                while (!KeyPressed())
                {
                    Thread.Sleep(1); // TODO Should not be here
                }
                if (Crt.KeyPressed())
                {
                    B = (byte)Crt.ReadKey();
                    LastKey.Location = DoorKeyLocation.Local;
                }
                else if (!Local && _Connection.CanRead())
                {
                    B = (byte)_Connection.ReadChar(0);
                    LastKey.Location = DoorKeyLocation.Remote;
                }
            } while (LastKey.Location == DoorKeyLocation.None);

            if (B != null)
            {
                LastKey.Ch = (char)B;
                LastKey.Time = DateTime.Now;
            }

            return B;
        }

        /// <summary>
        /// Reads a key from either the local or remote side
        /// </summary>
        /// <returns>A key if one was available, or null if no key was waiting</returns>
        public static char? ReadKey()
        {
            char? Ch = null;
            LastKey.Location = DoorKeyLocation.None;
            do
            {
                while (!KeyPressed())
                {
                    Thread.Sleep(1); // TODO Should not be here
                }
                if (Crt.KeyPressed())
                {
                    Ch = Crt.ReadKey();
                    if (Ch == '\0')
                    {
                        Ch = Crt.ReadKey();
                        switch (Ch)
                        {
                            case 'H':
                                Ch = ExtendedKeys.UpArrow;
                                LastKey.Location = DoorKeyLocation.Local;
                                break;
                            case 'K':
                                Ch = ExtendedKeys.LeftArrow;
                                LastKey.Location = DoorKeyLocation.Local;
                                break;
                            case 'M':
                                Ch = ExtendedKeys.RightArrow;
                                LastKey.Location = DoorKeyLocation.Local;
                                break;
                            case 'P':
                                Ch = ExtendedKeys.DownArrow;
                                LastKey.Location = DoorKeyLocation.Local;
                                break;
                            default:
                                // Non-arrow special key, pass to door to see if they want to do something
                                // with it (ie maybe there's a shortcut for hang-up).  Either way, this special
                                // key doesn't get added to the input buffer
                                if ((!Local) && (OnSysOpKey != null))
                                {
                                    OnSysOpKey(null, new CharEventArgs((char)Ch));
                                }
                                break;
                        }
                    }
                    else
                    {
                        LastKey.Location = DoorKeyLocation.Local;
                    }
                }
                else if (!Local && _Connection.CanRead())
                {
                    Ch = _Connection.ReadChar(0);
                    if (Ch == '\x1B')
                    {
                        // ESC, check if we have more data
                        if (!_Connection.CanRead())
                        {
                            // No data waiting, so wait 1/10th of a second to see if more data comes
                            Thread.Sleep(100);
                        }

                        // Check if we have data to follow the ESC
                        if (_Connection.CanRead())
                        {
                            // We have more data, see if it's a [
                            char SecondChar = (char)_Connection.PeekChar();
                            if (SecondChar == '[')
                            {
                                // Consume the [ since it's most likely part of an escape sequence (if someone actually hit ESC and [ on their own, tough luck)
                                _Connection.ReadChar(0);

                                // Now we have ESC[, see if we have more data
                                if (!_Connection.CanRead())
                                {
                                    // No data waiting, so wait 1/10th of a second to see if more data comes
                                    Thread.Sleep(100);
                                }

                                // Check if we have data to follow the ESC[
                                if (_Connection.CanRead())
                                {
                                    // We have more data, see if it's a sequence we handle
                                    // TODO Not all sequences are ESC[ followed by a single byte, ie F1
                                    char ThirdChar = (char)_Connection.ReadChar(0);
                                    switch (ThirdChar)
                                    {
                                        case 'A':
                                            Ch = Door.ExtendedKeys.UpArrow;
                                            LastKey.Location = DoorKeyLocation.Remote;
                                            break;
                                        case 'B':
                                            Ch = Door.ExtendedKeys.DownArrow;
                                            LastKey.Location = DoorKeyLocation.Remote;
                                            break;
                                        case 'C':
                                            Ch = Door.ExtendedKeys.RightArrow;
                                            LastKey.Location = DoorKeyLocation.Remote;
                                            break;
                                        case 'D':
                                            Ch = Door.ExtendedKeys.LeftArrow;
                                            LastKey.Location = DoorKeyLocation.Remote;
                                            break;
                                    }
                                }
                            }
                            else
                            {
                                // ESC not followed by [
                                LastKey.Location = DoorKeyLocation.Remote;
                            }
                        }
                        else
                        {
                            // ESC not followed by anything
                            LastKey.Location = DoorKeyLocation.Remote;
                        }
                    }
                    else
                    {
                        // Not an ESC
                        LastKey.Location = DoorKeyLocation.Remote;
                    }
                }
            } while (LastKey.Location == DoorKeyLocation.None);

            if (Ch != null)
            {
                LastKey.Ch = (char)Ch;
                LastKey.Time = DateTime.Now;

                if (LocalEcho)
                {
                    if (Ch == '\x08') // Backspace
                    {
                        Write("\x08 \x08");
                    }
                    else if (Ch == '\r') // Enter
                    {
                        Write("\r\n");
                    }
                    else if ((Ch >= 32) && (Ch <= 126))
                    {
                        Write(Ch.ToString());
                    }
                }
            }

            return Ch;
        }

        /// <summary>
        /// A shortcut for TextBox()
        /// </summary>
        /// <returns>The line of text input by the user</returns>
        public static string ReadLn()
        {
            return TextBox("", CharacterMask.All, '\0', 50, 50, 7);
        }

        /// <summary>
        /// The number of seconds the user has been idle for
        /// </summary>
        public static int SecondsIdle
        {
            get
            {
                return (int)DateTime.Now.Subtract(LastKey.Time).TotalSeconds;
            }
        }

        /// <summary>
        /// The number of seconds the user has left this call
        /// </summary>
        public static int SecondsLeft
        {
            get
            {
                return (DropInfo.MaxTime - SecondsOn);
            }
        }

        /// <summary>
        /// The number of seconds the user has been in the door
        /// </summary>
        public static int SecondsOn
        {
            get
            {
                return (int)DateTime.Now.Subtract(Session.TimeOn).TotalSeconds;
            }
        }

        /// <summary>
        /// Determines whether "seth" (aka LORD) sequences should be parsed
        /// </summary>
        public static bool SethWrite { get; set; }

        /// <summary>
        /// Closes the connection in a way that ensures the caller's connection doesn't get dropped
        /// Very important to close this before your door quits!
        /// </summary>
        public static void Shutdown()
        {
            if (_Connection != null)
            {
                _Connection.Close();
                _Connection = null;
            }
        }

        /// <summary>
        /// Parses the command-line and gets the doorkit ready for use
        /// </summary>
        public static void Startup()
        {
            string DropFile = "";
            bool Local = false;
            int Node = 0;
            int Socket = -1;

            foreach (string Arg in Environment.GetCommandLineArgs().Skip(1)) // Skip 1 because first is path/filename of door's EXE
            {
                if ((Arg.Length >= 2) && ((Arg[0] == '/') || (Arg[0] == '-')))
                {
                    char Key = Arg.ToUpper()[1];
                    string Value = Arg.Substring(2);

                    switch (Key)
                    {
                        case 'C':
                            if (!int.TryParse(Value, out DropInfo.ComType)) DropInfo.ComType = 0;
                            break;
                        case 'D':
                            DropFile = Value;
                            break;
                        case 'H':
                            if (!int.TryParse(Value, out Socket)) Socket = -1;
                            break;
                        case 'L':
                            Local = true;
                            break;
                        case 'N':
                            if (!int.TryParse(Value, out Node)) Node = 0;
                            break;
                        default:
                            if (OnCLP != null)
                            {
                                EventHandler<CommandLineParameterEventArgs> Handler = OnCLP;
                                if (Handler != null) Handler(null, new CommandLineParameterEventArgs(Key, Value));
                            }
                            break;
                    }
                }
            }

            if (Local)
            {
                DropInfo.Node = Node;
                if (OnLocalLogin != null)
                {
                    OnLocalLogin(null, EventArgs.Empty);
                    ClrScr();
                }
            }
            else if ((Socket > 0) && (Node > 0))
            {
                if (DropInfo.ComType == 0) DropInfo.ComType = 2;
                DropInfo.SocketHandle = Socket;
                DropInfo.Node = Node;
            }
            else if (!string.IsNullOrEmpty(DropFile))
            {
                if (File.Exists(DropFile))
                {
                    // 'DropFile' contains a full filename, so confirm it is a DOOR32.SYS file
                    if (DropFile.ToUpper().Contains("DOOR32.SYS"))
                    {
                        ReadDoor32(DropFile);
                    }
                    else
                    {
                        // TODOZ Maybe support other drop file formats for Linux, and assume local mode when they're used (ie DOOR.SYS, DORINFO.DEF)
                        ClrScr();
                        WriteLn();
                        WriteLn("  Drop File '" + Path.GetFileName(DropFile) + "' Not Supported");
                        WriteLn();
                        Thread.Sleep(2500);
                        throw new Exception("Drop File '" + Path.GetFileName(DropFile) + "' Not Supported");
                    }
                }
                else if (Directory.Exists(DropFile))
                {
                    // 'DropFile' only contains a path, so try both door32.sys and DOOR32.SYS (for Linux, which is case sensitive)
                    if (!ReadDoor32(StringUtils.PathCombine(DropFile, "door32.sys")) && !ReadDoor32(StringUtils.PathCombine(DropFile, "DOOR32.SYS")))
                    {
                        ClrScr();
                        WriteLn();
                        WriteLn("  DOOR32.SYS not found in '" + DropFile + "'");
                        WriteLn();
                        Thread.Sleep(2500);
                        throw new Exception("DOOR32.SYS not found in '" + DropFile + "'");
                    }
                }
                else
                {
                    ClrScr();
                    WriteLn();
                    WriteLn("  Drop File '" + DropFile + "' Not Found");
                    WriteLn();
                    Thread.Sleep(2500);
                    throw new Exception("Drop File '" + DropFile + "' Not Found");
                }
            }
            else
            {
                OnUsage?.Invoke(null, EventArgs.Empty);
            }

            if (!Local)
            {
                if (!Open())
                {
                    ClrScr();
                    WriteLn();
                    WriteLn("  No Carrier Detected");
                    WriteLn();
                    Thread.Sleep(2500);
                    throw new Exception("No Carrier Detected"); //Environment.Exit(0);
                }
            }

            Crt.Window(1, 1, 80, 24);
            ClrScr();
        }

        /// <summary>
        /// Determines whether the LF in CR+LF pairs is stripped out
        /// </summary>
        public static bool StripLF
        {
            get
            {
                if (Local)
                {
                    return true;
                }
                else
                {
                    return _Connection.StripLF;
                }
            }
            set
            {
                if (!Local)
                {
                    _Connection.StripLF = value;
                }
            }
        }

        /// <summary>
        /// Determines whether the NULL in CR+NULL pairs is stripped out
        /// </summary>
        public static bool StripNull
        {
            get
            {
                if (Local)
                {
                    return true;
                }
                else
                {
                    return _Connection.StripNull;
                }
            }
            set
            {
                if (!Local)
                {
                    _Connection.StripNull = value;
                }
            }
        }

        /// <summary>
        /// Strips "seth" (AKA LORD) sequences from an input string
        /// </summary>
        /// <param name="text">The text containing seth sequences</param>
        /// <returns>The text with seth sequences removed</returns>
        public static string StripSeth(string text)
        {
            if (text.Contains("`"))
            {
                text = Regex.Replace(text, "`1", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`2", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`3", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`4", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`5", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`6", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`7", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`8", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`9", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`0", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[!]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[@]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[#]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[$]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[%]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[*]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`b", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`c", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`d", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`k", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`l", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`w", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`x", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[\\\\]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[|]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`[.]", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r0", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r1", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r2", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r3", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r4", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r5", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r6", "", RegexOptions.IgnoreCase);
                text = Regex.Replace(text, "`r7", "", RegexOptions.IgnoreCase);
            }

            return text;
        }

        /// <summary>
        /// A very basic sysop chat feature
        /// </summary>
        public static void SysopChat()
        {
            char? Ch = null;
            DoorKeyLocation OurLastKeyLocation = DoorKeyLocation.None;

            do
            {
                if (KeyPressed())
                {
                    Ch = ReadKey();

                    if ((Ch >= 32) && (Ch <= 126))
                    {
                        if (OurLastKeyLocation != LastKey.Location)
                        {
                            switch (LastKey.Location)
                            {
                                case DoorKeyLocation.Local:
                                    TextColor((int)ConsoleColor.Green);
                                    break;
                                case DoorKeyLocation.Remote:
                                    TextColor((int)ConsoleColor.Red);
                                    break;
                            }
                            OurLastKeyLocation = LastKey.Location;
                        }

                        Write(Ch.ToString());
                    }
                    else if (Ch == '\x0D')
                    {
                        WriteLn();
                    }
                }
            } while (Ch != '\x1B');
        }

        /// <summary>
        /// Sets both the foreground and background colour to use for future write operations
        /// </summary>
        /// <param name="attribute">The new colour to use</param>
        public static void TextAttr(int attribute)
        {
            Write(Ansi.TextAttr(attribute));
        }

        /// <summary>
        /// Sets the background colour to use for future write operations (the foreground colour is unchanged)
        /// </summary>
        /// <param name="colour">The new background colour to use</param>
        public static void TextBackground(int colour)
        {
            Write(Ansi.TextBackground(colour));
        }

        /// <summary>
        /// Basically the doorkit equivalent of a textbox you might see on a webpage
        /// </summary>
        /// <param name="defaultText">The text to put in the text box by default</param>
        /// <param name="allowedCharacters">The characters the user is allowed to type in the box (see CharacterMask class for some built-in masks)</param>
        /// <param name="passwordCharacter">The character to echo to the screen for secure inputs, or '\0' to display the actual character the user presses</param>
        /// <param name="numberOfCharactersToDisplay">The size (width) of the textbox</param>
        /// <param name="maximumLength">The maximum length of the input string (if this is greater than numberOfCharactersToDisplay, the textbox will scroll)</param>
        /// <param name="attribute">The colour of the textbox and input text</param>
        /// <returns>The line of text input by the user</returns>
        public static string TextBox(string defaultText, string allowedCharacters, char passwordCharacter, int numberOfCharactersToDisplay, int maximumLength, int attribute)
        {
            if (defaultText.Length > maximumLength)
            {
                defaultText = defaultText.Substring(0, maximumLength);
            }
            string S = defaultText;

            int SavedAttr = Crt.TextAttr;
            TextAttr(attribute);
            Write(Ansi.CursorSave());

            char? Ch = null;
            bool UpdateText = true;

            do
            {
                if (UpdateText)
                {
                    UpdateText = false;
                    Write(Ansi.CursorRestore());
                    if (S.Length > numberOfCharactersToDisplay)
                    {
                        if (passwordCharacter == '\0')
                        {
                            Write(S.Substring(S.Length - numberOfCharactersToDisplay, numberOfCharactersToDisplay));
                        }
                        else
                        {
                            Write(new string(passwordCharacter, numberOfCharactersToDisplay));
                        }
                    }
                    else
                    {
                        if (passwordCharacter == '\0')
                        {
                            Write(S);
                        }
                        else
                        {
                            Write(new string(passwordCharacter, S.Length));
                        }
                        Write(new string(' ', numberOfCharactersToDisplay - S.Length));
                        Write(Ansi.CursorLeft(numberOfCharactersToDisplay - S.Length));
                    }
                }

                Ch = ReadKey();
                if (Ch != null)
                {
                    if (Ch == '\x08') // Backspace
                    {
                        if (S.Length > 0)
                        {
                            S = S.Substring(0, S.Length - 1);
                            Write("\x08 \x08");
                            if (S.Length >= numberOfCharactersToDisplay)
                            {
                                UpdateText = true;
                            }
                        }
                    }
                    else if (Ch == '\x19') // Ctrl-Y
                    {
                        S = "";
                        UpdateText = true;
                    }
                    else if ((S.Length < maximumLength) && (allowedCharacters.IndexOf((char)Ch) != -1))
                    {
                        S = S + Ch;
                        if (S.Length > numberOfCharactersToDisplay)
                        {
                            UpdateText = true;
                        }
                        else
                        {
                            if (passwordCharacter == '\0')
                            {
                                Write(Ch.ToString());
                            }
                            else
                            {
                                Write(passwordCharacter.ToString());
                            }
                        }
                    }

                    // Check if key was enter and string is blank
                    if ((Ch == '\x0D') && (S.Length == 0))
                    {
                        // It is, so override
                        Ch = null;
                    }
                }
            } while ((Ch != '\x1B') && (Ch != '\x0D'));

            TextAttr(SavedAttr);
            WriteLn();

            if (Ch == '\x1B')
            {
                S = defaultText;
            }

            return S;
        }

        /// <summary>
        /// Sets the foreground colour to use for future write operations (the background colour is unchanged)
        /// </summary>
        /// <param name="colour">The new foreground colour to use</param>
        public static void TextColor(int colour)
        {
            Write(Ansi.TextColor(colour));
        }

        /// <summary>
        /// Writes text to both the local and remote screen
        /// </summary>
        /// <param name="text">The text to write</param>
        public static void Write(string text)
        {
            if (PipeWrite && (text.Contains("|"))) text = PipeToAnsi(text);

            if (SethWrite && (text.Contains("`")))
            {
                while (text.Length > 0)
                {
                    // Write everything up to the next backtick
                    if (!text.StartsWith("`"))
                    {
                        if (text.Contains("`"))
                        {
                            string BeforeBackTick = text.Substring(0, text.IndexOf('`'));
                            Ansi.Write(BeforeBackTick);
                            if (!Local) _Connection.Write(BeforeBackTick);
                            text = text.Substring(BeforeBackTick.Length);
                        }
                        else
                        {
                            Ansi.Write(text);
                            if (!Local) _Connection.Write(text);
                            text = "";
                        }
                    }

                    // Now we have a backtick at the beginning of the string
                    while (text.StartsWith("`"))
                    {
                        string BackTick2 = (text.Length >= 2 ? text.Substring(0, 2) : "");
                        switch (BackTick2.ToLower())
                        {
                            case "``":
                                Ansi.Write("`");
                                if (!Local) _Connection.Write("`");
                                text = text.Substring(2);
                                break;
                            case "`1":
                                TextColor(Crt.Blue);
                                text = text.Substring(2);
                                break;
                            case "`2":
                                TextColor(Crt.Green);
                                text = text.Substring(2);
                                break;
                            case "`3":
                                TextColor(Crt.Cyan);
                                text = text.Substring(2);
                                break;
                            case "`4":
                                TextColor(Crt.Red);
                                text = text.Substring(2);
                                break;
                            case "`5":
                                TextColor(Crt.Magenta);
                                text = text.Substring(2);
                                break;
                            case "`6":
                                TextColor(Crt.Brown);
                                text = text.Substring(2);
                                break;
                            case "`7":
                                TextColor(Crt.LightGray);
                                text = text.Substring(2);
                                break;
                            case "`8":
                                TextColor(Crt.White); // Supposed to be dark gray, but a bug has this as white (TODO Check if this is still accurate)
                                text = text.Substring(2);
                                break;
                            case "`9":
                                TextColor(Crt.LightBlue);
                                text = text.Substring(2);
                                break;
                            case "`0":
                                TextColor(Crt.LightGreen);
                                text = text.Substring(2);
                                break;
                            case "`!":
                                TextColor(Crt.LightCyan);
                                text = text.Substring(2);
                                break;
                            case "`@":
                                TextColor(Crt.LightRed);
                                text = text.Substring(2);
                                break;
                            case "`#":
                                TextColor(Crt.LightMagenta);
                                text = text.Substring(2);
                                break;
                            case "`$":
                                TextColor(Crt.Yellow);
                                text = text.Substring(2);
                                break;
                            case "`%":
                                TextColor(Crt.White);
                                text = text.Substring(2);
                                break;
                            case "`*":
                                TextColor(Crt.Black);
                                text = text.Substring(2);
                                break;
                            case "`b": // TODO Case sensitive?
                                // TODO
                                text = text.Substring(2);
                                break;
                            case "`c": // TODO Case sensitive?
                                TextAttr(7);
                                ClrScr();
                                Ansi.Write("\r\n\r\n");
                                if (!Local) _Connection.Write("\r\n\r\n");
                                text = text.Substring(2);
                                break;
                            case "`d": // TODO Case sensitive?
                                Ansi.Write("\x08");
                                if (!Local) _Connection.Write("\x08");
                                text = text.Substring(2);
                                break;
                            case "`k": // TODO Case sensitive?
                                Write("  `2<`0MORE`2>");
                                ReadKey();
                                Write("\b\b\b\b\b\b\b\b        \b\b\b\b\b\b\b\b");
                                text = text.Substring(2);
                                break;
                            case "`l": // TODO Case sensitive?
                                Thread.Sleep(500);
                                text = text.Substring(2);
                                break;
                            case "`w": // TODO Case sensitive?
                                Thread.Sleep(100);
                                text = text.Substring(2);
                                break;
                            case "`x": // TODO Case sensitive?
                                Ansi.Write(" ");
                                if (!Local) _Connection.Write(" ");
                                text = text.Substring(2);
                                break;
                            case "`\\":
                                Ansi.Write("\r\n");
                                if (!Local) _Connection.Write("\r\n");
                                text = text.Substring(2);
                                break;
                            case "`|":
                                // TODO Unknown what this does, but it's used once in LORD2
                                text = text.Substring(2);
                                break;
                            case "`.":
                                // TODO Also unknown, used by RTNEWS
                                text = text.Substring(2);
                                break;
                            default:
                                string BackTick3 = (text.Length >= 3 ? text.Substring(0, 3) : "");
                                switch (BackTick3.ToLower())
                                {
                                    case "`r0":
                                        TextBackground(Crt.Black);
                                        text = text.Substring(3);
                                        break;
                                    case "`r1":
                                        TextBackground(Crt.Blue);
                                        text = text.Substring(3);
                                        break;
                                    case "`r2":
                                        TextBackground(Crt.Green);
                                        text = text.Substring(3);
                                        break;
                                    case "`r3":
                                        TextBackground(Crt.Cyan);
                                        text = text.Substring(3);
                                        break;
                                    case "`r4":
                                        TextBackground(Crt.Red);
                                        text = text.Substring(3);
                                        break;
                                    case "`r5":
                                        TextBackground(Crt.Magenta);
                                        text = text.Substring(3);
                                        break;
                                    case "`r6":
                                        TextBackground(Crt.Brown);
                                        text = text.Substring(3);
                                        break;
                                    case "`r7":
                                        TextBackground(Crt.LightGray);
                                        text = text.Substring(3);
                                        break;
                                    default:
                                        // No match, so output the backtick
                                        Ansi.Write("`");
                                        if (!Local) _Connection.Write("`");
                                        text = text.Substring(1);
                                        break;
                                }
                                break;
                        }
                    }
                }
            }
            else
            {
                Ansi.Write(text);
                if (!Local) _Connection.Write(text);
            }
        }

        /// <summary>
        /// Advances the cursor to the beginning of the next line, scrolling the screen if necessary
        /// </summary>
        public static void WriteLn()
        {
            Write("\r\n");
        }

        /// <summary>
        /// Outputs a string of text to the screen before advancing the cursor to the beginning of the next line, scrolling if necessary
        /// </summary>
        /// <param name="text">The text to be displayed</param>
        public static void WriteLn(string text)
        {
            Write(text + "\r\n");
        }

        private static bool ReadDoor32(string AFile)
        {
            if (File.Exists(AFile))
            {
                string[] Lines = FileUtils.FileReadAllLines(AFile);

                int.TryParse(Lines[0], out DropInfo.ComType); // 1 - Comm type (0=local, 1=serial, 2=telnet, 3=rlogin, 4=websocket)
                int.TryParse(Lines[1], out DropInfo.SocketHandle); // 2 - Comm or socket handle
                int.TryParse(Lines[2], out DropInfo.Baud); // 3 - Baud rate
                // 4 - BBSID (software name and version)
                int.TryParse(Lines[4], out DropInfo.RecPos); // 5 - User record position (1-based)
                DropInfo.RealName = Lines[5]; // 6 - User's real name
                DropInfo.Alias = Lines[6]; // 7 - User's handle/alias
                int.TryParse(Lines[7], out DropInfo.Access); // 8 - User's security level
                int.TryParse(Lines[8], out DropInfo.MaxTime); // 9 - User's time left (in minutes)
                DropInfo.MaxTime *= 60;
                DropInfo.Emulation = (Lines[9] == "0") ? DoorEmulationType.ASCII : DoorEmulationType.ANSI; // 10 - Emulation (0=Ascii, 1=Ansi, 2=Avatar, 3=RIP, 4=MaxGfx)
                int.TryParse(Lines[10], out DropInfo.Node); // 11 - Current node number
                return true;
            }

            return false;
        }
        #endregion

        #region Event handlers
        // It is not recommended you change these, instead
        // you should just reassign the above On* variables
        // to your own procedures.

        /// <summary>
        /// The event that gets raised for each command-line parameter
        /// </summary>
        public static event EventHandler<CommandLineParameterEventArgs> OnCLP = null;

        /// <summary>
        /// The event that gets raised when the user hangs up in the door
        /// </summary>
        public static event EventHandler OnHangUp = null;

        private static void DefaultOnHangUp(object sender, EventArgs e)
        {
            TextAttr(15);
            ClrScr();
            WriteLn();
            WriteLn("   Caller Dropped Carrier.  Returning To BBS...");
            Thread.Sleep(2500);
            throw new Exception("Caller Dropped Carrier"); //Environment.Exit(0);
        }

        /// <summary>
        /// The event that gets raised when a user logs in locally
        /// </summary>
        public static event EventHandler OnLocalLogin = null;

        private static void DefaultOnLocalLogin(object sender, EventArgs e)
        {
            ClrScr();
            DrawBox(2, 2, 18, 6, Crt.White, Crt.Blue, CrtPanel.BorderStyle.Double);
            GotoXY(5, 4);
            Write("|1FLOCAL LOGIN|07");

            GotoXY(2, 8);
            Write("Enter your name : ");
            string S = TextBox("SYSOP", CharacterMask.AlphanumericWithSpace, '\0', 40, 40, 31);
            DropInfo.RealName = S;
            DropInfo.Alias = S;
        }

        /// <summary>
        /// The event that gets raised when the status bar needs to be updated
        /// </summary>
        public static event EventHandler OnStatusBar = null;

        private static void DefaultOnStatusBar(object sender, EventArgs e)
        {
            Crt.FastWrite("þ                            þ                  þ             þ                þ", 1, 25, 30);
            Crt.FastWrite((DropInfo.RealName + new string(' ', 26)).Substring(0, 26), 3, 25, 31);
            Crt.FastWrite("R&M Door Library", 32, 25, 31);
            Crt.FastWrite(("Idle: " + StringUtils.SecToMS(SecondsIdle) + new string(' ', 11)).Substring(0, 11), 51, 25, 31);
            Crt.FastWrite(("Left: " + StringUtils.SecToHMS(SecondsLeft) + new string(' ', 14)).Substring(0, 14), 65, 25, 31);
        }

        /// <summary>
        /// The event that gets raised when a "special" key is pressed in the local window
        /// </summary>
        public static event EventHandler<CharEventArgs> OnSysOpKey = null;

        /// <summary>
        /// The event that gets raised when the user idles for too long
        /// </summary>
        public static event EventHandler OnTimeOut = null;

        private static void DefaultOnTimeOut(object sender, EventArgs e)
        {
            TextAttr(15);
            ClrScr();
            WriteLn();
            WriteLn("   Idle Time Limit Exceeded.  Returning To BBS...");
            Thread.Sleep(2500);
            throw new Exception("Idle Time Limit Exceeded"); //Environment.Exit(0);
        }

        /// <summary>
        /// The event that gets raised each minute when the user is idling
        /// </summary>
        public static event EventHandler OnTimeOutWarning = null;

        /// <summary>
        /// The event that gets raised when the user runs out of time
        /// </summary>
        public static event EventHandler OnTimeUp = null;

        private static void DefaultOnTimeUp(object sender, EventArgs e)
        {
            TextAttr(15);
            ClrScr();
            WriteLn();
            WriteLn("   Your Time Has Expired.  Returning To BBS...");
            Thread.Sleep(2500);
            throw new Exception("Your Time Has Expired"); //Environment.Exit(0);
        }

        /// <summary>
        /// The event that gets raised each minute when the user is running out of time
        /// </summary>
        public static event EventHandler OnTimeUpWarning = null;

        /// <summary>
        /// The event that gets raised when the usage screen needs to be displayed
        /// </summary>
        public static event EventHandler OnUsage = null;

        private static void DefaultOnUsage(object sender, EventArgs e)
        {
            string EXE = Path.GetFileName(ProcessUtils.ExecutablePath);

            ClrScr();
            WriteLn();
            WriteLn(" USAGE:");
            WriteLn();
            WriteLn(" " + EXE + " <PARAMETERS>");
            WriteLn();
            WriteLn("  -C         COMM TYPE (2=Telnet (Default), 3=RLogin, 4=WebSocket)");
            WriteLn("  -D         PATH\\FILENAME OF DROPFILE");
            WriteLn("  -L         LOCAL MODE");
            WriteLn("  -H         SOCKET HANDLE");
            WriteLn("  -N         NODE NUMBER");
            WriteLn();
            WriteLn(" Examples:");
            WriteLn();
            WriteLn(" " + EXE + " -L");
            WriteLn("  -  Run In Local Mode");
            WriteLn(" " + EXE + " -DC:\\GAMESRV\\NODE1\\DOOR32.SYS");
            WriteLn("  -  Load Settings From DOOR32.SYS");
            WriteLn(" " + EXE + " -H1000 -N1");
            WriteLn("  -  Open Telnet Socket Handle 1000 On Node #1");
            WriteLn(" " + EXE + " -C4 -H2000 -N2");
            WriteLn("  -  Open WebSocket Socket Handle 2000 On Node #2");
            Thread.Sleep(2500);
            throw new Exception("Usage"); //Environment.Exit(0);
        }

        #endregion

        /// <summary>
        /// Special keys that can be returned by ReadKey()
        /// </summary>
        public static class ExtendedKeys
        {
            /// <summary>
            /// The down arrow key
            /// </summary>
            public const char DownArrow = '\u2193';

            /// <summary>
            /// The left arrow key
            /// </summary>
            public const char LeftArrow = '\u2190';

            /// <summary>
            /// The right arrow key
            /// </summary>
            public const char RightArrow = '\u2192';

            /// <summary>
            /// The up arrow key
            /// </summary>
            public const char UpArrow = '\u2191';
        }
    }

    /// <summary>
    /// The available emulation types supported by Door
    /// </summary>
    public enum DoorEmulationType
    {
        /// <summary>
        /// The ASCII (plain text) emulation type
        /// </summary>
        ASCII,

        /// <summary>
        /// The ANSI (coloured) emulation type
        /// </summary>
        ANSI
    }

    /// <summary>
    /// The available locations a key could have been pressed at
    /// </summary>
    public enum DoorKeyLocation
    {
        /// <summary>
        /// A key has not yet been pressed
        /// </summary>
        None,

        /// <summary>
        /// The key was pressed on the local keyboard
        /// </summary>
        Local,

        /// <summary>
        /// The key was pressed on the remote terminal
        /// </summary>
        Remote
    }


    /// <summary>
    /// When a dropfile is read there is some useless information so it is not
    /// necessary to store the whole thing in memory.  Instead only certain
    /// parts are saved to this record
    /// 
    /// Supported Dropfiles
    /// D = Found In DOOR32.SYS
    /// I = Found In INFO.*
    /// </summary>
    public class TDropInfo
    {
        /// <summary>
        /// User's access level.
        /// </summary>
        public int Access = 0;

        /// <summary>
        /// User's alias.
        /// </summary>
        public string Alias = "Alias";

        /// <summary>
        /// Connection baud rate.
        /// </summary>
        public int Baud = 0;

        /// <summary>
        /// Com type (0=local, 1=serial, 2=telnet, 3=rlogin, 4=websocket)
        /// </summary>
        public int ComType = 0;

        /// <summary>
        /// User's emulation (ANSI or ASCII)
        /// </summary>
        public DoorEmulationType Emulation = DoorEmulationType.ANSI;

        /// <summary>
        /// Total seconds user has this session.
        /// </summary>
        public int MaxTime = 3600;

        /// <summary>
        /// Node number.
        /// </summary>
        public int Node = 0;

        /// <summary>
        /// User's real name.
        /// </summary>
        public string RealName = "Real Name";

        /// <summary>
        /// User's user file record position (1 based)
        /// </summary>
        public int RecPos = -1;

        /// <summary>
        /// Socket handle. 
        /// </summary>
        public int SocketHandle = -1;
    }

    /// <summary>
    /// Information about the last key pressed is stored in this record.
    /// This should be considered read-only.
    /// </summary>
    public class TLastKey
    {
        /// <summary>
        /// Character code for last key that was pressed
        /// </summary>
        public char Ch = '\0';

        /// <summary>
        /// Which side pressed the last key (LOCAL or REMOTE)
        /// </summary>
        public DoorKeyLocation Location = DoorKeyLocation.None;

        /// <summary>
        /// The time the last key was pressed
        /// </summary>
        public DateTime Time = DateTime.Now;
    }

    /// <summary>
    /// MORE prompts will use these two lines based on whether use has ANSI or ASCII
    /// </summary>
    public class TMOREPrompt
    {
        /// <summary>
        /// The ANSI prompt to use for the More() method
        /// </summary>
        public string ANSI = "|07 |0A<|02MORE|0A>";

        /// <summary>
        /// The visible length of the ANSI prompt (needed to ensure it gets erased correctly)
        /// </summary>
        public int ANSILength = 7;

        /// <summary>
        /// The ASCII prompt to use for the More() method
        /// </summary>
        public string ASCII = " <MORE>";
    }

    /// <summary>
    /// Information about the current session is stored in this record.
    /// </summary>
    public class TSession
    {
        /// <summary>
        /// Run the idle warning and timeout events?
        /// </summary>
        public bool DoIdleCheck = true;

        /// <summary>
        /// Run the various door related events?  (ie time warning, time up, update status bar, etc)
        /// </summary>
        public bool Events = true;

        /// <summary>
        /// The time the events last ran (used to ensure events only fire once per second)
        /// </summary>
        public DateTime EventsTime = DateTime.MinValue;

        /// <summary>
        /// The maximum amount of seconds a user can idle before they're booted
        /// </summary>
        public int MaxIdle = 300;

        /// <summary>
        /// The time the door was launched
        /// </summary>
        public DateTime TimeOn = DateTime.Now;
    }
}

