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
using System.IO;
using System.Text;
using System.Threading;
using System.Globalization;

namespace RandM.RMLib
{
    /// <summary>
    /// Please note that the Door class is not thread safe.  If:
    ///   1) Anybody every uses this class, and
    ///   2) A use-case for a thread safe Door class is presented
    /// then I'll be happy to add the required locking to make it thread safe.
    /// </summary>
    static public class Door
    {
        public const string _Version = "R&M Door v13.02.19";

        static public TDropInfo DropInfo = new TDropInfo();
        static public TLastKey LastKey = new TLastKey();
        static public TMOREPrompts MOREPrompts = new TMOREPrompts();
        static public TSession Session = new TSession();

        static private TcpConnection _Connection;

        #region Standard R&M Door functions

        static Door()
        {
            DropInfo.Access = -1;
            DropInfo.Alias = "";
            DropInfo.Baud = -1;
            DropInfo.Clean = false;
            DropInfo.ComType = 2;
            DropInfo.Emulation = DoorEmulationType.ANSI;
            DropInfo.Fairy = false;
            DropInfo.MaxTime = 3600;
            DropInfo.Node = -1;
            DropInfo.RealName = "";
            DropInfo.RecPos = -1;
            DropInfo.Registered = false;
            DropInfo.SocketHandle = -1;

            LastKey.Ch = '\0';
            LastKey.Extended = false;
            LastKey.Location = DoorKeyLocation.None;
            LastKey.Time = DateTime.Now;

            MOREPrompts.ANSI = "|07 |0A<|02MORE|0A>";
            MOREPrompts.ANSILength = 7;
            MOREPrompts.ASCII = " <MORE>";

            Session.DoIdleCheck = false;
            Session.Events = false;
            Session.EventsTime = DateTime.Now;
            Session.MaxIdle = 300;
            Session.TimeOn = DateTime.Now;

            LocalEcho = false;
        }

        /// <summary>
        /// Checks for a carrier.  Always returns true for local sessions
        /// </summary>
        /// <returns>True if local or carrier exists, false if no carrier exists</returns>
        static public bool Carrier
        {
            get { return (Local() || _Connection.Connected); }
        }

        /// <summary>
        /// Clears the local and remote input buffers
        /// </summary>
        static public void ClearBuffers()
        {
            while (Crt.KeyPressed())
                Crt.ReadKey();

            if (!Local())
                _Connection.ReadString();
        }

        /// <summary>
        /// Closes the socket connection, which will disconnect the remote user
        /// </summary>
        static public void Disconnect()
        {
            if (!Local())
            {
                _Connection.ShutdownOnClose = true;
                _Connection.Close();
            }

            DropInfo.SocketHandle = -1;
        }

        /// <summary>
        /// Clears all text to the end of the line
        /// </summary>
        static public void ClrEol()
        {
            Write(Ansi.ClrEol());
        }

        /// <summary>
        /// Clears all text on the screen
        /// </summary>
        static public void ClrScr()
        {
            Write(Ansi.ClrScr());
        }

        /// <summary>
        /// Moves the cursor down the screen
        /// </summary>
        /// <param name="count">The number of lines to move the cursor down</param>
        static public void CursorDown(int count)
        {
            Write(Ansi.CursorDown(count));
        }

        /// <summary>
        /// Moves the cursor to the left on the screen
        /// </summary>
        /// <param name="count">The number of columns to move the cursor left</param>
        static public void CursorLeft(int count)
        {
            Write(Ansi.CursorLeft(count));
        }

        /// <summary>
        /// Restores the previously saved cursor position
        /// </summary>
        /// <seealso cref="CursorSave"/>
        static public void CursorRestore()
        {
            Write(Ansi.CursorRestore());
        }

        /// <summary>
        /// Moves the cursor to the right on the screen
        /// </summary>
        /// <param name="count">The number of columns to move the cursor right</param>
        static public void CursorRight(int count)
        {
            Write(Ansi.CursorRight(count));
        }

        /// <summary>
        /// Saves the current cursor position
        /// </summary>
        /// <seealso cref="CursorRestore"/>
        static public void CursorSave()
        {
            Write(Ansi.CursorSave());
        }

        /// <summary>
        /// Moves the cursor up the screen
        /// </summary>
        /// <param name="count">The number of rows to move the cursor up</param>
        static public void CursorUp(int count)
        {
            Write(Ansi.CursorUp(count));
        }

        /// <summary>
        /// Displays a file (ANSI, ASCII, Text) on screen, optionally pausing every x lines
        /// </summary>
        /// <param name="fileName">The file to display</param>
        /// <param name="linesBeforePause">The number of lines to display before pausing.  0 causes no pauses</param>
        static public void DisplayFile(string fileName, int linesBeforePause)
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

        /*
        KeyPressed calls this procedure every time it is run.  This is where
        a lot of the "behind the scenes" stuff happens, such as determining how
        much time the user has left, if theyve dropped carrier, and updating the
        status bar.
        It is not recommended that you mess with anything in this procedure
        */
        static public void DoEvents()
        {
            TimeSpan Dif = DateTime.Now.Subtract(Session.EventsTime);
            if ((Session.Events) && (Dif.TotalMilliseconds > 1000))
            {
                //Check For Hangup
                if ((!Carrier) && (OnHangUp != null))
                {
                    OnHangUp();
                }

                // Check For Idle Timeout
                if ((Session.DoIdleCheck) && (TimeIdle() > Session.MaxIdle) && (OnTimeOut != null))
                {
                    OnTimeOut();
                }

                // Check For Idle Timeout Warning
                if ((Session.DoIdleCheck) && ((Session.MaxIdle - TimeIdle()) % 60 == 1) && ((Session.MaxIdle - TimeIdle()) / 60 <= 5) && (OnTimeOutWarning != null))
                {
                    OnTimeOutWarning((int)(Session.MaxIdle - TimeIdle()) / 60);
                }

                // Check For Time Up
                if ((TimeLeft() < 1) && (OnTimeUp != null))
                {
                    OnTimeUp();
                }

                // Check For Time Up Warning
                if ((TimeLeft() % 60 == 1) && (TimeLeft() / 60 <= 5) && (OnTimeUpWarning != null))
                {
                    OnTimeUpWarning((int)(TimeLeft() / 60));
                }

                // Update Status Bar
                if (OnStatusBar != null)
                {
                    OnStatusBar();
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
        static public void DrawBox(int left, int top, int right, int bottom, int foregroundColour, int backgroundColour, CrtPanel.BorderStyle borderStyle)
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

        static public void GotoX(int column)
        {
            Write(Ansi.GotoX(column));
        }

        static public void GotoXY(int column, int row)
        {
            Write(Ansi.GotoXY(column, row));
        }

        static public void GotoY(int row)
        {
            Write(Ansi.GotoY(row));
        }

        static public string Input(string defaultText, string allowedCharacters, char passwordCharacter, int numberOfCharactersToDisplay, int maximumLength, int attribute)
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

        static public bool KeyPressed()
        {
            DoEvents();

            if (Local())
            {
                return Crt.KeyPressed();
            }
            else
            {
                return (Crt.KeyPressed() || _Connection.CanRead());
            }
        }

        static public bool Local()
        {
            return (DropInfo.SocketHandle == -1);
        }

        static public bool LocalEcho { get; set; }

        static public void More()
        {
            string Line = "";
            int LineLength = 0;

            switch (DropInfo.Emulation)
            {
                case DoorEmulationType.ASCII:
                    Line = MOREPrompts.ASCII;
                    LineLength = MOREPrompts.ASCII.Length;
                    break;

                case DoorEmulationType.ANSI:
                    Line = MOREPrompts.ANSI;
                    LineLength = MOREPrompts.ANSILength;
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

        static public bool Open()
        {
            if (Local())
            {
                return true;
            }
            else
            {
                switch (DropInfo.ComType)
                {
                    case 2: _Connection = new TelnetConnection(); break;
                    case 3: _Connection = new RLoginConnection(); break;
                    case 4: _Connection = new WebSocketConnection(false); break;
                }
                _Connection.Open(DropInfo.SocketHandle);
                _Connection.ShutdownOnClose = false;
                _Connection.StripLF = true;

                return _Connection.Connected;
            }
        }

        static private string PipeToAnsi(string AText)
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

        static public char? ReadKey()
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
                        if ((!Local()) && (OnSysOpKey != null) && (!OnSysOpKey((char)Ch)))
                        {
                            LastKey.Extended = true;
                            LastKey.Location = DoorKeyLocation.Local;
                        }
                    }
                    else
                    {
                        LastKey.Extended = false;
                        LastKey.Location = DoorKeyLocation.Local;
                    }
                }
                else if ((!Local()) && (_Connection.CanRead()))
                {
                    Ch = _Connection.ReadChar(0);
                    LastKey.Extended = false;
                    LastKey.Location = DoorKeyLocation.Remote;
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

        static public string ReadLn()
        {
            return Input("", CharacterMask.All, '\0', 50, 50, 7);
        }

        static public void Shutdown()
        {
            if (!Local()) _Connection.Close();
        }

        static public void Startup(string[] args)
        {
            string DropFile = "";
            bool Local = false;
            int Node = 0;
            int Socket = -1;

            for (int i = 0; i < args.Length; i++)
            {
                string S = args[i];
                if ((S.Length >= 2) && ((S[0] == '/') || (S[0] == '-')))
                {
                    char Ch = S.ToUpper()[1];
                    S = S.Substring(2);

                    switch (Ch)
                    {
                        case 'C':
                            if (!int.TryParse(S, out DropInfo.ComType)) DropInfo.ComType = 0;
                            break;
                        case 'D':
                            DropFile = S.ToUpper();
                            break;
                        case 'H':
                            if (!int.TryParse(S, out Socket)) Socket = -1;
                            break;
                        case 'L':
                            Local = true;
                            break;
                        case 'N':
                            if (!int.TryParse(S, out Node)) Node = 0;
                            break;
                        default:
                            if (OnCLP != null)
                            {
                                EventHandler<CommandLineParameterEventArgs> Handler = OnCLP;
                                if (Handler != null) Handler(null, new CommandLineParameterEventArgs(Ch, S));
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
                    OnLocalLogin();
                    ClrScr();
                }
            }
            else if ((Socket > 0) && (Node > 0))
            {
                DropInfo.SocketHandle = Socket;
                DropInfo.Node = Node;
            }
            else if (!string.IsNullOrEmpty(DropFile ))
            {
                int SleepLoops = 0;
                while ((SleepLoops++ < 5) && (!File.Exists(DropFile)))
                {
                    Thread.Sleep(1000);
                }

                if (File.Exists(DropFile))
                {
                    if (DropFile.IndexOf("DOOR32.SYS") != -1)
                    {
                        ReadDoor32(DropFile);
                    }
                    else if (DropFile.IndexOf("INFO.") != -1)
                    {
                        ReadInfo(DropFile);
                    }
                    else
                    {
                        ClrScr();
                        WriteLn();
                        WriteLn("  Drop File Not Fount");
                        WriteLn();
                        Thread.Sleep(2500);
                        Environment.Exit(0);
                    }

                }
            }
            else if (OnUsage != null)
            {
                OnUsage();
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
                    Environment.Exit(0);
                }

                LastKey.Time = DateTime.Now;
                Session.Events = true;
                Session.EventsTime = DateTime.Now.AddSeconds(-1);
                Session.TimeOn = DateTime.Now;

                ClrScr();
            }
        }

        static public bool StripLF
        {
            set { if (!Local()) _Connection.StripLF = value; }
        }

        static public void SysopChat()
        {
            char? Ch = null;
            DoorKeyLocation OurLastKeyLocation = DoorKeyLocation.None;

            do
            {
                if (Door.KeyPressed())
                {
                    Ch = Door.ReadKey();

                    if ((Ch >= 32) && (Ch <= 126))
                    {
                        if (OurLastKeyLocation != Door.LastKey.Location)
                        {
                            switch (Door.LastKey.Location)
                            {
                                case DoorKeyLocation.Local:
                                    Door.TextColor((int)ConsoleColor.Green);
                                    break;
                                case DoorKeyLocation.Remote:
                                    Door.TextColor((int)ConsoleColor.Red);
                                    break;
                            }
                            OurLastKeyLocation = Door.LastKey.Location;
                        }

                        Door.Write(Ch.ToString());
                    }
                    else if (Ch == '\x0D')
                    {
                        Door.WriteLn();
                    }
                }
            } while (Ch != '\x1B');
        }

        static public void TextAttr(int attribute)
        {
            Write(Ansi.TextAttr(attribute));
        }

        static public void TextBackground(int colour)
        {
            Write(Ansi.TextBackground(colour));
        }

        static public void TextColor(int colour)
        {
            Write(Ansi.TextColor(colour));
        }

        static public int TimeIdle()
        {
            TimeSpan Dif = DateTime.Now.Subtract(LastKey.Time);
            return (int)Dif.TotalSeconds;
        }

        static public int TimeLeft()
        {
            return (DropInfo.MaxTime - TimeOn());
        }

        static public int TimeOn()
        {
            TimeSpan Dif = DateTime.Now.Subtract(Session.TimeOn);
            return (int)Dif.TotalSeconds;
        }

        static public void Write(string text)
        {
            if (text.Contains("|")) text = PipeToAnsi(text);

            Ansi.Write(text);
            if (!Local()) _Connection.Write(text);
        }

        /// <summary>
        /// Advances the cursor to the beginning of the next line, scrolling the screen if necessary
        /// </summary>
        static public void WriteLn()
        {
            Write("\r\n");
        }

        /// <summary>
        /// Outputs a string of text to the screen before advancing the cursor to the beginning of the next line, scrolling if necessary
        /// </summary>
        /// <param name="text">The text to be displayed</param>
        static public void WriteLn(string text)
        {
            Write(text + "\r\n");
        }

        static private void ReadDoor32(string AFile)
        {
            if (File.Exists(AFile))
            {
                string[] Lines = FileUtils.FileReadAllLines(AFile);

                int.TryParse(Lines[0], out DropInfo.ComType); // 1 - Comm type (0=local, 1=serial, 2=telnet, 3=rlogin, 4=websocket)
                int.TryParse(Lines[1], out DropInfo.SocketHandle); // 2 - Comm or socket handle
                int.TryParse(Lines[2], out DropInfo.Baud); // 3 - Baud rate
                // 4 - BBSID (software name and version)
                int.TryParse(Lines[4], out DropInfo.RecPos); // 5 - User record position (1-based)
                DropInfo.RecPos -= 1;
                DropInfo.RealName = Lines[5]; // 6 - User's real name
                DropInfo.Alias = Lines[6]; // 7 - User's handle/alias
                int.TryParse(Lines[7], out DropInfo.Access); // 8 - User's security level
                int.TryParse(Lines[8], out DropInfo.MaxTime); // 9 - User's time left (in minutes)
                DropInfo.MaxTime *= 60;
                DropInfo.Emulation = (Lines[9] == "0") ? DoorEmulationType.ASCII : DoorEmulationType.ANSI; // 10 - Emulation (0=Ascii, 1=Ansi, 2=Avatar, 3=RIP, 4=MaxGfx)
                int.TryParse(Lines[10], out DropInfo.Node); // 11 - Current node number
                if (Lines.Length >= 12) DropInfo.SocketInformationFile = Lines[11]; // 12 - SocketInformation File
            }
        }

        static private void ReadInfo(string AFile)
        {
            if (File.Exists(AFile))
            {
                string[] Lines = FileUtils.FileReadAllLines(AFile);

                int.TryParse(Lines[0], out DropInfo.RecPos); // 1 - Account Number (0 Based)}
                DropInfo.Emulation = (Lines[1] == "3") ? DoorEmulationType.ANSI : DoorEmulationType.ASCII; // 2 - Emulation (3=Ansi, Other = Ascii)}
                // 3 - RIP YES or RIP NO}
                DropInfo.Fairy = (Lines[3].ToUpper().Trim() == "FAIRY YES") ? true : false; // 4 - FAIRY YES or FAIRY NO}
                int.TryParse(Lines[4], out DropInfo.MaxTime); // 5 - User's Time Left (In Minutes)}
                DropInfo.MaxTime *= 60;
                DropInfo.Alias = Lines[5]; // 6 - User's Handle/Alias}
                DropInfo.RealName = Lines[6]; // 7 - User's First Name}
                if (!string.IsNullOrEmpty(Lines[7].Trim())) DropInfo.RealName += Lines[7]; // 8 - User's Last Name}
                int.TryParse(Lines[8], out DropInfo.SocketHandle); // 9 - Comm Port}
                int.TryParse(Lines[9], out DropInfo.Baud); // 10 - Caller Baud Rate}
                // 11 - Port Baud Rate}
                // 12 - FOSSIL or INTERNAL or TELNET or WC5
                DropInfo.Registered = (Lines[12].ToUpper().Trim() == "REGISTERED") ? true : false; // 13 - REGISTERED or UNREGISTERED}
                DropInfo.Clean = (Lines[13].ToUpper().Trim() == "CLEAN MODE ON") ? true : false; // 14 - CLEAN MODE ON or CLEAN MODE OFF}
            }
        }

        #endregion

        #region Event handlers
        // It is not recommended you change these, instead
        // you should just reassign the above On* variables
        // to your own procedures.

        static public event EventHandler<CommandLineParameterEventArgs> OnCLP = null;

        public delegate void OnHangUpCallback();
        static public OnHangUpCallback OnHangUp = new OnHangUpCallback(DefaultOnHangUp);
        static private void DefaultOnHangUp()
        {
            TextAttr(15);
            ClrScr();
            WriteLn();
            WriteLn("   Caller Dropped Carrier.  Returning To BBS...");
            Thread.Sleep(2500);
            Environment.Exit(0);
        }

        public delegate void OnLocalLoginCallback();
        static public OnLocalLoginCallback OnLocalLogin = new OnLocalLoginCallback(DefaultOnLocalLogin);
        static private void DefaultOnLocalLogin()
        {
            ClrScr();
            DrawBox(2, 2, 18, 6, Crt.White, Crt.Blue, CrtPanel.BorderStyle.Double);
            GotoXY(5, 4);
            Write("|1FLOCAL LOGIN|07");

            GotoXY(2, 8);
            Write("Enter your name : ");
            string S = Input("SYSOP", CharacterMask.AlphanumericWithSpace, '\0', 40, 40, 31);
            DropInfo.RealName = S;
            DropInfo.Alias = S;
        }

        public delegate void OnStatusBarCallback();
        static public OnStatusBarCallback OnStatusBar = new OnStatusBarCallback(DefaultOnStatusBar);
        static private void DefaultOnStatusBar()
        {
            Crt.FastWrite("þ                           þ                   þ             þ                þ", 1, 25, 30);
            Crt.FastWrite((DropInfo.RealName + new string(' ', 22)).Substring(0, 22), 3, 25, 31);
            Crt.FastWrite(_Version, 31, 25, 31);
            Crt.FastWrite(("Idle: " + StringUtils.SecToMS(TimeIdle()) + "s" + new string(' ', 11)).Substring(0, 11), 51, 25, 31);
            Crt.FastWrite("Left: " + StringUtils.SecToHMS(TimeLeft()) + "s", 65, 25, 31);
        }

        public delegate bool OnSysOpKeyCallback(char AKey);
        static public OnSysOpKeyCallback OnSysOpKey = null;

        public delegate void OnTimeOutCallback();
        static public OnTimeOutCallback OnTimeOut = new OnTimeOutCallback(DefaultOnTimeOut);
        static private void DefaultOnTimeOut()
        {
            TextAttr(15);
            ClrScr();
            WriteLn();
            WriteLn("   Idle Time Limit Exceeded.  Returning To BBS...");
            Thread.Sleep(2500);
            Environment.Exit(0);
        }

        public delegate void OnTimeOutWarningCallback(int AMinutesLeft);
        static public OnTimeOutWarningCallback OnTimeOutWarning = null;

        public delegate void OnTimeUpCallback();
        static public OnTimeUpCallback OnTimeUp = new OnTimeUpCallback(DefaultOnTimeUp);
        static private void DefaultOnTimeUp()
        {
            TextAttr(15);
            ClrScr();
            WriteLn();
            WriteLn("   Your Time Has Expired.  Returning To BBS...");
            Thread.Sleep(2500);
            Environment.Exit(0);
        }

        public delegate void OnTimeUpWarningCallback(int AMinutesLeft);
        static public OnTimeUpWarningCallback OnTimeUpWarning = null;

        public delegate void OnUsageCallback();
        static public OnUsageCallback OnUsage = new OnUsageCallback(DefaultOnUsage);
        static private void DefaultOnUsage()
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
            Environment.Exit(0);
        }

        #endregion
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

    /*
    When a dropfile is read there is some useless information so it is not
    necessary to store the whole thing in memory.  Instead only certain
    parts are saved to this record

    Supported Dropfiles
    D = Found In DOOR32.SYS
    I = Found In INFO.*
    */
    public struct TDropInfo
    {
        public int Access;                  //{D-} {User's Access Level}
        public string Alias;                //{DI} {User's Alias/Handle}
        public int Baud;                    //{DI} {Connection Baud Rate}
        public bool Clean;                  //{-I} {Is LORD In Clean Mode?}
        public int ComType;                 //{D-} {2=telnet, 3=rlogin, 4=websocket}
        public DoorEmulationType Emulation; //{DI} {User's Emulation (eANSI or eASCII)}
        public bool Fairy;                  //{-I} {Does LORD User Have Fairy?}
        public int MaxTime;                 //{DI} {User's Time Left At Start (In Seconds)}
        public int Node;                    //{D-} {Node Number}
        public string RealName;             //{DI} {User's Real Name}
        public int RecPos;                  //{DI} {User's Userfile Record Position (Always 0 Based)}
        public bool Registered;             //{-I} {Is LORD Registered?}
        public int SocketHandle;            //{DI} {Comm/Socket Number}
        public string SocketInformationFile;//{D-} {SocketInformation File}
    }

    /*
    Information about the last key pressed is stored in this record.
    This should be considered read-only.
    */
    public struct TLastKey
    {
        public char Ch;                 //{ Character of last key }
        public bool Extended;           //{ Was character preceded by #0 }
        public DoorKeyLocation Location;   //{ Location of last key }
        public DateTime Time;           //{ SecToday of last key }
    }

    /*
    MORE prompts will use these two lines based on whether use has ANSI or ASCII
    */
    public struct TMOREPrompts
    {
        public string ANSI;         //{ Used by people with ANSI }
        public int ANSILength;      //{ ANSI may have non-displaying characters, we need to know the length of just the text }
        public string ASCII;        //{ Used by people with ASCII }
    }

    /*
    Information about the current session is stored in this record.
    */
    public struct TSession
    {
        public bool DoIdleCheck;        //{ Check for idle timeout? }
        public bool Events;             //{ Run Events in mKeyPressed function }
        public DateTime EventsTime;     //{ MSecToday of last Events run }
        public int MaxIdle;             //{ Max idle before kick (in seconds) }
        public DateTime TimeOn;         //{ SecToday program was started }
    }
}
