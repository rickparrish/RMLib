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
using System.Net.Sockets;
using System.Text;

namespace RandM.RMLib
{
    public class RLoginConnection : TcpConnection
    {
        // RLogin commands
        public const byte RLC_COOKIE = 0xFF; // RLogin "cookie" character
        public const byte RLC_S = 0x73; // 0x73="s" Screen size report character
        // RLogin states
        const byte RLS_DATA = 0x00; // In Data mode
        const byte RLS_COOKIE1 = 1;    // Received first "cookie" character
        const byte RLS_COOKIE2 = 2;    // Received second "cookie" character
        const byte RLS_S1 = 3;    // Received first "s" character
        const byte RLS_SS = 5;    // Received second "s" character.  Transmitting screensize info

        private bool _ExpectHeader = true;
        private Int32 _RLoginSSBytes = 0;
        private Int32 _RLoginState = RLS_DATA;

        public RLoginConnection() : this(true) { }

        public RLoginConnection(bool expectHeader)
            : base()
        {
            _ExpectHeader = expectHeader;
        }

        public string ClientUserName { get; private set; }

        protected override void InitSocket()
        {
            base.InitSocket();

            LineEnding = "\r";
            _RLoginSSBytes = 0;
            _RLoginState = RLS_DATA;
        }

        protected override void NegotiateInbound(byte[] data, int numberOfBytes)
        {
            for (int i = 0; i < numberOfBytes; i++)
            {
                if (RLS_DATA == _RLoginState)
                {
                    if (RLC_COOKIE == data[i])
                    {
                        _RLoginState = RLS_COOKIE1;
                    }
                    else
                    {
                        _InputBuffer.Enqueue(data[i]);
                    }
                }

                else if (RLS_COOKIE1 == _RLoginState)
                {
                    if (RLC_COOKIE == data[i])
                    {
                        _RLoginState = RLS_COOKIE2;
                    }
                    else
                    {
                        _RLoginState = RLS_DATA;
                    }
                }

                else if (RLS_COOKIE2 == _RLoginState)
                {
                    if (RLC_S == data[i])
                    {
                        _RLoginState = RLS_S1;
                    }
                    else
                    {
                        _RLoginState = RLS_DATA;
                    }
                }

                else if (RLS_S1 == _RLoginState)
                {
                    if (RLC_S == data[i])
                    {
                        _RLoginState = RLS_SS;
                    }
                    else
                    {
                        _RLoginState = RLS_DATA;
                    }
                }

                else if (RLS_SS == _RLoginState)
                {
                    _RLoginSSBytes += 1;
                    if (_RLoginSSBytes >= 8)
                    {
                        _RLoginSSBytes = 0;
                        _RLoginState = RLS_DATA;
                    }
                }
            }
        }

        public override bool Open(Socket socket)
        {
            if (base.Open(socket))
            {
                if (_ExpectHeader)
                {
                    return ParseHeader();
                }
                else
                {
                    return true;
                }
            }

            return false;
        }

        private bool ParseHeader()
        {
            // Get the 4 null terminated strings
            string NullByte = ReadLn("\0", 5000);
            if (ReadTimedOut) return false;

            ClientUserName = ReadLn("\0", 5000);
            if (string.IsNullOrEmpty(ClientUserName)) return false;
            
            ServerUserName = ReadLn("\0", 5000);
            if (string.IsNullOrEmpty(ServerUserName)) return false;
            
            TerminalType = ReadLn("\0", 5000);
            if (string.IsNullOrEmpty(TerminalType)) return false;

            Write("\0");

            return true;
        }

        public string ServerUserName { get; private set; }

        public string TerminalType { get; private set; }
    }
}
