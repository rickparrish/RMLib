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
using RandM.RMLib;
using System.Globalization;
using System.Threading;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Net;
using System.Text;

namespace RandM.RMLib
{
    public class IPCSocketServerThread : RMThread
    {
        public const char EndStatement = '\xFF';

        public event EventHandler<StringEventArgs> ClientCommandEvent = null;
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<ExceptionEventArgs> ExceptionEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;

        private StringBuilder _Buffer = new StringBuilder();
        private TcpConnection _ClientConnection = null;
        private TcpConnection _Listener = null;


        public IPCSocketServerThread(TcpConnection serverConnection)
        {
            _Listener = serverConnection;
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")]
        protected override void Execute()
        {
            while (!_Stop)
            {
                // Accept an incoming control client connection
                if (_Listener.CanAccept(1000)) // 1 second
                {
                    try
                    {
                        RaiseMessageEvent("IPCSocketServer about to accept a client connection");
                        _ClientConnection = _Listener.AcceptTCP();
                        if (_ClientConnection != null)
                        {
                            string Request = _ClientConnection.ReadLn("\r\n", false, '\0', 1000);
                            if (Request == "OK?")
                            {
                                _ClientConnection.WriteLn("OK!");

                                RaiseMessageEvent("Control server accepted a client connection from " + _ClientConnection.GetRemoteIP() + "," + _ClientConnection.GetRemotePort());

                                _Buffer.Length = 0;
                                while ((!_Stop) && (_ClientConnection.Connected))
                                {
                                    if (_ClientConnection.CanRead(1000))
                                    {
                                        ParseClientCommands(_ClientConnection.ReadString());
                                    }
                                }
                            }

                            if (_ClientConnection.Connected) _ClientConnection.Close();
                        }
                        else
                        {
                            RaiseErrorMessageEvent("IPCSocketServer failed to accept a client connection");
                        }
                    }
                    catch (Exception ex)
                    {
                        RaiseExceptionEvent("Error in IPCSocketServerThread::Execute()", ex);
                    }
                }
            }
        }

        static public IPCSocketServerThread GetNewServer(string localAddress, int localPort)
        {
            TcpConnection ServerConnection = new TcpConnection();
            if (ServerConnection.Listen(localAddress, localPort))
            {
                IPCSocketServerThread Result = new IPCSocketServerThread(ServerConnection);
                Result.Start();
                return Result;
            }
            else
            {
                return null;
            }
        }

        private void ParseClientCommands(string data)
        {
            for (int i = 0; i < data.Length; i++)
            {
                if (data[i] == IPCSocketServerThread.EndStatement)
                {
                    RaiseClientCommandEvent(_Buffer.ToString());
                    _Buffer.Length = 0;
                }
                else
                {
                    _Buffer.Append(data[i]);
                }
            }
        }

        private void RaiseClientCommandEvent(string command)
        {
            EventHandler<StringEventArgs> Handler = ClientCommandEvent;
            if (Handler != null) Handler(this, new StringEventArgs(command));
        }

        private void RaiseErrorMessageEvent(string message)
        {
            EventHandler<StringEventArgs> Handler = ErrorMessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs(message));
        }

        private void RaiseExceptionEvent(string message, Exception exception)
        {
            EventHandler<ExceptionEventArgs> Handler = ExceptionEvent;
            if (Handler != null) Handler(this, new ExceptionEventArgs(message, exception));
        }

        private void RaiseMessageEvent(string message)
        {
            EventHandler<StringEventArgs> Handler = MessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs(message));
        }

        public void SendMessage(string command)
        {
            _ClientConnection.Write(command.Replace(IPCSocketServerThread.EndStatement.ToString(), "") + IPCSocketServerThread.EndStatement);
        }
    }
}
