/*
  RMLib: Nonvisual support classes used by multiple R&M Software programs
  Copyright (C) Rick Parrish, R&M Software

  This file is part of RMLib.

  RMLib is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  any later version.

  RMLib is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
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
    public class IPCSocketClientThread : RMThread
    {
        public event EventHandler<StringEventArgs> ServerMessageEvent = null;
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<ExceptionEventArgs> ExceptionEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;

        private StringBuilder _Buffer = new StringBuilder();
        private TcpConnection _ClientConnection = null;
        private string _RemoteIP;
        private int _RemotePort;


        public IPCSocketClientThread(TcpConnection clientConnection)
        {
            _ClientConnection = clientConnection;
            _RemoteIP = clientConnection.GetRemoteIP();
            _RemotePort = clientConnection.GetRemotePort();
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")]
        protected override void Execute()
        {
            while (!_Stop)
            {

            Reconnected:

                try
                {
                    while ((!_Stop) && (_ClientConnection.Connected))
                    {
                        // Check for server message
                        if (_ClientConnection.CanRead(1000))
                        {
                            ParseServerMessages(_ClientConnection.ReadString());
                        }
                    }
                }
                catch (Exception ex)
                {
                    RaiseExceptionEvent("Error in IPCSocketClientThread::Execute() while communicating", ex);
                }

                if (_ClientConnection.Connected) _ClientConnection.Close();

                while (!_Stop)
                {
                    try
                    {
                        RaiseMessageEvent("IPSocketClient trying to reconnect...");
                        if (_ClientConnection.Connect(_RemoteIP, _RemotePort))
                        {
                            _ClientConnection.WriteLn("OK?");
                            string Response = _ClientConnection.ReadLn("\r\n", false, '\0', 1000);
                            if (Response == "OK!")
                            {
                                RaiseMessageEvent("IPSocketClient reconnected");
                                goto Reconnected;
                            }
                        }
                        else
                        {
                            RaiseMessageEvent("IPSocketClient failed to reconnect, trying again in 10 seconds...");
                        }
                    }
                    catch (Exception ex)
                    {
                        RaiseExceptionEvent("Error in IPCSocketClientThread::Execute() while connecting", ex);
                    }

                    for (int i = 0; i < 10; i++)
                    {
                        if (!_Stop) break;
                        Thread.Sleep(1000);
                    }
                }
            }
        }

        public static IPCSocketClientThread GetNewClient(string remoteAddress, int remotePort)
        {
            TcpConnection ClientConnection = new TcpConnection();
            if (ClientConnection.Connect(remoteAddress, remotePort))
            {
                ClientConnection.WriteLn("OK?");
                string Response = ClientConnection.ReadLn("\r\n", false, '\0', 1000);
                if (Response == "OK!")
                {
                    IPCSocketClientThread Result = new IPCSocketClientThread(ClientConnection);
                    Result.Start();
                    return Result;
                }
            }

            return null;
        }

        private void ParseServerMessages(string data)
        {
            for (int i = 0; i < data.Length; i++)
            {
                if (data[i] == IPCSocketServerThread.EndStatement)
                {
                    RaiseServerMessageEvent(_Buffer.ToString());
                    _Buffer.Length = 0;
                }
                else
                {
                    _Buffer.Append(data[i]);
                }
            }
        }

        private void RaiseErrorMessageEvent(string message)
        {
            ErrorMessageEvent?.Invoke(this, new StringEventArgs(message));
        }

        private void RaiseExceptionEvent(string message, Exception exception)
        {
            ExceptionEvent?.Invoke(this, new ExceptionEventArgs(message, exception));
        }

        private void RaiseMessageEvent(string message)
        {
            MessageEvent?.Invoke(this, new StringEventArgs(message));
        }

        private void RaiseServerMessageEvent(string message)
        {
            ServerMessageEvent?.Invoke(this, new StringEventArgs(message));
        }

        public void SendCommand(string command)
        {
            _ClientConnection.Write(command + IPCSocketServerThread.EndStatement);
        }
    }
}
