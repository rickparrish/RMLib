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
    public class IPCSocketClientThread : RMThread, IDisposable
    {
        public event EventHandler<StringEventArgs> ServerMessageEvent = null;
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<ExceptionEventArgs> ExceptionEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;

        private StringBuilder _Buffer = new StringBuilder();
        private bool _Disposed = false;
        private TcpConnection _ClientConnection = null;
        private string _RemoteIP;
        private int _RemotePort;


        public IPCSocketClientThread(TcpConnection clientConnection)
        {
            _ClientConnection = clientConnection;
            _RemoteIP = clientConnection.GetRemoteIP();
            _RemotePort = clientConnection.GetRemotePort();
        }

        ~IPCSocketClientThread()
        {
            Dispose(false);
        }

        public void Dispose()
        {
            Dispose(true);
            // This object will be cleaned up by the Dispose method.
            // Therefore, you should call GC.SupressFinalize to
            // take this object off the finalization queue
            // and prevent finalization code for this object
            // from executing a second time.
            GC.SuppressFinalize(this);
        }

        private void Dispose(bool disposing)
        {
            // Check to see if Dispose has already been called.
            if (!_Disposed)
            {
                // If disposing equals true, dispose all managed
                // and unmanaged resources.
                if (disposing)
                {
                    // Dispose managed resources.
                }

                // Call the appropriate methods to clean up
                // unmanaged resources here.
                // If disposing is false,
                // only the following code is executed.

                // Note disposing has been done.
                _Disposed = true;
            }
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
                            ParseServerMessages(_ClientConnection.ReadBytes());
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

        static public IPCSocketClientThread GetNewClient(string remoteAddress, int remotePort)
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

        private void ParseServerMessages(byte[] data)
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

        private void RaiseServerMessageEvent(string message)
        {
            EventHandler<StringEventArgs> Handler = ServerMessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs(message));
        }
    }
}
