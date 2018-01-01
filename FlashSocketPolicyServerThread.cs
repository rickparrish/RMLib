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
using System.Globalization;
using System;

namespace RandM.RMLib
{
    public class FlashSocketPolicyServerThread : RMThread
    {
        private string _Address;
        private string _AllowedPorts;
        private TcpConnection _Connection;
        private int _Port;

        public event EventHandler BindFailedEvent = null;
        public event EventHandler BoundEvent = null;
        public event EventHandler<ConnectionAcceptedEventArgs> ConnectionAcceptedEvent = null;
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;
        public event EventHandler<StringEventArgs> WarningMessageEvent = null;

        public FlashSocketPolicyServerThread(string address, int port, string allowedPorts)
        {
            _Address = address;
            _Port = port;
            _AllowedPorts = allowedPorts;
        }

        protected override void Dispose(bool disposing)
        {
            if (!_Disposed)
            {
                if (disposing)
                {
                    // dispose managed state (managed objects).
                    _Connection.Dispose();
                }

                // free unmanaged resources (unmanaged objects)
                // set large fields to null.

                // Call the base dispose
                base.Dispose(disposing);
            }
        }

        protected override void Execute()
        {
            _Connection = new TcpConnection();
            if (_Connection.Listen(_Address, _Port))
            {
                RaiseBoundEvent();

                while (!_Stop)
                {
                    // Accept a new connection
                    if (_Connection.CanAccept(500)) // 1/2 of a second
                    {
                        TcpConnection NewConnection = _Connection.AcceptTCP();
                        if (NewConnection != null)
                        {
                            // Wait up to 5 seconds for the request string
                            string Request = NewConnection.ReadLn("\0", 5000);
                            if (Request.ToLower().Replace(" ", "") == "<policy-file-request/>")
                            {
                                ConnectionAcceptedEventArgs.Raise(this, ConnectionAcceptedEvent, _Address, _Port, NewConnection.GetRemoteIP(), NewConnection.GetRemotePort());
                                RaiseMessageEvent("Answered policy file request from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                                
                                NewConnection.WriteLn("<?xml version=\"1.0\"?>");
                                NewConnection.WriteLn("<cross-domain-policy>");
                                NewConnection.WriteLn("   <allow-access-from domain=\"*\" to-ports=\"" + _AllowedPorts + "\"/>");
                                NewConnection.WriteLn("   <site-control permitted-cross-domain-policies=\"all\"/>"); // TODO Maybe add property to determine whether this should be all or master-only
                                NewConnection.WriteLn("</cross-domain-policy>");
                                NewConnection.Write("\0");
                            }
                            else
                            {
                                RaiseErrorMessageEvent("Invalid policy file request from " + NewConnection.GetRemoteIP() + ":" + NewConnection.GetRemotePort().ToString());
                            }
                            NewConnection.Close();
                        }
                    }
                }
                _Connection.Close();
            }
            else
            {
                RaiseErrorMessageEvent("Flash Socket Policy Thread: Unable to listen on " + _Address + ":" + _Port);
                RaiseBindFailedEvent();
            }
        }

        private void RaiseBindFailedEvent()
        {
            EventHandler Handler = BindFailedEvent;
            if (Handler != null) Handler(this, EventArgs.Empty);
        }

        private void RaiseBoundEvent()
        {
            EventHandler Handler = BoundEvent;
            if (Handler != null) Handler(this, EventArgs.Empty);
        }
        
        private void RaiseErrorMessageEvent(string text)
        {
            EventHandler<StringEventArgs> Handler = ErrorMessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs(text));
        }

        private void RaiseMessageEvent(string text)
        {
            EventHandler<StringEventArgs> Handler = MessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs(text));
        }

        private void RaiseWarningMessageEvent(string text)
        {
            EventHandler<StringEventArgs> Handler = WarningMessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs(text));
        }

        public override void Stop()
        {
            // Close the socket so that any waits on ReadLn(), ReadChar(), etc, will not block
            _Connection.Close();

            base.Stop();
        }
    }
}
