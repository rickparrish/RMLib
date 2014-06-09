using System;
using System.Collections.Generic;
using System.Net.Sockets;
using System.Text;

namespace RandM.RMLib
{
    class RMSocket : IDisposable
    {
        private bool _Connected = false;
        private bool _Disposed = false;
        private byte[] _ReceiveBuffer = new byte[65536];

        public RMSocket(int socketHandle)
        {
            NativeMethods.WSAData WSA = new NativeMethods.WSAData();
            SocketError Result = NativeMethods.WSAStartup((short)0x0202, out WSA);
            if (Result != SocketError.Success) throw new SocketException(NativeMethods.WSAGetLastError());

            this.SocketHandle = new IntPtr(socketHandle);
        }

        ~RMSocket()
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

        public void Dispose(bool disposing)
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
                if (SocketHandle != IntPtr.Zero)
                {
                    Shutdown();
                    Close();
                }

                // Note disposing has been done.
                _Disposed = true;
            }
        }

        public int Available
        {
            get
            {
                if (SocketHandle == IntPtr.Zero) throw new ObjectDisposedException("SocketHandle");

                int Result = 0;
                int SocketResult = NativeMethods.ioctlsocket(SocketHandle, NativeMethods.Command.FIONREAD, ref Result);
                if ((SocketError)SocketResult == SocketError.SocketError)
                {
                    throw new SocketException(NativeMethods.WSAGetLastError());
                }
                else
                {
                    return Result;
                }
            }
        }

        public void Close()
        {
            if (SocketHandle != IntPtr.Zero)
            {
                NativeMethods.closesocket(SocketHandle);
                SocketHandle = IntPtr.Zero;
            }
        }

        public bool Connected
        {
            get
            {
                if (_Connected)
                {
                    // Send() will reset _Connected in the event of error (ie a disconnection)
                    Send(string.Empty);
                }

                return _Connected;
            }
        }

        public string Receive()
        {
            int BytesRead = Receive(_ReceiveBuffer);
            return RMEncoding.Ansi.GetString(_ReceiveBuffer, 0, BytesRead);
        }

        public int Receive(byte[] buffer)
        {
            if (SocketHandle == IntPtr.Zero) throw new ObjectDisposedException("SocketHandle");
            if (buffer == null) throw new ArgumentNullException("buffer");

            unsafe
            {
                fixed (byte* pData = _ReceiveBuffer)
                {
                    int BytesRead = NativeMethods.recv(SocketHandle, new IntPtr(pData), _ReceiveBuffer.Length, SocketFlags.None);
                    if ((SocketError)BytesRead == SocketError.SocketError)
                    {
                        throw new SocketException(NativeMethods.WSAGetLastError());
                    }
                    else if (BytesRead == 0)
                    {
                        Close();
                    }

                    return BytesRead;
                }
            }
        }

        public IntPtr SocketHandle { get; private set; }

        public void Shutdown()
        {
            NativeMethods.shutdown(SocketHandle, NativeMethods.ShutDownFlags.SD_BOTH);
            throw new NotImplementedException();
        }

        public int Send(string text)
        {
            if (SocketHandle == IntPtr.Zero) throw new ObjectDisposedException("SocketHandle");
            if (text == null) throw new ArgumentNullException("text");

            int TotalBytesSent = 0;

            unsafe
            {
                byte[] SendBuffer = RMEncoding.Ansi.GetBytes(text);
                fixed (byte* pData = SendBuffer)
                {
                    while (TotalBytesSent < SendBuffer.Length)
                    {
                        int ThisBytesSent = NativeMethods.send(SocketHandle, new IntPtr(pData), SendBuffer.Length, NativeMethods.MsgFlags.MSG_NONE);
                        if ((SocketError)ThisBytesSent == SocketError.SocketError)
                        {
                            throw new SocketException(NativeMethods.WSAGetLastError());
                        }
                        else if (ThisBytesSent == 0)
                        {
                            Close();
                            return TotalBytesSent;
                        }
                        else
                        {
                            TotalBytesSent += ThisBytesSent;
                        }
                    }

                    return TotalBytesSent;
                }
            }
        }
    }
}
