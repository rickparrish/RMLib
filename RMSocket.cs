using System;
using System.Collections.Generic;
using System.Net.Sockets;
using System.Text;

namespace RandM.RMLib
{
    class RMSocket : IDisposable
    {
        private bool _Disposed = false;

        public RMSocket(int socketHandle)
        {
            NativeMethods.WSAData WSA = new NativeMethods.WSAData();
            SocketError Result = NativeMethods.WSAStartup((short)0x0202, out WSA);
            if (Result != SocketError.Success)
            {
                SocketHandle = IntPtr.Zero;
            }
            else
            {
                SocketHandle = new IntPtr(socketHandle);
            }
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
                Shutdown();
                Close();

                // Note disposing has been done.
                _Disposed = true;
            }
        }

        public void Close()
        {
            if (Connected)
            {
                NativeMethods.closesocket(SocketHandle);
                SocketHandle = IntPtr.Zero;
            }
        }

        public bool Connected
        {
            get
            {
                return (SocketHandle != IntPtr.Zero);
            }
        }

        public bool Poll(int microSeconds, SelectMode mode)
        {
            if (Connected)
            {
                IntPtr[] FDSet = new IntPtr[2] { (IntPtr)1, SocketHandle };

                int Result = 0;
                if (microSeconds == -1)
                {
                    // Wait indefinitely
                    Result = NativeMethods.select(
                                0,
                                mode == SelectMode.SelectRead ? FDSet : null,
                                mode == SelectMode.SelectWrite ? FDSet : null,
                                mode == SelectMode.SelectError ? FDSet : null,
                                IntPtr.Zero);
                }
                else
                {
                    // Wait the specified time
                    NativeMethods.TimeVal timeout = new NativeMethods.TimeVal();
                    timeout.Seconds = microSeconds / 1000000;
                    timeout.Microseconds = microSeconds % 1000000;

                    Result = NativeMethods.select(
                                0,
                                mode == SelectMode.SelectRead ? FDSet : null,
                                mode == SelectMode.SelectWrite ? FDSet : null,
                                mode == SelectMode.SelectError ? FDSet : null,
                                ref timeout);
                }

                if ((SocketError)Result == SocketError.SocketError)
                {
                    SocketHandle = IntPtr.Zero;
                    return false;
                }

                if ((int)FDSet[0] == 0)
                {
                    return false;
                }

                return (FDSet[1] == SocketHandle);
            }
            else
            {
                return false;
            }
        }

        public int Receive(byte[] buffer)
        {
            if (buffer == null) throw new ArgumentNullException("buffer");

            if (Connected)
            {
                unsafe
                {
                    fixed (byte* pData = buffer)
                    {
                        int BytesRead = NativeMethods.recv(SocketHandle, new IntPtr(pData), buffer.Length, SocketFlags.None);
                        if ((SocketError)BytesRead == SocketError.SocketError)
                        {
                            SocketHandle = IntPtr.Zero;
                            return 0;
                        }
                        else if (BytesRead == 0)
                        {
                            SocketHandle = IntPtr.Zero;
                            return 0;
                        }
                        else
                        {
                            return BytesRead;
                        }
                    }
                }
            }
            else
            {
                return 0;
            }
        }

        public IntPtr SocketHandle { get; private set; }

        public void Shutdown()
        {
            if (Connected)
            {
                NativeMethods.shutdown(SocketHandle, NativeMethods.ShutDownFlags.SD_BOTH);
            }
        }

        public int Send(string text)
        {
            if (text == null) throw new ArgumentNullException("text");

            if (Connected)
            {
                int TotalBytesSent = 0;
                byte[] SendBuffer = RMEncoding.Ansi.GetBytes(text);

                unsafe
                {
                    fixed (byte* pData = SendBuffer)
                    {
                        while (TotalBytesSent < SendBuffer.Length)
                        {
                            int ThisBytesSent = NativeMethods.send(SocketHandle, new IntPtr(pData + TotalBytesSent), SendBuffer.Length, NativeMethods.MsgFlags.MSG_NONE);
                            if ((SocketError)ThisBytesSent == SocketError.SocketError)
                            {
                                SocketHandle = IntPtr.Zero;
                                return TotalBytesSent;
                            }
                            else if (ThisBytesSent == 0)
                            {
                                SocketHandle = IntPtr.Zero;
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
            else
            {
                return 0;
            }
        }
    }
}
