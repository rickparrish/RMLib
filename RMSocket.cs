using System;
using System.Collections;
using System.Collections.Generic;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace RandM.RMLib
{
    class RMSocket : IDisposable
    {
        private bool _Disposed = false;
        private AutoResetEvent _ReadEvent = new AutoResetEvent(false);
        private object _ReadLock = new object();
        private Queue<byte> _ReadQueue = new Queue<byte>();
        private Thread _ReadThread;
        private AutoResetEvent _WriteEvent = new AutoResetEvent(false);
        private object _WriteLock = new object();
        private Queue<byte> _WriteQueue = new Queue<byte>();
        private Thread _WriteThread;

        public RMSocket(int socketHandle)
        {
            if (OSUtils.IsWindows)
            {
                NativeMethods.WSAData WSA = new NativeMethods.WSAData();
                SocketError Result = NativeMethods.WSAStartup((short)0x0202, out WSA);
                if (Result == SocketError.Success)
                {
                    SocketHandle = new IntPtr(socketHandle);
                }
                else
                {
                    SocketHandle = IntPtr.Zero;
                }
            }
            else
            {
                SocketHandle = new IntPtr(socketHandle);
            }

            // Start threads
            _ReadThread = new Thread(new ThreadStart(ReadFunc));
            _ReadThread.IsBackground = true;
            _ReadThread.Start();
            _WriteThread = new Thread(new ThreadStart(WriteFunc));
            _WriteThread.IsBackground = true;
            _WriteThread.Start();
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

        public void ClearBuffers()
        {
            lock (_ReadLock)
            {
                _ReadQueue.Clear();
            }

            lock (_WriteLock)
            {
                _WriteQueue.Clear();
            }
        }

        public void Close()
        {
            if (Connected)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.closesocket(SocketHandle);
                }
                else
                {
                    NativeMethods.close_libc(SocketHandle.ToInt32());
                }
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

        public byte ReadByte()
        {
            while (true)
            {
                lock (_ReadLock)
                {
                    if (_ReadQueue.Count > 0)
                    {
                        return _ReadQueue.Dequeue();
                    }
                }

                // No data waiting, so wait for ReadFunc() to signal that new data has been received
                _ReadEvent.WaitOne();
            }
        }

        private unsafe void ReadFunc()
        {
            int BytesRead = 0;
            byte[] Buffer = new byte[1024];

            fixed (byte* pData = Buffer)
            {
                while (Connected)
                {
                    if (OSUtils.IsWindows)
                    {
                        BytesRead = NativeMethods.recv(SocketHandle, new IntPtr(pData), Buffer.Length, SocketFlags.None);
                    }
                    else
                    {
                        BytesRead = NativeMethods.recv_libc(SocketHandle, pData, Buffer.Length, (int)SocketFlags.None);
                    }

                    if ((SocketError)BytesRead == SocketError.SocketError)
                    {
                        SocketHandle = IntPtr.Zero;
                    }
                    else if (BytesRead == 0)
                    {
                        SocketHandle = IntPtr.Zero;
                    }
                    else
                    {
                        lock (_ReadLock)
                        {
                            for (int i = 0; i < BytesRead; i++)
                            {
                                _ReadQueue.Enqueue(Buffer[i]);
                            }
                        }

                        _ReadEvent.Set();
                    }
                }
            }
        }

        public int ReadQueueSize
        {
            get
            {
                lock (_ReadLock)
                {
                    return _ReadQueue.Count;
                }
            }
        }

        public IntPtr SocketHandle { get; private set; }

        public void Shutdown()
        {
            if (Connected)
            {
                if (OSUtils.IsWindows)
                {
                    NativeMethods.shutdown(SocketHandle, NativeMethods.ShutDownFlags.SD_BOTH);
                }
                else
                {
                    NativeMethods.shutdown_libc(SocketHandle, (int)NativeMethods.ShutDownFlags.SD_BOTH);
                }
            }
        }

        public void WriteBytes(byte[] buffer)
        {
            if (buffer == null) throw new ArgumentNullException("buffer");
            if (buffer.Length == 0) return;
            if (!Connected) return;

            lock (_WriteLock)
            {
                for (int i = 0; i < buffer.Length; i++)
                {
                    _WriteQueue.Enqueue(buffer[i]);
                }
            }

            // Tell WriteFunc() that there's data to be sent
            _WriteEvent.Set();
        }

        public void WriteString(string text)
        {
            if (text == null) throw new ArgumentException("text");
            if (text.Length == 0) return;
            if (!Connected) return;

            WriteBytes(RMEncoding.Ansi.GetBytes(text));
        }

        private unsafe void WriteFunc()
        {
            int BytesWritten = 0;
            byte[] Buffer;

            while (Connected)
            {
                lock (_WriteLock)
                {
                    Buffer = (_WriteQueue.Count > 0) ? _WriteQueue.ToArray() : null;
                }

                if (Buffer == null)
                {
                    // No data waiting to be sent, so wait for Write*() to add new data to the write buffer
                    _WriteEvent.WaitOne();
                }
                else
                {
                    fixed (byte* pData = Buffer)
                    {
                        if (OSUtils.IsWindows)
                        {
                            BytesWritten = NativeMethods.send(SocketHandle, new IntPtr(pData), Buffer.Length, NativeMethods.MsgFlags.MSG_NONE);
                        }
                        else
                        {
                            BytesWritten = NativeMethods.send_libc(SocketHandle, pData, Buffer.Length, (int)NativeMethods.MsgFlags.MSG_NONE);
                        }
                    }

                    if ((SocketError)BytesWritten == SocketError.SocketError)
                    {
                        SocketHandle = IntPtr.Zero;
                    }
                    else if (BytesWritten > 0)
                    {
                        lock (_WriteLock)
                        {
                            for (int i = 0; i < BytesWritten; i++)
                            {
                                _WriteQueue.Dequeue();
                            }
                        }
                    }
                }
            }
        }
    }
}
