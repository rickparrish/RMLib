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
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Runtime.InteropServices;
using System.Text;

// If linux needs to be pinvoked, here's a sample:
//[DllImport("libc")]
//private extern static int send(IntPtr sock, byte[] buf, int count, SocketFlags flags);

namespace RandM.RMLib
{
    public class TcpConnection : IDisposable
    {
        // Events
        public event EventHandler<StringEventArgs> ReadEvent = null;

        // Read buffer size
        protected const int READ_BUFFER_SIZE = 64 * 1024;   // 64k input buffer

        // Protected variables
        protected string _LocalHost = "";
        protected string _LocalIP = "";
        protected Int32 _LocalPort = 0;
        protected Queue<byte> _OutputBuffer = new Queue<byte>();
        protected string _RemoteHost = "";
        protected string _RemoteIP = "";
        protected Int32 _RemotePort = 0;
        protected Socket _Socket = null;
        protected Stream _Stream = null;

        // Private variables
        private IntPtr _DuplicateHandle = IntPtr.Zero;
        private bool _Disposed = false;
        private Queue<byte> _InputBuffer = new Queue<byte>();
        private byte _LastByte = 0;

        // Public properties
        public string LineEnding { get; set; }
        public bool ReadTimedOut { get; private set; }
        public bool StripLF { get; set; }
        public bool StripNull { get; set; }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2214:DoNotCallOverridableMethodsInConstructors")]
        public TcpConnection()
        {
            // Reset the socket
            InitSocket();
        }

        ~TcpConnection()
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
                    Close();
                }

                // Call the appropriate methods to clean up
                // unmanaged resources here.
                // If disposing is false,
                // only the following code is executed.

                // Note disposing has been done.
                _Disposed = true;
            }
        }

        public Socket Accept()
        {
            try
            {
                return _Socket.Accept();
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::Accept().  ErrorCode=" + sex.ErrorCode.ToString());
                return null;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::Accept()");
                return null;
            }
        }

        public TcpConnection AcceptTCP()
        {
            try
            {
                Socket NewSocket = _Socket.Accept();
                TcpConnection NewConnection = new TcpConnection();
                NewConnection.Open(NewSocket);
                return NewConnection;
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::AcceptTCP().  ErrorCode=" + sex.ErrorCode.ToString());
                return null;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::AcceptTCP()");
                return null;
            }
        }

        protected void AddToInputQueue(byte data)
        {
            var ReadHandler = ReadEvent;
            if (ReadHandler != null)
            {
                if ((data >= 33) && (data <= 126))
                {
                    ReadHandler(this, new StringEventArgs("Char " + ((char)data).ToString() + " (0x" + data.ToString("X2") + ")"));
                }
                else
                {
                    ReadHandler(this, new StringEventArgs("Byte " + data.ToString() + " (0x" + data.ToString("X2") + ")"));
                }
            }

            if (StripLF && (_LastByte == 0x0D) && (data == 0x0A))
            {
                // Ignore LF following CR
            }
            else if (StripNull && (_LastByte == 0x0D) && (data == 0x00))
            {
                // Ignore NULL following CR
            }
            else
            {
                _InputBuffer.Enqueue(data);
            }
            _LastByte = data;
        }

        public bool CanAccept()
        {
            return CanAccept(0);
        }

        public bool CanAccept(int milliseconds)
        {
            try
            {
                return (_Socket == null) ? false : _Socket.Poll(milliseconds * 1000, SelectMode.SelectRead);
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::CanAccept().  ErrorCode=" + sex.ErrorCode.ToString());
                return false;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::CanAccept()");
                return false;
            }
        }

        public bool CanRead()
        {
            return CanRead(1);
        }

        public bool CanRead(int milliseconds)
        {
            // Check if buffer is empty
            if (!_InputBuffer.Any()) ReceiveData(milliseconds);
            return _InputBuffer.Any();
        }

        public void Close()
        {
            if (Connected)
            {
                try
                {
                    if (ShutdownOnClose) _Socket.Shutdown(SocketShutdown.Both);
                }
                catch (SocketException sex)
                {
                    switch (sex.ErrorCode)
                    {
                        case 10057: break; // The socket is not connected
                        default: RMLog.DebugException(sex, "SocketException in TcpConnection::Close().  ErrorCode=" + sex.ErrorCode.ToString()); break;
                    }
                }
                catch (Exception ex)
                {
                    RMLog.DebugException(ex, "Exception in TcpConnection::Close()");
                }
                if (Connected)
                {
                    _Stream.Close();
                    _Socket.Close();
                }
            }

            InitSocket();
        }

        public bool Connect(string hostName, Int32 port)
        {
            Close();

            try
            {
                // Get the ip addresses for the give hostname, and then convert from ipv4 to ipv6 if necessary
                var HostAddresses = new List<IPAddress>();
                if (!Socket.OSSupportsIPv6)
                {
                    HostAddresses.AddRange(Dns.GetHostAddresses(hostName));
                    _Socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
                }
                else
                {
                    foreach (var IPA in Dns.GetHostAddresses(hostName))
                    {
                        if (IPA.AddressFamily == AddressFamily.InterNetwork)
                        {
                            HostAddresses.Add(IPAddress.Parse("::ffff:" + IPA.ToString()));
                        }
                        else
                        {
                            HostAddresses.Add(IPA);
                        }
                    }

                    _Socket = new Socket(AddressFamily.InterNetworkV6, SocketType.Stream, ProtocolType.Tcp);
                    // Only do the Mono check here because the XP check is done above
                    if (!ProcessUtils.IsRunningOnMono) // From: https://github.com/statianzo/Fleck/blob/master/src/Fleck/WebSocketServer.cs
                    {
#if __MonoCS__
#else
                        // Windows default to IPv6Only=true, so we call this to allow connections to both IPv4 and IPv6
                        // Mono will throw an exception, but that's OK because it defaults to allowing both IPv4 and IPv6
                        _Socket.SetSocketOption(SocketOptionLevel.IPv6, (SocketOptionName)27, false); // 27 = SocketOptionName.IPv6Only
#endif
                    }
                }

                _Socket.Blocking = true;
                _Socket.Connect(HostAddresses.ToArray(), port);
                if (_Socket.Connected)
                {
                    _LocalIP = ((IPEndPoint)_Socket.LocalEndPoint).Address.ToString();
                    _LocalPort = ((IPEndPoint)_Socket.LocalEndPoint).Port;
                    _RemoteIP = ((IPEndPoint)_Socket.RemoteEndPoint).Address.ToString();
                    _RemotePort = ((IPEndPoint)_Socket.RemoteEndPoint).Port;
                    _Stream = new NetworkStream(_Socket);

                    if (_RemoteIP.ToLower().StartsWith("::ffff:"))
                    {
                        _RemoteIP = _RemoteIP.Substring(7); // Trim leading ::ffff:
                    }
                    else if (((IPEndPoint)_Socket.RemoteEndPoint).Address.AddressFamily == AddressFamily.InterNetworkV6)
                    {
                        _RemoteIP = "[" + _RemoteIP + "]"; // Wrap IPv6 in []
                    }
                }
                return _Socket.Connected;
            }
            catch (SocketException sex)
            {
                switch (sex.ErrorCode)
                {
                    case 10060: break; // Connection timed out
                    case 10061: break; // Connection refused
                    case 10065: break; // No route to host
                    default: RMLog.DebugException(sex, "SocketException in TcpConnection::Connect().  ErrorCode=" + sex.ErrorCode.ToString()); break;
                }
                return false;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::Connect()");
                return false;
            }
        }

        public bool Connected
        {
            get { return ((_Socket != null) && (_Socket.Connected)); }
        }

        public static IPAddress GetAddrByName(string hostName)
        {
            try
            {
                IPAddress[] Addresses = Dns.GetHostAddresses(hostName);
                return (Addresses.Length > 0) ? Addresses[0] : IPAddress.None;
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::GetAddrByName().  ErrorCode=" + sex.ErrorCode.ToString());
                return IPAddress.None;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::GetAddrByName()");
                return IPAddress.None;
            }
        }

        public static string GetHostByIP(string ipAddress)
        {
            try
            {
                return Dns.GetHostEntry(ipAddress).HostName.ToString();
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::GetHostByIP().  ErrorCode=" + sex.ErrorCode.ToString());
                return "";
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::GetHostByIP()");
                return "";
            }
        }

        public static string GetIPByName(string hostName)
        {
            try
            {
                return Dns.GetHostEntry(hostName).AddressList[0].ToString();
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::GetIPByName().  ErrorCode=" + sex.ErrorCode.ToString());
                return "";
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::GetIPByName()");
                return "";
            }
        }

        public string GetLocalHost()
        {
            if (string.IsNullOrWhiteSpace(_LocalHost)) { _LocalHost = GetHostByIP(_LocalIP); }
            return _LocalHost;
        }

        public string GetLocalIP()
        {
            return _LocalIP;
        }

        public static string GetLocalIPs()
        {
            try
            {
                StringBuilder Result = new StringBuilder();

                IPAddress[] Addresses = Dns.GetHostAddresses(Dns.GetHostName());
                foreach (IPAddress Address in Addresses)
                {
                    if (Result.Length > 0) { Result.Append(","); }
                    Result.Append(Address.ToString());
                }
                return Result.ToString();
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::GetLocalIPs().  ErrorCode=" + sex.ErrorCode.ToString());
                return "";
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::GetLocalIPs()");
                return "";
            }
        }

        public Int32 GetLocalPort()
        {
            return _LocalPort;
        }

        public string GetRemoteHost()
        {
            if (string.IsNullOrWhiteSpace(_RemoteHost)) { _RemoteHost = GetHostByIP(_RemoteIP); }
            return _RemoteHost;
        }

        public string GetRemoteIP()
        {
            return _RemoteIP;
        }

        public Int32 GetRemotePort()
        {
            return _RemotePort;
        }

        public Socket GetSocket()
        {
            return _Socket;
        }

        public Stream GetStream()
        {
            return _Stream;
        }

        public IntPtr Handle
        {
            get
            {
                if (Environment.OSVersion.Platform == PlatformID.Win32Windows)
                {
                    if (_DuplicateHandle == IntPtr.Zero)
                    {
                        NativeMethods.DuplicateHandle(Process.GetCurrentProcess().Handle, _Socket.Handle, Process.GetCurrentProcess().Handle, out _DuplicateHandle, 0, true, (uint)NativeMethods.DuplicateOptions.DUPLICATE_SAME_ACCESS);
                    }
                    return _DuplicateHandle;
                }
                else
                {
                    return _Socket.Handle;
                }
            }
        }

        protected virtual void InitSocket()
        {
            _Socket = null;
            _Stream = null;

            _InputBuffer.Clear();
            _OutputBuffer.Clear();
            LineEnding = "\r\n";
            _LocalHost = "";
            _LocalIP = "";
            _LocalPort = 0;
            ReadTimedOut = false;
            _RemoteHost = "";
            _RemoteIP = "";
            _RemotePort = 0;
            ShutdownOnClose = true;
            StripLF = false;
        }

        public bool Listen(string ipAddress, Int32 port)
        {
            Close();

            try
            {
                IPAddress IPA = ParseIPAddress(ipAddress);
                _Socket = new Socket(IPA.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
                if (!ProcessUtils.IsRunningOnMono && Socket.OSSupportsIPv6) // From: https://github.com/statianzo/Fleck/blob/master/src/Fleck/WebSocketServer.cs
                {
#if __MonoCS__
#else
                    // Windows default to IPv6Only=true, so we call this to allow connections to both IPv4 and IPv6
                    // Mono will throw an exception, but that's OK because it defaults to allowing both IPv4 and IPv6
                    _Socket.SetSocketOption(SocketOptionLevel.IPv6, (SocketOptionName)27, false); // 27 = SocketOptionName.IPv6Only
#endif
                }
                _Socket.Blocking = true;
                _Socket.Bind(new IPEndPoint(IPA, port));
                _Socket.Listen(5);

                return true;
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::Listen().  ErrorCode=" + sex.ErrorCode.ToString());
                return false;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::Listen()");
                return false;
            }
        }

        protected virtual void NegotiateInbound(byte[] data)
        {
            NegotiateInbound(data, data.Length);
        }

        protected virtual void NegotiateInbound(byte[] data, int numberOfBytes)
        {
            for (int i = 0; i < numberOfBytes; i++)
            {
                AddToInputQueue(data[i]);
            }
        }

        protected virtual void NegotiateOutbound(byte[] data)
        {
            NegotiateOutbound(data, data.Length);
        }

        protected virtual void NegotiateOutbound(byte[] data, int numberOfBytes)
        {
            for (int i = 0; i < numberOfBytes; i++)
            {
                _OutputBuffer.Enqueue(data[i]);
            }
        }

        public bool Open(int ASocketHandle)
        {
            if (OSUtils.IsWindows)
            {
                try
                {
                    NativeMethods.WSAData WSA = new NativeMethods.WSAData();
                    SocketError Result = NativeMethods.WSAStartup((short)0x0202, out WSA);
                    if (Result != SocketError.Success) throw new SocketException(NativeMethods.WSAGetLastError());

                    SocketPermission SP = new SocketPermission(System.Security.Permissions.PermissionState.Unrestricted);
                    SP.Demand();

                    SocketInformation SI = new SocketInformation() {
                        Options = SocketInformationOptions.Connected,
                        ProtocolInformation = new byte[Marshal.SizeOf(typeof(NativeMethods.WSAPROTOCOL_INFO))],
                    };

                    Result = SocketError.Success;
                    unsafe
                    {
                        fixed (byte* pinnedBuffer = SI.ProtocolInformation)
                        {
                            Result = NativeMethods.WSADuplicateSocket(new IntPtr(ASocketHandle), (uint)Process.GetCurrentProcess().Id, pinnedBuffer);
                        }
                    }

                    if (Result != SocketError.Success) throw new SocketException(NativeMethods.WSAGetLastError());

                    return Open(new Socket(SI));
                }
                catch (SocketException sex)
                {
                    RMLog.DebugException(sex, "SocketException in TcpConnection::Open().  ErrorCode=" + sex.ErrorCode.ToString());
                    return false;
                }
                catch (Exception ex)
                {
                    RMLog.DebugException(ex, "Exception in TcpConnection::Open()");
                    return false;
                }
            }
            else
            {
                RMLog.Debug("MONO cannot open an existing socket handle");
                return false;
                //try
                //{
                //    SocketInformation SI = new SocketInformation();
                //    SI.Options = SocketInformationOptions.Connected;
                //    SI.ProtocolInformation = new byte[24];

                //    // From Mono's Socket.cs DuplicateAndClose() SI.ProtocolInformation = Mono.DataConverter.Pack("iiiil", (int)address_family, (int)socket_type, (int)protocol_type, isbound ? 1 : 0, (long)socket);
                //    byte[] B1 = BitConverter.GetBytes((int)AddressFamily.InterNetwork);
                //    byte[] B2 = BitConverter.GetBytes((int)SocketType.Stream);
                //    byte[] B3 = BitConverter.GetBytes((int)ProtocolType.Tcp);
                //    byte[] B4 = BitConverter.GetBytes((int)1);
                //    byte[] B5 = BitConverter.GetBytes((long)ASocketHandle);
                //    Array.Copy(B1, 0, SI.ProtocolInformation, 0, B1.Length);
                //    Array.Copy(B2, 0, SI.ProtocolInformation, 4, B2.Length);
                //    Array.Copy(B3, 0, SI.ProtocolInformation, 8, B3.Length);
                //    Array.Copy(B4, 0, SI.ProtocolInformation, 12, B4.Length);
                //    Array.Copy(B5, 0, SI.ProtocolInformation, 16, B5.Length);

                //    return Open(new Socket(SI));
                //}
                //catch (SocketException sex)
                //{
                //    RMLog.DebugException(sex, "SocketException in TcpConnection::Open().  ErrorCode=" + sex.ErrorCode.ToString());
                //    return false;
                //}
                //catch (Exception ex)
                //{
                //    RMLog.DebugException(ex, "Exception in TcpConnection::Open()");
                //    return false;
                //}
            }
        }

        public bool Open(byte[] socketInformationBytes)
        {
            try
            {
                SocketInformation SI = new SocketInformation() { 
                    ProtocolInformation = socketInformationBytes,
                    Options = SocketInformationOptions.Connected,
                };

                _Socket = new Socket(SI) { 
                    Blocking = true,
                };
                if (_Socket.Connected) _Stream = new NetworkStream(_Socket);
                return _Socket.Connected;
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::Open().  ErrorCode=" + sex.ErrorCode.ToString());
                return false;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::Open()");
                return false;
            }
        }

        public virtual bool Open(Socket socket)
        {
            Close();

            try
            {
                _Socket = socket;
                _Socket.Blocking = true;
                if (_Socket.Connected)
                {
                    _LocalIP = ((IPEndPoint)_Socket.LocalEndPoint).Address.ToString();
                    _LocalPort = ((IPEndPoint)_Socket.LocalEndPoint).Port;
                    _RemoteIP = ((IPEndPoint)_Socket.RemoteEndPoint).Address.ToString();
                    _RemotePort = ((IPEndPoint)_Socket.RemoteEndPoint).Port;
                    _Stream = new NetworkStream(_Socket);

                    if (_RemoteIP.ToLower().StartsWith("::ffff:"))
                    {
                        _RemoteIP = _RemoteIP.Substring(7); // Trim leading ::ffff:
                    }
                    else if (((IPEndPoint)_Socket.RemoteEndPoint).Address.AddressFamily == AddressFamily.InterNetworkV6)
                    {
                        _RemoteIP = "[" + _RemoteIP + "]"; // Wrap IPv6 in []
                    }
                }
                return _Socket.Connected;
            }
            catch (SocketException sex)
            {
                RMLog.DebugException(sex, "SocketException in TcpConnection::Open().  ErrorCode=" + sex.ErrorCode.ToString());
                return false;
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::Open()");
                return false;
            }
        }

        // Modified version of https://github.com/statianzo/Fleck/blob/master/src/Fleck/WebSocketServer.cs
        private IPAddress ParseIPAddress(string ipAddress)
        {
            string[] BindToAll = new string[]
            {
                "",
                "0.0.0.0",
                "::",
                "[::]",
                "0000:0000:0000:0000:0000:0000:0000:0000",
                "[0000:0000:0000:0000:0000:0000:0000:0000]"
            };

            if (BindToAll.Contains(ipAddress))
            {
                // We want to bind to all addresses, so return IPAddress.Any or IPv6Any based on the OS we're running on
                return Socket.OSSupportsIPv6 ? IPAddress.IPv6Any : IPAddress.Any;
            }
            else
            {
                try
                {
                    return IPAddress.Parse(ipAddress);
                }
                catch (Exception ex)
                {
                    throw new FormatException("Failed to parse the IP address. Please make sure you specify a valid IP address. Use 0.0.0.0 or [::] to listen on all interfaces.", ex);
                }
            }
        }

        public byte[] PeekBytes()
        {
            return _InputBuffer.ToArray();
        }

        public char? PeekChar()
        {
            if (!_InputBuffer.Any())
            {
                return null;
            }
            else
            {
                return (char)_InputBuffer.Peek();
            }
        }

        public string PeekString()
        {
            return RMEncoding.Ansi.GetString(_InputBuffer.ToArray());
        }

        public byte[] ReadBytes()
        {
            return ReadBytes(int.MaxValue);
        }

        public byte[] ReadBytes(int bytesToRead)
        {
            bytesToRead = Math.Min(bytesToRead, _InputBuffer.Count);

            byte[] Result = new byte[bytesToRead];
            for (int i = 0; i < bytesToRead; i++)
            {
                Result[i] = _InputBuffer.Dequeue();
            }

            return Result;
        }

        public char? ReadChar(Int32 timeOut)
        {
            DateTime StartTime = DateTime.Now;
            ReadTimedOut = false;

            while (true)
            {
                if (!Connected) return null;

                if (CanRead())
                {
                    return (char)_InputBuffer.Dequeue();
                }

                if ((timeOut != 0) && (DateTime.Now.Subtract(StartTime).TotalMilliseconds > timeOut))
                {
                    ReadTimedOut = true;
                    return null;
                }
            }
        }

        public string ReadLn(int timeOut)
        {
            return ReadLn(new string[] { LineEnding }, true, '\0', timeOut);
        }

        public string ReadLn(string terminator, int timeOut)
        {
            return ReadLn(new string[] { terminator }, false, '\0', timeOut);
        }

        public string ReadLn(bool echo, int timeOut)
        {
            return ReadLn(new string[] { LineEnding }, echo, '\0', timeOut);
        }

        public string ReadLn(char passwordCharacter, int timeOut)
        {
            return ReadLn(new string[] { LineEnding }, true, passwordCharacter, timeOut);
        }

        public string ReadLn(string terminator, bool echo, char passwordCharacter, int timeOut)
        {
            return ReadLn(new string[] { terminator }, echo, passwordCharacter, timeOut);
        }

        public string ReadLn(string[] terminators, bool echo, char passwordCharacter, int timeOut)
        {
            DateTime StartTime = DateTime.Now;
            char? Ch = null;
            bool NeedTerminator = true;
            string Result = "";
            string TerminatorCharacters = string.Join("", terminators);

            while (NeedTerminator)
            {
                if (!Connected) break;

                Ch = ReadChar((timeOut == 0) ? 0 : timeOut - (int)DateTime.Now.Subtract(StartTime).TotalMilliseconds);
                if (Ch != null)
                {
                    if (echo)
                    {
                        if ((Ch == '\x08') || (Ch == '\x7F'))
                        {
                            // Delete, check if we have a character to remove
                            if (Result.Length > 0)
                            {
                                // Determine if character we're trimming is printable, and if so, backspace it off the screen
                                Ch = Result[Result.Length - 1];
                                if (Ch >= ' ') Write("\x08 \x08");

                                // Remove the last character from the input string
                                Result = Result.Substring(0, Result.Length - 1);
                            }
                        }
                        else
                        {
                            // Not a delete, so add it to the string and echo it if its printable
                            if ((Ch >= ' '))
                            {
                                Result += Ch;
                                Write((passwordCharacter == '\0') ? Ch.ToString() : passwordCharacter.ToString());
                            }
                            // Also add it to the string (but no echo) if its part of a non-printable terminator
                            else if (TerminatorCharacters.Contains(Ch.ToString()))
                            {
                                Result += Ch;
                            }
                        }
                    }
                    else
                    {
                        Result += Ch;
                    }
                }
                else if (ReadTimedOut)
                {
                    // Return what we have so far
                    return Result;
                }

                foreach (string Terminator in terminators)
                {
                    if (Result.EndsWith(Terminator))
                    {
                        NeedTerminator = false;
                        break;
                    }
                }
            }

            // Find the terminator and remove it
            foreach (string Terminator in terminators)
            {
                if (Result.EndsWith(Terminator))
                {
                    Result = Result.Substring(0, Result.Length - Terminator.Length);
                    break;
                }
            }

            if (echo) { WriteLn(); }
            return Result;
        }

        /// <summary>
        /// Reads the entire buffer into a string variable
        /// </summary>
        /// <returns></returns>
        public string ReadString()
        {
            string Result = RMEncoding.Ansi.GetString(_InputBuffer.ToArray());
            _InputBuffer.Clear();
            return Result;
        }

        public void ReceiveData(int milliseconds)
        {
            try
            {
                if ((Connected) && (_Socket.Poll(milliseconds * 1000, SelectMode.SelectRead)))
                {
                    byte[] Buffer = new byte[READ_BUFFER_SIZE];
                    int BytesRead = _Stream.Read(Buffer, 0, READ_BUFFER_SIZE);
                    if (BytesRead == 0)
                    {
                        // No bytes read means we have a disconnect
                        _Stream.Close();
                        _Socket.Close();
                    }
                    else
                    {
                        // If we have bytes, add them to the buffer
                        NegotiateInbound(Buffer, BytesRead);
                    }
                }
            }
            catch (IOException ioex)
            {
                if (ioex.Message == "Read failure")
                {
                    // Apparently we have a disconnect
                }
                else
                {
                    RMLog.DebugException(ioex, "IOException in TcpConnection::ReceiveData()");
                }
                _Stream.Close();
                _Socket.Close();
            }
            catch (SocketException sex)
            {
                if (sex.Message == "Connection reset by peer")
                {
                    // Apparently we have a disconnect
                }
                else
                {
                    RMLog.DebugException(sex, "SocketException in TcpConnection::ReceiveData().  ErrorCode=" + sex.ErrorCode.ToString());
                }
                _Stream.Close();
                _Socket.Close();
            }
            catch (Exception ex)
            {
                RMLog.DebugException(ex, "Exception in TcpConnection::ReceiveData()");
                _Stream.Close();
                _Socket.Close();
            }
        }

        public void SetBlocking(bool block)
        {
            if (Connected)
            {
                _Socket.Blocking = block;
            }
        }

        public bool ShutdownOnClose { get; set; }

        public virtual void Write(string text)
        {
            WriteBytes(RMEncoding.Ansi.GetBytes(text));
        }

        public void WriteBytes(byte[] data)
        {
            WriteBytes(data, data.Length);
        }

        public void WriteBytes(byte[] data, int numberOfBytes)
        {
            NegotiateOutbound(data, numberOfBytes);
            WriteRaw(_OutputBuffer.ToArray());
            _OutputBuffer.Clear();
        }

        public void WriteLn()
        {
            Write("\r\n");
        }

        public void WriteLn(string text)
        {
            Write(text + "\r\n");
        }

        public void WriteRaw(byte[] data)
        {
            if (Connected)
            {
                try
                {
                    // TODO Will this always write all the bytes?
                    _Stream.Write(data, 0, data.Length);
                }
                catch (IOException ioex)
                {
                    if (ioex.Message == "Write failure")
                    {
                        // Apparently we have a disconnect
                    }
                    else
                    {
                        RMLog.DebugException(ioex, "IOException in TcpConnection::WriteRaw()");
                    }
                    _Stream.Close();
                    _Socket.Close();
                }
                catch (SocketException sex)
                {
                    if ((sex.Message == "An established connection was aborted by the software in your host machine") ||
                        (sex.Message == "An existing connection was forcibly closed by the remote host"))
                    {
                        // Apparently we have a disconnect
                    }
                    else
                    {
                        RMLog.DebugException(sex, "SocketException in TcpConnection::WriteRaw().  ErrorCode=" + sex.ErrorCode.ToString());
                    }
                    _Stream.Close();
                    _Socket.Close();
                }
                catch (Exception ex)
                {
                    RMLog.DebugException(ex, "Exception in TcpConnection::ReceiveData()");
                    _Stream.Close();
                    _Socket.Close();
                }
            }
        }
    }

    // Supported connection types
    public enum ConnectionType { None, RLogin, Telnet, WebSocket };
}
