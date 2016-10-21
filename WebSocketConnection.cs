﻿/*
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

  The handshake/negotiation routines in this class were not written by me.  Unfortunately
  I did not record where I got them from, so I can't properly acknowledge the original source.
*/
using System;
using System.Net.Sockets;
using System.Text;
using System.Diagnostics;
using System.Collections.Specialized;
using System.Text.RegularExpressions;
using System.Security.Cryptography;
using System.Collections.Generic;
using System.Collections;
using System.IO;
using System.Threading;
using System.Net.Security;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;

namespace RandM.RMLib
{
    /// <summary>
    /// Not a very complete implementation, but good enough for the girls I date
    /// </summary>
    public class WebSocketConnection : TcpConnection
    {
        public enum ProtocolVersion
        {
            None,
            Hixie75,
            Hixie76,
            RFC6455
        }

        private X509Certificate2 _Certificate = null;
        private byte[] _FrameMask = null;
        private int _FrameOpCode = 0;
        private long _FramePayloadLength = 0;
        private int _FramePayloadReceived = 0;
        private StringDictionary _Header = new StringDictionary();
        private ProtocolVersion _ProtocolVersion = ProtocolVersion.None;
        private byte[] _QueuedBytes = null;
        private bool _Shake = true;
        private bool _Shook = false;
        private WebSocketNegotiationState _State = WebSocketNegotiationState.NeedPacketStart;

        public WebSocketConnection() : this(true) { }

        public WebSocketConnection(bool shake) : this(shake, null) { }

        public WebSocketConnection(bool shake, X509Certificate2 certificate)
        {
            _Shake = shake;
            _Shook = !shake;
            _Certificate = certificate;

            FlashPolicyFileRequest = false;
            Protocol = "ws";
            SubProtocol = "plain";
        }

        private int CalculateWebSocketKey(string text)
        {
            string Digits = "";
            int Spaces = 0;

            // Loop through the line, looking for digits and spaces
            for (var i = 0; i < text.Length; i++)
            {
                if (text[i] == ' ')
                {
                    Spaces++;
                }
                else if ((text[i] >= '0') && (text[i] <= '9'))
                {
                    Digits += text[i];
                }

            }

            return (int)(Convert.ToInt64(Digits) / Spaces);
        }

        public bool FlashPolicyFileRequest { get; set; }

        public StringDictionary Header { get { return _Header; } }

        // NB: The base class constructor calls InitSocket(), which means this method will run before this classes constructor, so
        //     everything accessed here needs to be initialized already (ie can't rely on the constructor to initialize it)
        protected override void InitSocket()
        {
            base.InitSocket();
        }

        protected override void NegotiateInbound(byte[] data, int numberOfBytes)
        {
            if (_Shook)
            {
                switch (_ProtocolVersion)
                {
                    case ProtocolVersion.Hixie76:
                        NegotiateInboundHixie76(data, numberOfBytes);
                        break;
                    case ProtocolVersion.RFC6455:
                        NegotiateInboundRFC6455(data, numberOfBytes);
                        break;
                }
            }
            else
            {
                for (int i = 0; i < numberOfBytes; i++)
                {
                    AddToInputQueue(data[i]);
                }
            }
        }

        protected void NegotiateInboundHixie76(byte[] data, int numberOfBytes)
        {
            if (_QueuedBytes != null)
            {
                MemoryStream MS = new MemoryStream();
                MS.Write(_QueuedBytes, 0, _QueuedBytes.Length);
                MS.Write(data, 0, data.Length);
                data = MS.ToArray();
                numberOfBytes = data.Length;
                _QueuedBytes = null;
            }

            for (int i = 0; i < numberOfBytes; i++)
            {
                // Check what the client packet state is
                switch (_State)
                {
                    case WebSocketNegotiationState.NeedPacketStart:
                        // Check for 0x00 to indicate the start of a data packet
                        if (data[i] == 0x00)
                        {
                            _State = WebSocketNegotiationState.Data;
                        }
                        break;
                    case WebSocketNegotiationState.Data:
                        // We're in a data packet, so check for 0xFF, which indicates the data packet is done
                        if (data[i] == 0xFF)
                        {
                            _State = WebSocketNegotiationState.NeedPacketStart;
                        }
                        else
                        {
                            // Check if the byte needs to be UTF-8 decoded
                            if (data[i] < 128)
                            {
                                AddToInputQueue(data[i]);
                            }
                            else if ((data[i] > 191) && (data[i] < 224))
                            {
                                // Handle UTF-8 decode
                                if (i < (numberOfBytes - 1))
                                {
                                    AddToInputQueue((byte)(((data[i] & 31) << 6) | (data[++i] & 63)));
                                }
                                else
                                {
                                    _QueuedBytes = new byte[] { data[i] };
                                }
                            }
                            else
                            {
                                // Handle UTF-8 decode (should never need this, but included anyway)
                                if (i < (numberOfBytes - 2))
                                {
                                    AddToInputQueue((byte)(((data[i] & 15) << 12) | ((data[++i] & 63) << 6) | (data[++i] & 63)));
                                }
                                else if (i < (numberOfBytes - 1))
                                {
                                    _QueuedBytes = new byte[] { data[i], data[++i] };
                                }
                                else
                                {
                                    _QueuedBytes = new byte[] { data[i] };
                                }
                            }
                        }
                        break;
                }
            }
        }

        protected void NegotiateInboundRFC6455(byte[] data, int numberOfBytes)
        {
            if (_QueuedBytes != null)
            {
                MemoryStream MS = new MemoryStream();
                MS.Write(_QueuedBytes, 0, _QueuedBytes.Length);
                MS.Write(data, 0, data.Length);
                data = MS.ToArray();
                numberOfBytes = data.Length;
                _QueuedBytes = null;
            }

            for (int i = 0; i < numberOfBytes; i++)
            {
                // Check what the client packet state is
                switch (_State)
                {
                    case WebSocketNegotiationState.NeedPacketStart:
                        // Next byte will give us the opcode, and also tell is if the message is fragmented
                        _FrameMask = new byte[4];
                        _FrameOpCode = data[i];
                        _FramePayloadLength = 0;
                        _FramePayloadReceived = 0;
                        _State = WebSocketNegotiationState.NeedPayloadLength;
                        break;
                    case WebSocketNegotiationState.NeedPayloadLength:
                        _FramePayloadLength = (data[i] & 0x7F);
                        if (_FramePayloadLength <= 125)
                        {
                            _State = WebSocketNegotiationState.NeedMaskingKey;
                        }
                        else if (_FramePayloadLength == 126)
                        {
                            if (i < (numberOfBytes - 2))
                            {
                                byte[] Bytes = new byte[] { data[++i], data[++i] };
                                _FramePayloadLength = BitConverter.ToInt16(Bytes, 0);
                                _State = WebSocketNegotiationState.NeedMaskingKey;
                            }
                            else
                            {
                                List<byte> LeftoverBytes = new List<byte>();
                                while (i < numberOfBytes) LeftoverBytes.Add(data[i++]);
                                _QueuedBytes = LeftoverBytes.ToArray();
                            }
                        }
                        else if (_FramePayloadLength == 127)
                        {
                            if (i < (numberOfBytes - 8))
                            {
                                byte[] Bytes = new byte[] { data[++i], data[++i], data[++i], data[++i], data[++i], data[++i], data[++i], data[++i] };
                                _FramePayloadLength = BitConverter.ToInt64(Bytes, 0);
                                _State = WebSocketNegotiationState.NeedMaskingKey;
                            }
                            else
                            {
                                List<byte> LeftoverBytes = new List<byte>();
                                while (i < numberOfBytes) LeftoverBytes.Add(data[i++]);
                                _QueuedBytes = LeftoverBytes.ToArray();
                            }
                        }
                        break;
                    case WebSocketNegotiationState.NeedMaskingKey:
                        if (i < (numberOfBytes - 3))
                        {
                            byte[] Bytes = new byte[] { data[i], data[++i], data[++i], data[++i] };
                            int TempMask = BitConverter.ToInt32(Bytes, 0);
                            _FrameMask[3] = (byte)((TempMask & 0xFF000000) >> 24);
                            _FrameMask[2] = (byte)((TempMask & 0x00FF0000) >> 16);
                            _FrameMask[1] = (byte)((TempMask & 0x0000FF00) >> 8);
                            _FrameMask[0] = (byte)(TempMask & 0x000000FF);
                            _State = (_FramePayloadLength > 0) ? WebSocketNegotiationState.Data : WebSocketNegotiationState.NeedPacketStart;
                        }
                        else
                        {
                            List<byte> LeftoverBytes = new List<byte>();
                            while (i < numberOfBytes) LeftoverBytes.Add(data[i++]);
                            _QueuedBytes = LeftoverBytes.ToArray();
                        }
                        break;
                    case WebSocketNegotiationState.Data:
                        byte UnMaskedByte = (byte)(data[i] ^ _FrameMask[_FramePayloadReceived++ % 4]);

                        // Check if the byte needs to be UTF-8 decoded
                        if (UnMaskedByte < 128)
                        {
                            AddToInputQueue(UnMaskedByte);
                        }
                        else if ((UnMaskedByte > 191) && (UnMaskedByte < 224))
                        {
                            // Handle UTF-8 decode
                            if (i < (numberOfBytes - 1))
                            {
                                byte UnMaskedByte2 = (byte)(data[++i] ^ _FrameMask[_FramePayloadReceived++ % 4]);
                                AddToInputQueue((byte)(((UnMaskedByte & 31) << 6) | (UnMaskedByte2 & 63)));
                            }
                            else
                            {
                                _QueuedBytes = new byte[] { data[i] };
                                _FramePayloadReceived -= 1; // Roll back how much we've received so the masking isn't broken
                            }
                        }
                        else
                        {
                            // Handle UTF-8 decode (should never need this, but included anyway)
                            if (i < (numberOfBytes - 2))
                            {
                                byte UnMaskedByte2 = (byte)(data[++i] ^ _FrameMask[_FramePayloadReceived++ % 4]);
                                byte UnMaskedByte3 = (byte)(data[++i] ^ _FrameMask[_FramePayloadReceived++ % 4]);
                                AddToInputQueue((byte)(((UnMaskedByte & 15) << 12) | ((UnMaskedByte2 & 63) << 6) | (UnMaskedByte3 & 63)));
                            }
                            else if (i < (numberOfBytes - 1))
                            {
                                _QueuedBytes = new byte[] { data[i], data[++i] };
                                _FramePayloadReceived -= 2; // Roll back how much we've received so the masking isn't broken
                            }
                            else
                            {
                                _QueuedBytes = new byte[] { data[i] };
                                _FramePayloadReceived -= 1; // Roll back how much we've received so the masking isn't broken
                            }
                        }

                        // Check if we've received the full payload
                        if (_FramePayloadReceived == _FramePayloadLength) _State = WebSocketNegotiationState.NeedPacketStart;
                        break;
                }
            }
        }

        protected override void NegotiateOutbound(byte[] data, int numberOfBytes)
        {
            if (_Shook)
            {
                switch (_ProtocolVersion)
                {
                    case ProtocolVersion.Hixie76:
                        NegotiateOutboundHixie76(data, numberOfBytes);
                        break;
                    case ProtocolVersion.RFC6455:
                        NegotiateOutboundRFC6455(data, numberOfBytes);
                        break;
                }
            }
            else
            {
                for (int i = 0; i < numberOfBytes; i++)
                {
                    _OutputBuffer.Enqueue(data[i]);
                }
            }
        }

        protected void NegotiateOutboundHixie76(byte[] data, int numberOfBytes)
        {
            _OutputBuffer.Enqueue(0);
            for (int i = 0; i < numberOfBytes; i++)
            {
                // Check if the byte needs to be UTF-8 encoded
                if (data[i] < 128)
                {
                    _OutputBuffer.Enqueue(data[i]);
                }
                else
                {
                    // Handle UTF-8 encode
                    _OutputBuffer.Enqueue((byte)((data[i] >> 6) | 192));
                    _OutputBuffer.Enqueue((byte)((data[i] & 63) | 128));
                }
            }

            _OutputBuffer.Enqueue(255);
        }

        protected void NegotiateOutboundRFC6455(byte[] data, int numberOfBytes)
        {
            List<byte> ToSend = new List<byte>();

            for (int i = 0; i < numberOfBytes; i++)
            {
                // Check if the byte needs to be UTF-8 encoded
                if (data[i] < 128)
                {
                    ToSend.Add(data[i]);
                }
                else
                {
                    // Handle UTF-8 encode
                    ToSend.Add((byte)((data[i] >> 6) | 192));
                    ToSend.Add((byte)((data[i] & 63) | 128));
                }
            }

            _OutputBuffer.Enqueue(0x81);
            if (ToSend.Count <= 125)
            {
                _OutputBuffer.Enqueue((byte)ToSend.Count);
            }
            else if (ToSend.Count <= 65535)
            {
                _OutputBuffer.Enqueue(126);
                byte[] Bytes = BitConverter.GetBytes((short)ToSend.Count);
                _OutputBuffer.Enqueue(Bytes[1]);
                _OutputBuffer.Enqueue(Bytes[0]);
            }
            else
            {
                _OutputBuffer.Enqueue(127);
                byte[] Bytes = BitConverter.GetBytes((long)ToSend.Count);
                _OutputBuffer.Enqueue(Bytes[7]);
                _OutputBuffer.Enqueue(Bytes[6]);
                _OutputBuffer.Enqueue(Bytes[5]);
                _OutputBuffer.Enqueue(Bytes[4]);
                _OutputBuffer.Enqueue(Bytes[3]);
                _OutputBuffer.Enqueue(Bytes[2]);
                _OutputBuffer.Enqueue(Bytes[1]);
                _OutputBuffer.Enqueue(Bytes[0]);
            }

            for (var i = 0; i < ToSend.Count; i++)
            {
                _OutputBuffer.Enqueue(ToSend[i]);
            }
        }

        public override bool Open(Socket socket)
        {
            if (base.Open(socket))
            {
                if (_Shake)
                {
                    _Shook = ShakeHands();
                    return _Shook;
                }
                else
                {
                    return true;
                }
            }

            return false;
        }

        public string Protocol { get; set; }

        private bool ShakeHands()
        {
            _Header["Version"] = "0";

            try
            {
                // Peek first byte for 22, 128 (indicates ssl)
                // Don't use class methods for peek/read since they eat data into the input buffer, and AuthenticateAsServer needs that data
                if (_Socket.Poll(5 * 1000 * 1000, SelectMode.SelectRead))
                {
                    byte[] FirstByte = new byte[1];
                    _Socket.Receive(FirstByte, 0, 1, SocketFlags.Peek);
                    if ((FirstByte[0] == 22) || (FirstByte[0] == 128))
                    {
                        if (_Certificate == null)
                        {
                            throw new Exception("wss:// requires a certificate");
                        }
                        else
                        {
                            var SSL = new SslStream(_Stream, false);
                            _Stream = SSL;
                            try
                            {
                                SSL.AuthenticateAsServer(_Certificate, false, SslProtocols.Tls, false);
                            }
                            catch (Exception ex)
                            {
                                RMLog.Error("Error during SSL.AuthenticateAsServer(): " + ex.Message);
                                return false;
                            }
                            Protocol = "wss";
                        }
                    }
                }
                else
                {
                    RMLog.Error("Timeout exceeded while waiting for complete handshake");
                    return false;
                }

                // Keep reading header data until we get all the data we want
                DateTime LoopStart = DateTime.Now;
                while (true)
                {
                    // Read another line, and abort if we don't get one within 5 seconds
                    string InLine = ReadLn(new string[] { "\r\n", "\0" }, false, '\0', 5000).Trim();
                    if (ReadTimedOut) {
                        RMLog.Error("Timeout exceeded while waiting for next handshake line");
                        return false;
                    } else if (DateTime.Now.Subtract(LoopStart).TotalSeconds > 30.0) {
                        RMLog.Error("Timeout exceeded while waiting for handshake to complete");
                        return false;
                    }

                    RMLog.Trace("Handshake Line: " + InLine);

                    // Check for blank line (indicates we have most of the header, and only the last 8 bytes remain
                    if (string.IsNullOrEmpty(InLine))
                    {
                        switch (_Header["Version"])
                        {
                            case "0":
                                if (_Header.ContainsKey("Sec-WebSocket-Key1"))
                                {
                                    _ProtocolVersion = ProtocolVersion.Hixie76;
                                    return ShakeHandsHixie76();
                                }
                                else
                                {
                                    // Only used by Chrome 4 and iOS 5.0.0 so probably not worth bothering
                                    _ProtocolVersion = ProtocolVersion.Hixie75;
                                    return false;
                                }
                            case "7":
                            case "8":
                            case "13":
                                _ProtocolVersion = ProtocolVersion.RFC6455;
                                return ShakeHandsRFC6455();
                            default:
                                //		    TODO If this version does not
                                //          match a version understood by the server, the server MUST
                                //          abort the websocket handshake described in this section and
                                //          instead send an appropriate HTTP error code (such as 426
                                //          Upgrade Required), and a |Sec-WebSocket-Version| header
                                //          indicating the version(s) the server is capable of
                                //          understanding.
                                return false;
                        }
                    }
                    else if (InLine.StartsWith("Connection:"))
                    {
                        // Example: "Connection: Upgrade"
                        // NB: New in protocol 8+
                        _Header["Connection"] = InLine.Replace("Connection:", "").Trim();
                    }
                    else if (InLine.StartsWith("GET"))
                    {
                        // Example: "GET /demo HTTP/1.1"
                        string[] GET = InLine.Split(' ');
                        _Header["Path"] = GET[1];
                    }
                    else if (InLine.StartsWith("Host:"))
                    {
                        // Example: "Host: example.com"
                        _Header["Host"] = InLine.Replace("Host:", "").Trim();
                    }
                    else if (InLine.StartsWith("Origin:"))
                    {
                        // Example: "Origin: http://example.com"
                        // NB: Not used in protocol 8+
                        _Header["Origin"] = InLine.Replace("Origin:", "").Trim();
                    }
                    else if (InLine.StartsWith("Sec-WebSocket-Key:"))
                    {
                        // Example: "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ=="
                        // NB: New in protocol 8+
                        _Header["Key"] = InLine.Replace("Sec-WebSocket-Key:", "").Trim();
                    }
                    else if (InLine.StartsWith("Sec-WebSocket-Key1:"))
                    {
                        // Example: "Sec-WebSocket-Key1: 4 @1  46546xW%0l 1 5"
                        // NB: Not used in protocol 8+
                        _Header["Key1"] = InLine.Replace("Sec-WebSocket-Key1:", "").Trim();
                    }
                    else if (InLine.StartsWith("Sec-WebSocket-Key2:"))
                    {
                        // Example: "Sec-WebSocket-Key2: 12998 5 Y3 1  .P00"
                        // NB: Not used in protocol 8+
                        _Header["Key2"] = InLine.Replace("Sec-WebSocket-Key2:", "").Trim();
                    }
                    else if (InLine.StartsWith("Sec-WebSocket-Origin:"))
                    {
                        // Example: "Sec-WebSocket-Origin: http://example.com"
                        // NB: New in protocol 8+
                        _Header["Origin"] = InLine.Replace("Sec-WebSocket-Origin:", "").Trim();
                    }
                    else if (InLine.StartsWith("Sec-WebSocket-Protocol:"))
                    {
                        // Example: "Sec-WebSocket-Protocol: sample"
                        _Header["SubProtocol"] = InLine.Replace("Sec-WebSocket-Protocol:", "").Trim();
                    }
                    else if (InLine.StartsWith("Sec-WebSocket-Draft"))
                    {
                        // Example: "Sec-WebSocket-Draft: 2"
                        _Header["Version"] = InLine.Replace("Sec-WebSocket-Draft:", "").Trim();
                    }
                    else if (InLine.StartsWith("Sec-WebSocket-Version"))
                    {
                        // Example: "Sec-WebSocket-Version: 8"
                        _Header["Version"] = InLine.Replace("Sec-WebSocket-Version:", "").Trim();
                    }
                    else if (InLine.StartsWith("Upgrade:"))
                    {
                        // Example: "Upgrade: websocket"
                        // NB: New in protocol 8+
                        _Header["Upgrade"] = InLine.Replace("Upgrade:", "").Trim();
                    }
                    else if (InLine.StartsWith("<policy-file-request"))
                    {
                        string PolicyResponse =
                            "<?xml version=\"1.0\"?>\n" +
                            "<cross-domain-policy>\n" +
                            "   <allow-access-from domain=\"*\" to-ports=\"*\"/>\n" +
                            "   <site-control permitted-cross-domain-policies=\"all\"/>\n" +
                            "</cross-domain-policy>\n" +
                            "\0";
                        WriteRaw(Encoding.UTF8.GetBytes(PolicyResponse));
                        FlashPolicyFileRequest = true;
                        return false;
                    }
                }
            }
            catch (Exception ex)
            {
                RMLog.Exception(ex, "Exception in WebSocketConnection::ShakeHands()");
            }

            return false;
        }

        private bool ShakeHandsHixie76()
        {
            // Ensure we have all the data we need
            if ((_Header.ContainsKey("Key1")) && (_Header.ContainsKey("Key2")) && (_Header.ContainsKey("Host")) && (_Header.ContainsKey("Origin")) && (_Header.ContainsKey("Path")))
            {
                List<byte> ToHash = new List<byte>();
                byte[] TempBytes;

                // Get the data to hash
                TempBytes = BitConverter.GetBytes(CalculateWebSocketKey(_Header["Key1"]));
                for (int i = 3; i >= 0; i--) ToHash.Add(TempBytes[i]);
                TempBytes = BitConverter.GetBytes(CalculateWebSocketKey(_Header["Key2"]));
                for (int i = 3; i >= 0; i--) ToHash.Add(TempBytes[i]);
                ToHash.AddRange(ReadBytes(8));

                // Hash the data
                byte[] Hashed = MD5.Create().ComputeHash(ToHash.ToArray());

                // Setup the handshake response
                string Response = "HTTP/1.1 101 Web Socket Protocol Handshake\r\n" +
                                  "Upgrade: WebSocket\r\n" +
                                  "Connection: Upgrade\r\n" +
                                  "Sec-WebSocket-Origin: " + _Header["Origin"] + "\r\n" +
                                  "Sec-WebSocket-Location: ws://" + _Header["Host"] + _Header["Path"] + "\r\n";
                if (_Header.ContainsKey("SubProtocol")) Response += "Sec-WebSocket-Protocol: plain\r\n"; // Only sub-protocol we support
                Response += "\r\n";

                // Send the response and return
                WriteBytes(Encoding.ASCII.GetBytes(Response));
                WriteBytes(Hashed);

                return true;
            }
            else
            {
                // We're missing some pice of data, log what we do have
                RMLog.Error("Missing some piece of handshake data.  Here's what we have:");
                foreach (DictionaryEntry DE in _Header) RMLog.Error(DE.Key + " => " + DE.Value);
                return false;
            }
        }

        private bool ShakeHandsRFC6455()
        {
            // Ensure we have all the data we need
            // TODOX Firefox (v49, maybe others) is not sending an Origin header, which breaks things
            // TODOX if ((_Header.ContainsKey("Key")) && (_Header.ContainsKey("Host")) && (_Header.ContainsKey("Origin")) && (_Header.ContainsKey("Path")))
            if ((_Header.ContainsKey("Key")) && (_Header.ContainsKey("Host")) && (_Header.ContainsKey("Path"))) {
                string AcceptGUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

                // Combine Key and GUID
                string ToHash = _Header["Key"] + AcceptGUID;

                // Hash the string
                byte[] Hashed = SHA1.Create().ComputeHash(Encoding.ASCII.GetBytes(ToHash));

                // Encode the hash
                string Encoded = Convert.ToBase64String(Hashed);

                // Setup the handshake response
                var Response = "HTTP/1.1 101 Switching Protocols\r\n" +
                               "Upgrade: websocket\r\n" +
                               "Connection: Upgrade\r\n" +
                               "Sec-WebSocket-Accept: " + Encoded + "\r\n";
                if (_Header.ContainsKey("SubProtocol")) Response += "Sec-WebSocket-Protocol: plain\r\n"; // Only sub-protocol we support
                Response += "\r\n";

                // Send the response and return
                WriteBytes(Encoding.ASCII.GetBytes(Response));

                return true;
            }
            else
            {
                // We're missing some pice of data, log what we do have
                RMLog.Error("Missing some piece of handshake data.  Here's what we have:");
                foreach (DictionaryEntry DE in _Header) RMLog.Error(DE.Key + " => " + DE.Value);
                return false;
            }
        }

        public string SubProtocol { get; set; }
    }

    /// <summary>
    /// The possible states the websocket negotiator may find itself in
    /// </summary>
    public enum WebSocketNegotiationState
    {
        /// <summary>
        /// Need the start of a new packet/frame
        /// </summary>
        NeedPacketStart,

        /// <summary>
        /// Need the payload length bytes
        /// </summary>
        NeedPayloadLength,

        /// <summary>
        /// Need the masking key bytes
        /// </summary>
        NeedMaskingKey,

        /// <summary>
        /// Receiving data
        /// </summary>
        Data,
    }
}
