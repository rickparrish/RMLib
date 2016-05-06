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
    public class TelnetConnection : TcpConnection
    {
        private bool[] _TelnetOptions = new bool[255];
        private TelnetNegotiationState _TelnetNegotiationState = TelnetNegotiationState.Data;
        private TelnetOption _SubnegotiationOption = TelnetOption.None;
        private StringBuilder _SubnegotiationData = new StringBuilder();

        // NB: The base class constructor calls InitSocket(), which means this method will run before this classes constructor, so
        //     everything accessed here needs to be initialized already (ie can't rely on the constructor to initialize it)
        protected override void InitSocket()
        {
            base.InitSocket();
            Array.Clear(_TelnetOptions, 0, _TelnetOptions.Length);
            _TelnetNegotiationState = TelnetNegotiationState.Data;
        }

        protected override void NegotiateInbound(byte[] data, int numberOfBytes)
        {
            for (int i = 0; i < numberOfBytes; i++)
            {
                if (_TelnetNegotiationState == TelnetNegotiationState.Data)
                {
                    if (data[i] == (byte)TelnetCommand.IAC)
                    {
                        _TelnetNegotiationState = TelnetNegotiationState.IAC;
                    }
                    else
                    {
                        AddToInputQueue(data[i]);
                    }
                }
                else if (_TelnetNegotiationState == TelnetNegotiationState.IAC)
                {
                    if (data[i] == (byte)TelnetCommand.IAC)
                    {
                        _TelnetNegotiationState = TelnetNegotiationState.Data;
                        AddToInputQueue(data[i]);
                    }
                    else
                    {
                        switch ((TelnetCommand)data[i])
                        {
                            case TelnetCommand.NoOperation:
                            case TelnetCommand.DataMark:
                            case TelnetCommand.Break:
                            case TelnetCommand.InterruptProcess:
                            case TelnetCommand.AbortOutput:
                            case TelnetCommand.AreYouThere:
                            case TelnetCommand.EraseCharacter:
                            case TelnetCommand.EraseLine:
                            case TelnetCommand.GoAhead:
                                // TODO We recognize, but ignore these for now
                                _TelnetNegotiationState = TelnetNegotiationState.Data;
                                break;
                            case TelnetCommand.Do: _TelnetNegotiationState = TelnetNegotiationState.Do; break;
                            case TelnetCommand.Dont: _TelnetNegotiationState = TelnetNegotiationState.Dont; break;
                            case TelnetCommand.Will: _TelnetNegotiationState = TelnetNegotiationState.Will; break;
                            case TelnetCommand.Wont: _TelnetNegotiationState = TelnetNegotiationState.Wont; break;
                            case TelnetCommand.Subnegotiation:
                                _TelnetNegotiationState = TelnetNegotiationState.Subnegotiation;
                                _SubnegotiationData.Length = 0;
                                break;
                            default: _TelnetNegotiationState = TelnetNegotiationState.Data; break;
                        }
                    }
                }
                else if (_TelnetNegotiationState == TelnetNegotiationState.Do)
                {
                    switch ((TelnetOption)data[i])
                    {
                        case TelnetOption.TransmitBinary: SendWill(data[i]); break;
                        case TelnetOption.Echo: SendWill(data[i]); break;
                        case TelnetOption.SuppressGoAhead: SendWill(data[i]); break;
                        case TelnetOption.WindowSize: SendWont(data[i]); break;
                        case TelnetOption.TerminalType: SendWont(data[i]); break;
                        case TelnetOption.LineMode: SendWont(data[i]); break;
                        default: SendWont(data[i]); break;
                    }
                    _TelnetNegotiationState = TelnetNegotiationState.Data;
                }
                else if (_TelnetNegotiationState == TelnetNegotiationState.Dont)
                {
                    switch ((TelnetOption)data[i])
                    {
                        case TelnetOption.TransmitBinary: SendWill(data[i]); break;
                        case TelnetOption.Echo: SendWill(data[i]); break;
                        case TelnetOption.SuppressGoAhead: SendWill(data[i]); break;
                        case TelnetOption.WindowSize: SendWont(data[i]); break;
                        case TelnetOption.TerminalType: SendWont(data[i]); break;
                        case TelnetOption.LineMode: SendWont(data[i]); break;
                        default: SendWont(data[i]); break;
                    }
                    _TelnetNegotiationState = TelnetNegotiationState.Data;
                }
                else if (_TelnetNegotiationState == TelnetNegotiationState.Will)
                {
                    switch ((TelnetOption)data[i])
                    {
                        case TelnetOption.TransmitBinary: SendDo(data[i]); break;
                        case TelnetOption.Echo: SendDont(data[i]); break;
                        case TelnetOption.SuppressGoAhead: SendDo(data[i]); break;
                        case TelnetOption.WindowSize: SendDont(data[i]); break;
                        case TelnetOption.TerminalType: SendSubnegotiate(data[i]); break;
                        case TelnetOption.LineMode: SendDont(data[i]); break;
                        default: SendDont(data[i]); break;
                    }
                    _TelnetNegotiationState = TelnetNegotiationState.Data;
                }
                else if (_TelnetNegotiationState == TelnetNegotiationState.Wont)
                {
                    switch ((TelnetOption)data[i])
                    {
                        case TelnetOption.TransmitBinary: SendDont(data[i]); break;
                        case TelnetOption.Echo: SendDont(data[i]); break;
                        case TelnetOption.SuppressGoAhead: SendDo(data[i]); break;
                        case TelnetOption.WindowSize: SendDont(data[i]); break;
                        case TelnetOption.TerminalType: SendDont(data[i]); break;
                        case TelnetOption.LineMode: SendDont(data[i]); break;
                        default: SendDont(data[i]); break;
                    }
                    _TelnetNegotiationState = TelnetNegotiationState.Data;
                }
                // TODO Port to fTelnet, HtmlTerm
                else if (_TelnetNegotiationState == TelnetNegotiationState.Subnegotiation)
                {
                    // First byte of subnegotiation should be the option to negotiate
                    switch ((TelnetOption)data[i])
                    {
                        case TelnetOption.TerminalType:
                        case TelnetOption.WindowSize:
                            // Known option
                            _SubnegotiationOption = (TelnetOption)data[i];
                            break;
                        default:
                            // Unknown option
                            _SubnegotiationOption = TelnetOption.None;
                            break;
                    }
                    _TelnetNegotiationState = TelnetNegotiationState.SubnegotiationData;
                }
                else if (_TelnetNegotiationState == TelnetNegotiationState.SubnegotiationData)
                {
                    // Add next byte to negotiation string, unless it's an IAC
                    switch ((TelnetCommand)data[i])
                    {
                        case TelnetCommand.IAC: _TelnetNegotiationState = TelnetNegotiationState.SubnegotiationIAC; break;
                        default: _SubnegotiationData.Append((char)data[i]); break;
                    }
                }
                else if (_TelnetNegotiationState == TelnetNegotiationState.SubnegotiationIAC)
                {
                    // Check to see if negotiation has ended, or if it's just a doubled IAC
                    switch ((TelnetCommand)data[i])
                    {
                        case TelnetCommand.IAC: 
                            // Doubled IAC means go back to data
                            _SubnegotiationData.Append((char)data[i]);
                            _TelnetNegotiationState = TelnetNegotiationState.SubnegotiationData;
                            break;
                        case TelnetCommand.EndSubnegotiation:
                            // Subnegotiation has ended
                            // TODO Do something with FSubnegotiationData based on FSubnegotiationOption
                            switch (_SubnegotiationOption)
                            {
                                case TelnetOption.TerminalType:
                                    // TODO
                                    break;
                                case TelnetOption.WindowSize:
                                    // TODO
                                    break;
                            }
                            _TelnetNegotiationState = TelnetNegotiationState.Data;
                            break;
                        default: 
                            // Something unknown has happened
                            _TelnetNegotiationState = TelnetNegotiationState.Data;
                            break;
                    }
                }
                else
                {
                    _TelnetNegotiationState = TelnetNegotiationState.Data;
                }
            }
        }

        protected override void NegotiateOutbound(byte[] data, int numberOfBytes)
        {
            for (int i = 0; i < numberOfBytes; i++)
            {
                // If there's an IAC, double it up
                if (data[i] == (byte)TelnetCommand.IAC) _OutputBuffer.Enqueue((byte)TelnetCommand.IAC);

                _OutputBuffer.Enqueue(data[i]);
            }
        }

        public override bool Open(Socket socket)
        {
            if (base.Open(socket))
            {
                SendWill(TelnetOption.TransmitBinary);
                SendDont(TelnetOption.Echo);
                SendDo(TelnetOption.SuppressGoAhead);
                SendDont(TelnetOption.WindowSize);
                SendDont(TelnetOption.LineMode);
                return true;
            }

            return false;
        }

        public void SendDo(byte option)
        {
            SendIAC(TelnetCommand.Do, option);
        }

        public void SendDo(TelnetOption option)
        {
            SendIAC(TelnetCommand.Do, option);
        }

        public void SendDont(byte option)
        {
            SendIAC(TelnetCommand.Dont, option);
        }

        public void SendDont(TelnetOption option)
        {
            SendIAC(TelnetCommand.Dont, option);
        }

        public void SendGoAhead()
        {
            if (Connected) base.WriteRaw(new byte[] { (byte)TelnetCommand.IAC, (byte)TelnetCommand.GoAhead });
        }

        private void SendIAC(TelnetCommand command, byte option)
        {
            if (Connected)
            {
                if (!_TelnetOptions[option])
                {
                    _TelnetOptions[option] = true;
                    base.WriteRaw(new byte[] { (byte)TelnetCommand.IAC, (byte)command, option });
                }
            }
        }

        private void SendIAC(TelnetCommand command, TelnetOption option)
        {
            SendIAC(command, (byte)option);
        }

        public void SendSubnegotiate(TelnetOption option)
        {
            SendSubnegotiate((byte)option);
        }

        public void SendSubnegotiate(byte option)
        {
            base.WriteRaw(new byte[] { (byte)TelnetCommand.IAC,
                                       (byte)TelnetCommand.Subnegotiation,
                                       option,
                                       1, // TODO This is for SEND in Terminal Type
                                       (byte)TelnetCommand.IAC,
                                       (byte)TelnetCommand.EndSubnegotiation });
        }

        public void SendWill(byte option)
        {
            SendIAC(TelnetCommand.Will, option);
        }

        public void SendWill(TelnetOption option)
        {
            SendIAC(TelnetCommand.Will, option);
        }

        public void SendWont(byte option)
        {
            SendIAC(TelnetCommand.Wont, option);
        }

        public void SendWont(TelnetOption option)
        {
            SendIAC(TelnetCommand.Wont, option);
        }
    }

    /// <summary>
    /// Commands the telnet negotiator will handle
    /// </summary>
    public enum TelnetCommand : byte
    {
        /// <summary>
        /// SE: End of subnegotiation parameters.
        /// </summary>
        EndSubnegotiation = 240,

        /// <summary>
        /// NOP: No operation.
        /// </summary>
        NoOperation = 241,

        /// <summary>
        /// Data Mark: The data stream portion of a Synch. This should always be accompanied by a TCP Urgent notification.
        /// </summary>
        DataMark = 242,

        /// <summary>
        /// Break: NVT character BRK.
        /// </summary>
        Break = 243,

        /// <summary>
        /// Interrupt Process: The function IP.
        /// </summary>
        InterruptProcess = 244,

        /// <summary>
        /// Abort output: The function AO.
        /// </summary>
        AbortOutput = 245,

        /// <summary>
        /// Are You There: The function AYT.
        /// </summary>
        AreYouThere = 246,

        /// <summary>
        /// Erase character: The function EC.
        /// </summary>
        EraseCharacter = 247,

        /// <summary>
        /// Erase Line: The function EL.
        /// </summary>
        EraseLine = 248,

        /// <summary>
        /// Go ahead: The GA signal
        /// </summary>
        GoAhead = 249,

        /// <summary>
        /// SB: Indicates that what follows is subnegotiation of the indicated option.
        /// </summary>
        Subnegotiation = 250,

        /// <summary>
        /// WILL: Indicates the desire to begin performing, or confirmation that you are now performing, the indicated option.
        /// </summary>
        Will = 251,

        /// <summary>
        /// WON'T: Indicates the refusal to perform, or continue performing, the indicated option.
        /// </summary>
        Wont = 252,

        /// <summary>
        /// DO: Indicates the request that the other party perform, or confirmation that you are expecting the other party to perform, the indicated option.
        /// </summary>
        Do = 253,

        /// <summary>
        /// DON'T: Indicates the demand that the other party stop performing, or confirmation that you are no longer expecting the other party to perform, the indicated option.
        /// </summary>
        Dont = 254,

        /// <summary>
        /// IAC: Data Byte 255
        /// </summary>
        IAC = 255
    }

    /// <summary>
    /// The possible states the telnet negotiator may find itself in
    /// </summary>
    public enum TelnetNegotiationState
    {
        /// <summary>
        /// The default data state
        /// </summary>
        Data,

        /// <summary>
        /// The last received character was an IAC
        /// </summary>
        IAC,

        /// <summary>
        /// The last received character was a DO command
        /// </summary>
        Do,

        /// <summary>
        /// The last received character was a DONT command
        /// </summary>
        Dont,

        /// <summary>
        /// The last received character was a WILL command
        /// </summary>
        Will,

        /// <summary>
        /// The last received character was a WONT command
        /// </summary>
        Wont,

        /// <summary>
        /// The last received character was a SUBNEGOTIATION command
        /// </summary>
        Subnegotiation,

        /// <summary>
        /// The last received character was the option to be negotiated
        /// </summary>
        SubnegotiationData,

        /// <summary>
        /// The last received character was an IAC
        /// </summary>
        SubnegotiationIAC
    }

    /// <summary>
    /// Options that can be enabled/disabled in a telnet connection
    /// </summary>
    public enum TelnetOption : sbyte
    {
        /// <summary>
        /// Null option
        /// </summary>
        None = -1,

        /// <summary>
        /// When enabled, data is transmitted as 8-bit binary data.
        /// </summary>
        /// <remarks>
        /// Defined in RFC 856
        /// 
        /// Default is to not transmit in binary.
        /// </remarks>
        TransmitBinary = 0,

        /// <summary>
        /// When enabled, the side performing the echoing transmits (echos) data characters it receives back to the sender of the data characters.
        /// </summary>
        /// <remarks>
        /// Defined in RFC 857
        /// 
        /// Default is to not echo over the telnet connection.
        /// </remarks>
        Echo = 1,

        /// <summary>
        /// When enabled, the sender need not transmit GAs.
        /// </summary>
        /// <remarks>
        /// Defined in RFC 858
        /// 
        /// Default is to not suppress go aheads.
        /// </remarks>
        SuppressGoAhead = 3,

        TerminalType = 24,

        /// <summary>
        /// Allows the NAWS (negotiate about window size) subnegotiation command to be used if both sides agree
        /// </summary>
        /// <remarks>
        /// Defined in RFC 1073
        /// 
        /// Default is to not allow the NAWS subnegotiation
        /// </remarks>
        WindowSize = 31,

        /// <summary>
        /// Linemode Telnet is a way of doing terminal character processing on the client side of a Telnet connection.
        /// </summary>
        /// <remarks>
        /// Defined in RFC 1184
        /// 
        /// Default is to not allow the LINEMODE subnegotiation
        /// </remarks>
        LineMode = 34,
    }
}
