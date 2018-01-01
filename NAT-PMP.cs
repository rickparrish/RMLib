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
using System.Net;
using System.Net.NetworkInformation;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace RandM.RMLib
{
    class NATPMP
    {
        public static IPAddress GetExternalIPv4()
        {
            // Get a list of UdpClients that have been "connected" to a valid gateway address
            List<UdpClient> Clients = GetUdpClients();

            // Send public address requests
            SendPublicAddressRequests(Clients);

            // Check for public address responses (will return IPAddress.None after 5 seconds if no response)
            return GetPublicAddressResponse(Clients);
        }

        private static IPAddress GetPublicAddressResponse(List<UdpClient> clients)
        {
            // Loop through that list of clients up to 100 times to check for a discovery response
            IPEndPoint RemoteEndPoint = null;
            for (int i = 0; i < 100; i++)
            {
                foreach (UdpClient Client in clients)
                {
                    try
                    {
                        // Check if this client has a response
                        if (Client.Available > 0)
                        {
                            byte[] ResponseBytes = Client.Receive(ref RemoteEndPoint);
                            if ((ResponseBytes.Length == 12) && (ResponseBytes[0] == 0) && (ResponseBytes[1] == 128) && (IPAddress.NetworkToHostOrder(BitConverter.ToInt16(ResponseBytes, 2)) == 0))
                            {
                                IPAddress Result = new IPAddress(new byte[] { ResponseBytes[8], ResponseBytes[9], ResponseBytes[10], ResponseBytes[11] });
                                if (!WebUtils.IsPrivateIP(Result)) return Result;
                            }

                        }
                    }
                    catch
                    {
                        // Ignore
                    }
                }

                // 100 times through at 50 millisecond sleep each time means we'll wait up to 5 seconds for a response
                Thread.Sleep(50);
            }

            return IPAddress.None;
        }

        private static List<UdpClient> GetUdpClients()
        {
            List<UdpClient> Result = new List<UdpClient>();

            try
            {
                // Loop through each network interface in the system
                foreach (NetworkInterface NI in NetworkInterface.GetAllNetworkInterfaces())
                {
                    // Loop through each gateway address for the current network interface
                    IPInterfaceProperties IPIP = NI.GetIPProperties();
                    foreach (GatewayIPAddressInformation GIPAddress in IPIP.GatewayAddresses)
                    {
                        // Ensure gateway is IPv4, and not IPAddress.ANY or IPAddress.NONE
                        if ((GIPAddress.Address.AddressFamily == AddressFamily.InterNetwork) && (GIPAddress.Address.ToString() != IPAddress.Any.ToString()) && (GIPAddress.Address.ToString() != IPAddress.None.ToString()))
                        {
                            // Gateway is valid, so add new UdpClient
                            try
                            {
                                UdpClient Client = new UdpClient();
                                IPEndPoint RemoteEndPoint = new IPEndPoint(GIPAddress.Address, 5351);
                                Client.Connect(RemoteEndPoint);
                                Result.Add(Client);
                            }
                            catch
                            {
                                // Ignore, don't want a single gateway failure to abort all gateway lookups
                            }
                            break;
                        }
                    }
                }
            }
            catch
            {
                // Ignore, indicates windows system call failure
            }

            return Result;
        }

        private static void SendPublicAddressRequests(List<UdpClient> clients)
        {
            // Loop through that list of clients to send the external ip request 3 times
            foreach (UdpClient Client in clients)
            {
                try
                {
                    // Send request 3 times since UDP doesn't guarantee delivery
                    Client.Send(new byte[] { 0, 0 }, 2);
                    Client.Send(new byte[] { 0, 0 }, 2);
                    Client.Send(new byte[] { 0, 0 }, 2);
                }
                catch
                {
                    // Ignore, don't let single client fail all the clients
                }
            }
        }
    }
}
