/*
  RMLib: Nonvisual support classes used by multiple R&M Software programs
  Copyright (C) 2008-2013  Rick Parrish, R&M Software

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
using System.Collections.Generic;
using System.Text;
using System.Net.Sockets;
using System.Net;
using System.Xml;
using System.IO;
using System.Net.NetworkInformation;
using System.Threading;

namespace RandM.RMLib
{
    static public class UPnP
    {
        static private bool _WANPPP = false;

        static public IPAddress GetExternalIPv4()
        {
            // Get a list of UdpClients that have been bound to local addresses that exist on network interfaces with a valid gateway address
            List<UdpClient> Clients = GetUdpClients();

            // Send discovery messages
            SendDiscoveryRequests(Clients);

            // Check for discovery response
            string DescriptionUrl = GetDiscoveryResponse(Clients);
            if (string.IsNullOrEmpty(DescriptionUrl)) return IPAddress.None;

            // Get service url
            string ServiceUrl = GetServiceUrl(DescriptionUrl);
            if (string.IsNullOrEmpty(ServiceUrl)) return IPAddress.None;

            // Request external ip (will return IPAddress.None after 5 seconds)
            return GetExternalIPAddress(ServiceUrl);
        }

        private static string GetDiscoveryResponse(List<UdpClient> clients)
        {
            // Loop through that list of clients up to 100 times to check for a discovery response
            for (int i = 0; i < 100; i++)
            {
                foreach (UdpClient Client in clients)
                {
                    try
                    {
                        // Check if this client has a response
                        if (Client.Available > 0)
                        {
                            // Read the response
                            IPEndPoint RemoteEndPoint = null;
                            byte[] ResponseBytes = Client.Receive(ref RemoteEndPoint);
                            string ResponseText = Encoding.ASCII.GetString(ResponseBytes);

                            // Ensure we're dealing with an internet gateway device
                            if (ResponseText.ToLower().Contains("urn:schemas-upnp-org:device:internetgatewaydevice:"))
                            {
                                // Parse out this line "LOCATION: http://192.168.0.1:5000/rootDesc.xml", trimming the leading "LOCATION:" text
                                string Location = ResponseText.Substring("location:".Length + ResponseText.IndexOf("location", StringComparison.OrdinalIgnoreCase)).Split('\n')[0].Trim();
                                if (Location.ToLower().StartsWith("http://")) return Location;
                            }
                        }
                    }
                    catch
                    {
                        // Ignore, don't let a single client abort all the rest
                    }
                }

                // 100 times through at 50 millisecond sleep each time means we'll wait up to 5 seconds for a response
                Thread.Sleep(50);
            }

            return null;
        }

        private static IPAddress GetExternalIPAddress(string serviceUrl)
        {
            string RequestText = "<?xml version=\"1.0\"?>" +
                                 "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\">" +
                                 "<s:Body>" +
                                 "<u:GetExternalIPAddress xmlns:u=\"urn:schemas-upnp-org:service:WAN" + (_WANPPP ? "PPP" : "IP") + "Connection:1\"></u:GetExternalIPAddress>" +
                                 "</s:Body>" +
                                 "</s:Envelope>";
            byte[] RequestBytes = Encoding.ASCII.GetBytes(RequestText);

            // Request the router tell us the external ip
            byte[] ResponseBytes = null;
            string ResponseText = null;
            using (RMWebClient WC = new RMWebClient())
            {
                WC.ContentType = "text/xml; charset=\"utf-8\"";
                WC.Headers.Add("SOAPACTION", "\"urn:schemas-upnp-org:service:WAN" + (_WANPPP ? "PPP" : "IP") + "Connection:1#GetExternalIPAddress\"");
                WC.Timeout = 5000;
                ResponseBytes = WC.UploadData(serviceUrl, RequestBytes);
                ResponseText = Encoding.ASCII.GetString(ResponseBytes);
            }

            // Load xml into parser
            XmlDocument XmlDoc = new XmlDocument();
            XmlDoc.LoadXml(ResponseText);

            // Add namespace
            XmlNamespaceManager NSManager = new XmlNamespaceManager(XmlDoc.NameTable);
            NSManager.AddNamespace("tns", "urn:schemas-upnp-org:device-1-0");

            // Get IP
            string IP = XmlDoc.SelectSingleNode("//NewExternalIPAddress/text()", NSManager).Value;

            // Return if it's valid
            IPAddress Result = null;
            if (IPAddress.TryParse(IP, out Result) && !WebUtils.IsPrivateIP(Result))
            {
                return Result;
            }
            else
            {
                return IPAddress.None;
            }
        }

        private static string GetServiceUrl(string descriptionUrl)
        {
            // Download description file from router
            string ResponseText = "";
            using (RMWebClient WC = new RMWebClient())
            {
                WC.Timeout = 5000;
                ResponseText = WC.DownloadString(descriptionUrl);
            }

            // Load xml into parser
            XmlDocument XmlDoc = new XmlDocument();
            XmlDoc.LoadXml(ResponseText);

            // Add namespace
            XmlNamespaceManager NSManager = new XmlNamespaceManager(XmlDoc.NameTable);
            NSManager.AddNamespace("tns", "urn:schemas-upnp-org:device-1-0");

            // Find deviceType node and ensure it's an internet gateway device
            XmlNode DeviceTypeNode = XmlDoc.SelectSingleNode("//tns:device/tns:deviceType/text()", NSManager);
            if (!DeviceTypeNode.Value.ToLower().Contains("internetgatewaydevice")) return null;

            // Find controlURL node
            XmlNode ControlUrlNode = XmlDoc.SelectSingleNode("//tns:service[tns:serviceType=\"urn:schemas-upnp-org:service:WANIPConnection:1\"]/tns:controlURL/text()", NSManager);
            if (ControlUrlNode == null)
            {
                _WANPPP = true;
                ControlUrlNode = XmlDoc.SelectSingleNode("//tns:service[tns:serviceType=\"urn:schemas-upnp-org:service:WANPPPConnection:1\"]/tns:controlURL/text()", NSManager);
                if (ControlUrlNode == null) return null;
            }

            // Return the path to the service url
            string BaseUrl = descriptionUrl.Substring(7); // Trim http://
            BaseUrl = BaseUrl.Substring(0, BaseUrl.IndexOf("/")); // Trim everything after first /
            if (ControlUrlNode.Value.StartsWith("/"))
            {
                return "http://" + BaseUrl + ControlUrlNode.Value;
            }
            else
            {
                return "http://" + BaseUrl + "/" + ControlUrlNode.Value;
            }
        }

        static private List<UdpClient> GetUdpClients()
        {
            List<UdpClient> Result = new List<UdpClient>();

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
                        // Gateway is valid, so loop through each unicast address for the current network interface
                        foreach (UnicastIPAddressInformation UIPAddress in NI.GetIPProperties().UnicastAddresses)
                        {
                            // Ensure unicast address is IPv4, and not IPAddress.ANY or IPAddress.NONE
                            if ((UIPAddress.Address.AddressFamily == AddressFamily.InterNetwork) && (UIPAddress.Address.ToString() != IPAddress.Any.ToString()) && (UIPAddress.Address.ToString() != IPAddress.None.ToString()))
                            {
                                try
                                {
                                    Result.Add(new UdpClient(new IPEndPoint(UIPAddress.Address, 0)));
                                }
                                catch
                                {
                                    // Ignore
                                }
                            }
                        }
                        break;
                    }
                }
            }

            return Result;
        }

        private static void SendDiscoveryRequests(List<UdpClient> clients)
        {
            IPEndPoint BroadcastEndPoint1900 = new IPEndPoint(IPAddress.Broadcast, 1900);
            string DiscoverText = "M-SEARCH * HTTP/1.1\r\n" +
                                                 "HOST: 239.255.255.250:1900\r\n" +
                                                 "MAN: \"ssdp:discover\"\r\n" +
                                                 "MX: 1\r\n" +
                                                 "ST: ssdp:all\r\n\r\n";
            byte[] DiscoverBytes = Encoding.ASCII.GetBytes(DiscoverText);

            // Loop through that list of clients to send the discovery message 3 times each
            foreach (UdpClient Client in clients)
            {
                try
                {
                    // UPNP spec says to send 3 requests since UDP is unreliable (although a single hop to a router should be fairly reliable, i would hope!)
                    Client.Send(DiscoverBytes, DiscoverBytes.Length, BroadcastEndPoint1900);
                    Client.Send(DiscoverBytes, DiscoverBytes.Length, BroadcastEndPoint1900);
                    Client.Send(DiscoverBytes, DiscoverBytes.Length, BroadcastEndPoint1900);
                }
                catch
                {
                    // Ignore
                }
            }
        }

    }
}
