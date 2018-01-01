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
using System.Collections.Specialized;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Mail;
using System.Net.NetworkInformation;
using System.Net.Security;
using System.Net.Sockets;
using System.Security.Cryptography.X509Certificates;
using System.Threading;
using System.Web;
using System.Web.Configuration;

namespace RandM.RMLib
{
    public enum GetExternalIPv4Methods
    {
        Http80 = 0,
        Http8880 = 1,
        NatPmp = 2,
        UnicastAddress = 3,
        Upnp = 4
    }

    public static class WebUtils
    {
        public delegate void FTPProgressDelegate(string fileName, long lastBytesSent, long totalBytesSent, long fileSize);

        public static void Email(string smtpHostname, int smtpPort, MailAddress fromAddress, MailAddress toAddress, MailAddress senderAddress, string subject, string body, bool isBodyHtml, string smtpUsername, RMSecureString smtpPassword, bool ssl)
        {
            MailMessage Msg = new MailMessage(fromAddress, toAddress);
            Msg.Sender = senderAddress;
            Msg.Subject = subject;
            Msg.Body = body;
            Msg.IsBodyHtml = isBodyHtml;

            SmtpClient Smtp = new SmtpClient(smtpHostname);
            if ((smtpUsername.Length > 0) && (smtpPassword.Length > 0))
            {
                Smtp.UseDefaultCredentials = false;
                Smtp.Credentials = new NetworkCredential(smtpUsername, smtpPassword.GetPlainText());
            }
            Smtp.DeliveryMethod = SmtpDeliveryMethod.Network;
            Smtp.EnableSsl = ssl;
            Smtp.Port = smtpPort;
            Smtp.Timeout = 10000;
            Smtp.Send(Msg);
        }

        public static bool FtpUpload(bool useSsl, string hostName, string userName, RMSecureString password, string remoteDirectory, string fileName, EventHandler<FtpUploadProgressEventArgs> progressEventHandler)
        {
            FtpWebRequest ftpRequest = null;
            try
            {
                ftpRequest = (FtpWebRequest)WebRequest.Create("ftp://" + hostName + remoteDirectory + Path.GetFileName(fileName));
                ftpRequest.Method = WebRequestMethods.Ftp.UploadFile;
                ftpRequest.Proxy = null;
                ftpRequest.UseBinary = true;
                ftpRequest.UsePassive = true;
                ftpRequest.Credentials = new NetworkCredential(userName, password.GetPlainText());
                ftpRequest.KeepAlive = false;
                //ftpRequest.EnableSsl = ASSL;
                //if (ASSL) ServicePointManager.ServerCertificateValidationCallback = new RemoteCertificateValidationCallback(FtpUploadCertificateValidation);

                FileInfo FI = new FileInfo(fileName);
                using (Stream Reader = FI.OpenRead())
                {
                    using (Stream Writer = ftpRequest.GetRequestStream())
                    {
                        FtpUploadProgressEventArgs e = new FtpUploadProgressEventArgs(FI.Length);
                        byte[] Buffer = new byte[8192];
                        long TotalBytesRead = 0;

                        progressEventHandler(null, e);
                        int BytesRead = Reader.Read(Buffer, 0, Buffer.Length);
                        while (BytesRead > 0)
                        {
                            TotalBytesRead += BytesRead;

                            Writer.Write(Buffer, 0, BytesRead);
                            e.BytesSent = TotalBytesRead;
                            progressEventHandler(null, e);

                            BytesRead = Reader.Read(Buffer, 0, Buffer.Length);
                        }
                    }
                }
                ftpRequest.GetResponse().Close();
                return true;
            }
            catch (Exception)
            {
                return false;
            }
            finally
            {
                ftpRequest = null;
            }
        }

        public static bool FtpUploadCertificateValidation(Object sender, X509Certificate cert, X509Chain chain, SslPolicyErrors errors)
        {
            return true;
        }

        public static IPAddress GetExternalIPv4(GetExternalIPv4Methods[] methodOrder)
        {
            IPAddress Result = IPAddress.None;

            foreach (GetExternalIPv4Methods method in methodOrder)
            {
                try
                {
                    switch (method)
                    {
                        case GetExternalIPv4Methods.Http80:
                            Result = GetExternalIPv4ByHttp(80);
                            break;
                        case GetExternalIPv4Methods.Http8880:
                            Result = GetExternalIPv4ByHttp(8880);
                            break;
                        case GetExternalIPv4Methods.NatPmp:
                            Result = GetExternalIPv4ByNatPmp();
                            break;
                        case GetExternalIPv4Methods.UnicastAddress:
                            Result = GetExternalIPv4ByUnicastAddress();
                            break;
                        case GetExternalIPv4Methods.Upnp:
                            Result = GetExternalIPv4ByUpnp();
                            break;
                    }
                    if (Result != IPAddress.None) break;
                }
                catch
                {
                    // Ignore, move on to the next method type
                }
            }

            return Result;
        }

        public static IPAddress GetExternalIPv4ByHttp(int port)
        {
            using (RMWebClient WC = new RMWebClient())
            {
                WC.Timeout = 5000;
                string IP = WC.DownloadString("http://myip.randm.ca:" + port.ToString() + "/whats-my-ip.php");

                // Return if it's valid
                IPAddress Result = IPAddress.None;
                if (IPAddress.TryParse(IP, out Result) && !WebUtils.IsPrivateIP(Result)) return Result;
            }

            return IPAddress.None;
        }

        public static IPAddress GetExternalIPv4ByNatPmp()
        {
            return NATPMP.GetExternalIPv4();
        }

        public static IPAddress GetExternalIPv4ByUnicastAddress()
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
                        // Gateway is valid, so loop through each unicast address for the current network interface
                        foreach (UnicastIPAddressInformation UIPAddress in NI.GetIPProperties().UnicastAddresses)
                        {
                            // Ensure unicast address is IPv4, and not IPAddress.ANY or IPAddress.NONE
                            if ((UIPAddress.Address.AddressFamily == AddressFamily.InterNetwork) && (UIPAddress.Address.ToString() != IPAddress.Any.ToString()) && (UIPAddress.Address.ToString() != IPAddress.None.ToString()))
                            {
                                if (!IsPrivateIP(UIPAddress.Address)) return UIPAddress.Address;
                            }
                        }
                        break;
                    }
                }
            }

            return IPAddress.None;
        }

        public static IPAddress GetExternalIPv4ByUpnp()
        {
            return UPnP.GetExternalIPv4();
        }

        public static string HttpGet(string url)
        {
            try
            {
                using (RMWebClient WC = new RMWebClient())
                {
                    return WC.DownloadString(url);
                }
            }
            catch (Exception)
            {
                return string.Empty;
            }
        }

        public static string HttpPost(string url, NameValueCollection nameValues)
        {
            bool OldExpect100Continue = ServicePointManager.Expect100Continue;
            ServicePointManager.Expect100Continue = false;

            try
            {
                using (RMWebClient WC = new RMWebClient())
                {
                    return RMEncoding.Ansi.GetString(WC.UploadValues(url, nameValues));
                }
            }
            catch (Exception)
            {
                // TODO Consider not catching this
                return string.Empty;
            }
            finally
            {
                ServicePointManager.Expect100Continue = OldExpect100Continue;
            }
        }

        public static bool IsAdminUserHostAddress()
        {
            if (HttpContext.Current == null) return false;

            List<string> Addresses = new List<string>();

            // Check if we have the IP addresses cached
            if (HttpContext.Current.Cache["IsAdminUserHostAddress"] == null)
            {
                // Nope, so perform the lookup
                string AddressesFromAppSettings = WebConfigurationManager.AppSettings["IsAdminUserHostAddress"];
                foreach (string Address in AddressesFromAppSettings.Split(','))
                {
                    IPAddress[] AddressesFromLookup = Dns.GetHostAddresses(Address);
                    foreach (IPAddress IP in AddressesFromLookup)
                    {
                        Addresses.Add(IP.ToString());
                    }
                }

                // And add them to the cache
                HttpContext.Current.Cache["IsAdminUserHostAddress"] = Addresses;
            }
            else
            {
                Addresses = (List<string>)HttpContext.Current.Cache["IsAdminUserHostAddress"];
            }

            foreach (string Address in Addresses)
            {
                if (Address == HttpContext.Current.Request.UserHostAddress) return true;
            }

            return false;
        }

        public static bool IsPrivateIP(IPAddress ipAddress)
        {
            if (ipAddress.AddressFamily == AddressFamily.InterNetwork)
            {
                // Check in order of what I expect is most common to least common
                string[] Octets = ipAddress.ToString().Split(new char[] { '.' });
                if (Octets[0] == "192" && Octets[1] == "168")
                {
                    return true;
                }
                else if (Octets[0] == "169" && Octets[1] == "254")
                {
                    return true;
                }
                else if (Octets[0] == "127")
                {
                    return true;
                }
                else if (Octets[0] == "10")
                {
                    return true;
                }
                else if (Octets[0] == "172" && int.Parse(Octets[1]) >= 16 && int.Parse(Octets[1]) <= 31)
                {
                    return true;
                }
                else if (ipAddress.ToString() == IPAddress.Any.ToString())
                {
                    return true;
                }
                else if (ipAddress.ToString() == IPAddress.None.ToString())
                {
                    return true;
                }

                return false;
            }
            else if (ipAddress.AddressFamily == AddressFamily.InterNetworkV6)
            {
                throw new NotImplementedException("I dont speak IPv6 yet");
            }
            else
            {
                throw new ArgumentOutOfRangeException("ipAddress");
            }
        }

        public static void ParseHostPort(string input, ref string hostname, ref int port)
        {
            int ColonCount = input.Count(x => x == ':');
            if (ColonCount == 0)
            {
                // No colons means either hostname or IPv4 address without a port
                hostname = input;
            }
            else if (ColonCount == 1)
            {
                // 1 colon means either hostname:port or IPv4:port
                hostname = input.Split(':')[0];
                port = int.Parse(input.Split(':')[1]);
            }
            else
            {
                // More than one colon means IPv6 address, possibly with a port
                if (input.Contains("[") && input.Contains("]"))
                {
                    // Having [] means we could have a [IPv6]:port
                    if (input.LastIndexOf(']') < input.LastIndexOf(':'))
                    {
                        // [IPv6]:port
                        hostname = input.Substring(0, input.LastIndexOf(':'));
                        port = int.Parse(input.Substring(input.LastIndexOf(':') + 1));
                    }
                    else
                    {
                        // [IPv6]
                        hostname = input;
                    }
                }
                else
                {
                    // No [] means we only have an IPv6 without a port (or we do have a port, but that's not valid because you need to use [] to add a port)
                    hostname = input;
                }
            }
        }

        public static void ParseHostPortTest()
        {
            string input = "127.0.0.1";
            string hostname = "";
            int port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");

            input = "127.0.0.1:23";
            hostname = "";
            port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");

            input = "[0:1:2:3::4]";
            hostname = "";
            port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");

            input = "[0:1:2:3::4]:23";
            hostname = "";
            port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");

            input = "host";
            hostname = "";
            port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");

            input = "host:23";
            hostname = "";
            port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");

            input = "0:1:2:3::4";
            hostname = "";
            port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");

            input = "0:1:2:3::4:23";
            hostname = "";
            port = 0;
            WebUtils.ParseHostPort(input, ref hostname, ref port);
            Console.WriteLine($"input={input}, hostname={hostname}, port={port}");
        }

        public static bool Ping(string hostName, int timeout)
        {
            try
            {
                using (Ping P = new Ping())
                {
                    return (P.Send(hostName, timeout).Status == IPStatus.Success);
                }
            }
            catch (Exception)
            {
                return false;
            }
        }

        public static string UrlEncode(string url)
        {
            const string NO_ENCODE = "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
            string Result = "";
            for (int i = 0; i < url.Length; i++)
            {
                if (NO_ENCODE.Contains(url[i].ToString()))
                {
                    Result += url[i];
                }
                else
                {
                    Result += "%" + ((byte)url[i]).ToString("X2");
                }
            }
            return Result;
        }
    }
}
