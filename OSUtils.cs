/*
  This code file was originally downloaded from the following article:
  C# Detect Windows OS Version – Part 2 (WMI)
  by Andrew October 7th, 2009
  http://andrewensley.com/2009/10/c-detect-windows-os-version-%E2%80%93-part-2-wmi/
  
  It was modified to add basic OSX and Linux support, as well as to add the
  Is* helper functions (IsOSX, IsWindows, IsWinNT, etc)
*/
using System;
using System.Collections.Generic;
using System.Text;
using System.Management;
using System.Text.RegularExpressions;
using System.IO;

namespace RandM.RMLib
{
    public class OSUtils
    {
        public enum Platform
        {
            DOS,
            Linux,
            Windows,
            Unknown
        }

        /// <summary>
        /// Gets Operating System Name, Service Pack, and Architecture using WMI with the legacy methods as a fallback
        /// </summary>
        /// <returns>String containing the name of the operating system followed by its service pack (if any) and architecture</returns>
        public static string GetNameAndVersion()
        {
            //Variables to hold our return value
            string os = "";
            int OSArch = 0;

            if (IsOSX)
            {
                os = "OS X";
            }
            else if (IsUnix)
            {
                try
                {
                    // TODO Handle any -release file
                    string[] LSBRelease = FileUtils.FileReadAllLines("/etc/lsb-release");
                    for (int i = 0; i < LSBRelease.Length; i++)
                    {
                        if (LSBRelease[i].Contains("DISTRIB_DESCRIPTION"))
                        {
                            os = LSBRelease[i].Replace("DISTRIB_DESCRIPTION", "").Trim();
                            os = os.Trim('=');
                            os = os.Trim('"');
                        }
                    }
                }
                catch (Exception)
                {
                    os = "*nix";
                }
            }
            else if (IsWindows)
            {
                try
                {
                    using (ManagementObjectSearcher objMOS = new ManagementObjectSearcher("SELECT * FROM  Win32_OperatingSystem"))
                    {
                        foreach (ManagementObject objManagement in objMOS.Get())
                        {
                            // Get OS version from WMI - This also gives us the edition
                            object osCaption = objManagement.GetPropertyValue("Caption");
                            if (osCaption != null)
                            {
                                // Remove all non-alphanumeric characters so that only letters, numbers, and spaces are left.
                                string osC = Regex.Replace(osCaption.ToString(), "[^A-Za-z0-9 ]", "");

                                // If the OS starts with "Microsoft," remove it.  We know that already
                                if (osC.StartsWith("Microsoft"))
                                {
                                    osC = osC.Substring(9);
                                }

                                // Remove any remaining beginning or ending spaces.
                                os = osC.Trim();

                                // Only proceed if we actually have an OS version - service pack is useless without the OS version.
                                if (!String.IsNullOrEmpty(os))
                                {
                                    object osSP = null;
                                    try
                                    {
                                        // Get OS service pack from WMI
                                        osSP = objManagement.GetPropertyValue("ServicePackMajorVersion");
                                        if (osSP != null && osSP.ToString() != "0")
                                        {
                                            os += " Service Pack " + osSP.ToString();
                                        }
                                        else
                                        {
                                            // Service Pack not found.  Try built-in Environment class.
                                            os += getOSServicePackLegacy();
                                        }
                                    }
                                    catch (Exception)
                                    {
                                        // There was a problem getting the service pack from WMI.  Try built-in Environment class.
                                        os += getOSServicePackLegacy();
                                    }
                                }
                                object osA = null;
                                try
                                {
                                    // Get OS architecture from WMI
                                    osA = objManagement.GetPropertyValue("OSArchitecture");
                                    if (osA != null)
                                    {
                                        string osAString = osA.ToString();
                                        // If "64" is anywhere in there, it's a 64-bit architectore.
                                        OSArch = (osAString.Contains("64") ? 64 : 32);
                                    }
                                }
                                catch (Exception)
                                {
                                }
                            }
                        }
                    }
                }
                catch (Exception)
                {
                }

                // If WMI couldn't tell us the OS, use our legacy method.
                // We won't get the exact OS edition, but something is better than nothing.
                if (string.IsNullOrEmpty(os))
                {
                    os = getOSLegacy();
                }
                // If WMI couldn't tell us the architecture, use our legacy method.
                if (OSArch == 0)
                {
                    OSArch = getOSArchitectureLegacy();
                }
            }
            else
            {
                os = "Unknown OS";
            }

            if (OSArch > 0)
            {
                return os + " " + OSArch.ToString() + "-bit";
            }
            else
            {
                return os;
            }
        }

        /// <summary>
        /// Gets Operating System Name using .Net's Environment class.
        /// </summary>
        /// <returns>String containing the name of the operating system followed by its service pack (if any)</returns>
        private static string getOSLegacy()
        {
            //Get Operating system information.
            OperatingSystem os = Environment.OSVersion;
            //Get version information about the os.
            Version vs = os.Version;

            //Variable to hold our return value
            string operatingSystem = "";

            if (os.Platform == PlatformID.Win32Windows)
            {
                //This is a pre-NT version of Windows
                switch (vs.Minor)
                {
                    case 0:
                        operatingSystem = "95";
                        break;
                    case 10:
                        if (vs.Revision.ToString() == "2222A")
                            operatingSystem = "98SE";
                        else
                            operatingSystem = "98";
                        break;
                    case 90:
                        operatingSystem = "Me";
                        break;
                    default:
                        break;
                }
            }
            else if (os.Platform == PlatformID.Win32NT)
            {
                switch (vs.Major)
                {
                    case 3:
                        operatingSystem = "NT 3.51";
                        break;
                    case 4:
                        operatingSystem = "NT 4.0";
                        break;
                    case 5:
                        if (vs.Minor == 0)
                        {
                            operatingSystem = "2000";
                        }
                        else
                        {
                            operatingSystem = "XP";
                        }
                        break;
                    case 6:
                        if (vs.Minor == 0)
                        {
                            operatingSystem = "Vista";
                        }
                        else
                        {
                            operatingSystem = "7";
                        }
                        break;
                    default:
                        break;
                }
            }
            //Make sure we actually got something in our OS check
            //We don't want to just return " Service Pack 2"
            //That information is useless without the OS version.
            if (!string.IsNullOrEmpty(operatingSystem))
            {
                //Got something.  Let's see if there's a service pack installed.
                operatingSystem += getOSServicePackLegacy();
            }
            //Return the information we've gathered.
            return operatingSystem;
        }

        /// <summary>
        /// Gets the installed Operating System Service Pack using .Net's Environment class.
        /// </summary>
        /// <returns>String containing the operating system's installed service pack (if any)</returns>
        private static string getOSServicePackLegacy()
        {
            // Get service pack from Environment Class
            string sp = Environment.OSVersion.ServicePack;
            if (!string.IsNullOrEmpty(sp) && sp.ToString() != " ")
            {
                // If there's a service pack, return it with a space in front (for formatting)
                return " " + sp.ToString();
            }
            // No service pack.  Return an empty string
            return "";
        }

        /// <summary>
        /// Gets Operating System Architecture.  This does not tell you if the program in running in
        /// 32- or 64-bit mode or if the CPU is 64-bit capable.  It tells you whether the actual Operating
        /// System is 32- or 64-bit.
        /// </summary>
        /// <returns>Int containing 32 or 64 representing the number of bits in the OS Architecture</returns>
        private static int getOSArchitectureLegacy()
        {
            string pa = Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE");
            return ((String.IsNullOrEmpty(pa) || String.Compare(pa, 0, "x86", 0, 3, true) == 0) ? 32 : 64);
        }

        public static bool IsOSX
        {
            get { return Environment.OSVersion.Platform == PlatformID.MacOSX; }
        }

        public static bool IsUnix
        {
            get { return Environment.OSVersion.Platform == PlatformID.Unix; }
        }

        public static bool IsWin2003 {
            get { return IsWinNT && (Environment.OSVersion.Version.Major == 5) && (Environment.OSVersion.Version.Minor == 2); }
        }

        public static bool IsWin9x
        {
            get { return Environment.OSVersion.Platform == PlatformID.Win32Windows; }
        }

        public static bool IsWindows
        {
            get
            {
                if (Environment.OSVersion.Platform == PlatformID.Win32NT) return true;
                if (Environment.OSVersion.Platform == PlatformID.Win32S) return true;
                if (Environment.OSVersion.Platform == PlatformID.Win32Windows) return true;
                return false;
            }
        }

        public static bool IsWinNT
        {
            get { return Environment.OSVersion.Platform == PlatformID.Win32NT; }
        }

        public static bool IsWinXP
        {
            get { return IsWinNT && (Environment.OSVersion.Version.Major == 5) && (Environment.OSVersion.Version.Minor == 1); }
        }
    }
}
