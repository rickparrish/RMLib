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
  
  Some of the functions below were not written by me.  I've identified them by placing a link
  to the source of the code above the function definition
*/
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System;
using System.Globalization;

namespace RandM.RMLib
{
    public class ProcessUtils
    {
        public static string CompanyName
        {
            get
            {
                object[] Attributes = Assembly.GetEntryAssembly().GetCustomAttributes(typeof(AssemblyCompanyAttribute), false);
                if (Attributes.Length == 0)
                {
                    return "";
                }
                else
                {
                    return ((AssemblyCompanyAttribute)Attributes[0]).Company;
                }
            }
        }

        /// <summary>
        /// Returns the full path and filename to the .exe
        /// </summary>
        public static string ExecutablePath
        {
            get { return Assembly.GetEntryAssembly().Location; }
        }

        // From Tergiver's post at:
        // http://social.msdn.microsoft.com/Forums/en/csharpgeneral/thread/24792cdc-2d8e-454b-9c68-31a19892ca53
        public static bool Is64BitOperatingSystem
        {
            get
            {
                // Clearly if this is a 64-bit process we must be on a 64-bit OS.
                if (Is64BitProcess)
                    return true;

                // Ok, so we are a 32-bit process, but is the OS 64-bit?
                // If we are running under Wow64 then the OS is 64-bit.
                bool isWow64;
                return ModuleContainsFunction("kernel32.dll", "IsWow64Process") && NativeMethods.IsWow64Process(Process.GetCurrentProcess().Handle, out isWow64) && isWow64;
            }
        }

        // From Tergiver's post at:
        // http://social.msdn.microsoft.com/Forums/en/csharpgeneral/thread/24792cdc-2d8e-454b-9c68-31a19892ca53
        public static bool Is64BitProcess
        {
            get { return IntPtr.Size == 8; }
        }

        public static bool IsProcessRunning(string AProcessName)
        {
            int ThisProcessID = Process.GetCurrentProcess().Id;

            Process[] Processes = Process.GetProcesses();
            foreach (Process P in Processes)
            {
                try
                {
                    if ((P.ProcessName == AProcessName) && (P.Id != ThisProcessID)) return true;
                }
                catch
                {
                    // Ignore, just means the process has probably ended between when we got the process list, and now
                }
            }
            return false;
        }

        public static bool IsRunningOnMono
        {
            get { return (Type.GetType("Mono.Runtime") != null); }
        }

        // From Tergiver's post at:
        // http://social.msdn.microsoft.com/Forums/en/csharpgeneral/thread/24792cdc-2d8e-454b-9c68-31a19892ca53
        private static bool ModuleContainsFunction(string moduleName, string methodName)
        {
            IntPtr hModule = NativeMethods.GetModuleHandle(moduleName);
            if (hModule != IntPtr.Zero)
                return NativeMethods.GetProcAddress(hModule, methodName) != IntPtr.Zero;
            return false;
        }

        public static string ProductName
        {
            get
            {
                object[] Attributes = Assembly.GetEntryAssembly().GetCustomAttributes(typeof(AssemblyProductAttribute), false);
                if (Attributes.Length == 0)
                {
                    return Assembly.GetEntryAssembly().GetName().Name;
                }
                else
                {
                    return ((AssemblyProductAttribute)Attributes[0]).Product;
                }
            }
        }

        public static string ProductNameOfCallingAssembly
        {
            get
            {
                object[] Attributes = Assembly.GetCallingAssembly().GetCustomAttributes(typeof(AssemblyProductAttribute), false);
                if (Attributes.Length == 0)
                {
                    return Assembly.GetCallingAssembly().GetName().Name;
                }
                else
                {
                    return ((AssemblyProductAttribute)Attributes[0]).Product;
                }
            }
        }

        public static string ProductVersion
        {
            get
            {
                Version V = Assembly.GetEntryAssembly().GetName().Version;
                return StringUtils.PadLeft(V.Major.ToString(), '0', 2) + "." + StringUtils.PadLeft(V.Minor.ToString(), '0', 2) + "." + StringUtils.PadLeft(V.Build.ToString(), '0', 2);
            }
        }

        public static string ProductVersionOfCallingAssembly
        {
            get
            {
                Version V = Assembly.GetCallingAssembly().GetName().Version;
                return StringUtils.PadLeft(V.Major.ToString(), '0', 2) + "." + StringUtils.PadLeft(V.Minor.ToString(), '0', 2) + "." + StringUtils.PadLeft(V.Build.ToString(), '0', 2);
            }
        }

        /// <summary>
        /// Returns the full path (without the filename) of the .exe
        /// </summary>
        public static string StartupPath
        {
            get { return Path.GetDirectoryName(ExecutablePath); }
        }

        public static string Title
        {
            get
            {
                object[] Attributes = Assembly.GetEntryAssembly().GetCustomAttributes(typeof(AssemblyTitleAttribute), false);
                if (Attributes.Length == 0)
                {
                    return ProductName;
                }
                else
                {
                    return ((AssemblyTitleAttribute)Attributes[0]).Title;
                }
            }
        }
    }
}
