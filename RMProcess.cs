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
  
  This is a stripped down version of the Process.cs file from the .NET framework 2.0 source code.
  The standard Process class does not allow you to CREATE_NEW_CONSOLE, so the sole purpose
  of this class is to enable that creation flag.
  
  There were a couple other additions/modifications made by me, but for the most part this is not
  my code and I take no credit for it.
*/
using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.ComponentModel;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Collections.Specialized;
using System.Collections;

namespace RandM.RMLib
{
    /// <summary>
    /// Stripped down version of Process.cs cobbled together from the .NET framework 2.0 source code
    /// </summary>
    public class RMProcess : IDisposable
    {
        public event EventHandler<RMProcessStartAndWaitEventArgs> ProcessStartEvent = null;
        public event EventHandler<RMProcessStartAndWaitEventArgs> ProcessWaitEvent = null;
        
        private bool _Disposed = false;
        private int _ExitCode = -1;
        private bool _Exited = false;
        private IntPtr _ProcessHandle = IntPtr.Zero;
        private bool _HaveProcessHandle = false;
        private bool _Signaled = false;
        private ProcessStartInfo _StartInfo = null;

        ~RMProcess()
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
                }

                // Call the appropriate methods to clean up
                // unmanaged resources here.
                // If disposing is false,
                // only the following code is executed.
                Close();

                // Note disposing has been done.
                _Disposed = true;
            }
        }

        private static StringBuilder BuildCommandLine(string executableFileName, string arguments)
        {
            // Construct a StringBuilder with the appropriate command line
            // to pass to CreateProcess.  If the filename isn't already 
            // in quotes, we quote it here.  This prevents some security 
            // problems (it specifies exactly which part of the string
            // is the file to execute). 
            StringBuilder commandLine = new StringBuilder();
            string fileName = executableFileName.Trim();
            bool fileNameIsQuoted = (fileName.StartsWith("\"") && fileName.EndsWith("\"", StringComparison.Ordinal));
            if (!fileNameIsQuoted)
            {
                commandLine.Append("\"");
            }

            commandLine.Append(fileName);

            if (!fileNameIsQuoted)
            {
                commandLine.Append("\"");
            }

            if (!String.IsNullOrEmpty(arguments))
            {
                commandLine.Append(" ");
                commandLine.Append(arguments);
            }

            return commandLine;
        }

        public void Close()
        {
            if (_HaveProcessHandle)
            {
                NativeMethods.CloseHandle(_ProcessHandle);
                _ProcessHandle = IntPtr.Zero;
                _HaveProcessHandle = false;
            }

            _ExitCode = -1;
            _Exited = false;
            _Signaled = false;
        }

        public int ExitCode
        {
            get
            {
                return _ExitCode;
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
        public bool HasExited
        {
            get
            {
                if (!_Exited)
                {
                    if (_HaveProcessHandle)
                    {
                        int ExitCode;

                        // Although this is the wrong way to check whether the process has exited, 
                        // it was historically the way we checked for it, and a lot of code then took a dependency on
                        // the fact that this would always be set before the pipes were closed, so they would read 
                        // the exit code out after calling ReadToEnd() or standard output or standard error. In order 
                        // to allow 259 to function as a valid exit code and to break as few people as possible that
                        // took the ReadToEnd dependency, we check for an exit code before doing the more correct 
                        // check to see if we have been signalled.
                        if (NativeMethods.GetExitCodeProcess(_ProcessHandle, out ExitCode) && ExitCode != NativeMethods.STILL_ACTIVE)
                        {
                            this._Exited = true;
                            this._ExitCode = ExitCode;
                        }
                        else
                        {
                            // The best check for exit is that the kernel process object handle is invalid,
                            // or that it is valid and signaled.  Checking if the exit code != STILL_ACTIVE 
                            // does not guarantee the process is closed,
                            // since some process could return an actual STILL_ACTIVE exit code (259).
                            if (!_Signaled) // if we just came from WaitForExit, don't repeat
                            {
                                _Signaled = (NativeMethods.WaitForSingleObject(_ProcessHandle, 0) == NativeMethods.WAIT_OBJECT_0);
                            }

                            if (_Signaled)
                            {
                                if (!NativeMethods.GetExitCodeProcess(_ProcessHandle, out ExitCode))
                                    throw new Win32Exception();

                                this._Exited = true;
                                this._ExitCode = ExitCode;
                            }
                        }
                    }
                    else
                    {
                        throw new InvalidOperationException("Process is not running");
                    }
                }

                return _Exited;
            }
        }

        public void Kill()
        {
            if (_HaveProcessHandle)
            {
                if (!NativeMethods.TerminateProcess(_ProcessHandle, -1))
                    throw new Win32Exception();
                Close();
            }
        }

        private void RaiseStartEvent()
        {
            EventHandler<RMProcessStartAndWaitEventArgs> Handler = ProcessStartEvent;
            if (Handler != null) Handler(this, new RMProcessStartAndWaitEventArgs());
        }

        private bool RaiseWaitEvent()
        {
            EventHandler<RMProcessStartAndWaitEventArgs> Handler = ProcessWaitEvent;
            if (Handler != null)
            {
                RMProcessStartAndWaitEventArgs e = new RMProcessStartAndWaitEventArgs();
                Handler(this, e);
                return e.Stop;
            }

            return false;
        }

        public static RMProcess Start(ProcessStartInfo startInfo)
        {
            if (startInfo == null) throw new ArgumentNullException("startInfo");
            startInfo.UseShellExecute = false;
            RMProcess process = new RMProcess();
            process.StartInfo = startInfo;
            if (process.Start())
            {
                return process;
            }
            return null;
        }

        public bool Start()
        {
            Close();
            ProcessStartInfo startInfo = StartInfo;
            if (startInfo.FileName.Length == 0)
                throw new InvalidOperationException("FileName is missing");

            if (startInfo.UseShellExecute)
            {
                throw new InvalidOperationException("Use plain old Process class if you want ShellExecute");
            }
            else
            {
                return StartWithCreateProcess(startInfo);
            }
        }

        public void StartAndWait(ProcessStartInfo startInfo)
        {
            // And start the process and wait for it to quit
            using (RMProcess P = RMProcess.Start(startInfo))
            {
                RaiseStartEvent();

                bool Stop = false;
                while ((!Stop) && (!P.HasExited))
                {
                    P.WaitForExit(100);
                    Stop = RaiseWaitEvent();
                }

                if (!P.HasExited) P.Kill();
            }
        }

        public ProcessStartInfo StartInfo
        {
            get
            {
                if (_StartInfo == null)
                {
                    _StartInfo = new ProcessStartInfo();
                }
                return _StartInfo;
            }
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException("value");
                }
                _StartInfo = value;
            }
        }

        private bool StartWithCreateProcess(ProcessStartInfo startInfo)
        {
            if (startInfo.RedirectStandardInput)
            {
                throw new InvalidOperationException("Standard input redirect not allowed");
            }

            if (startInfo.RedirectStandardOutput)
            {
                throw new InvalidOperationException("Standard output redirect not allowed");
            }

            if (startInfo.RedirectStandardError)
            {
                throw new InvalidOperationException("Standard error redirect not allowed");
            }

            //Cannot start a new process and store its handle if the object has been disposed, since finalization has been suppressed.
            if (this._Disposed)
            {
                throw new ObjectDisposedException(GetType().Name);
            }

            StringBuilder commandLine = BuildCommandLine(startInfo.FileName, startInfo.Arguments);

            NativeMethods.STARTUPINFO startupInfo = new NativeMethods.STARTUPINFO();
            NativeMethods.PROCESS_INFORMATION processInfo = new NativeMethods.PROCESS_INFORMATION();

            bool retVal;
            int errorCode = 0;

            // set up the creation flags paramater
            int creationFlags = 0;
            if (startInfo.CreateNoWindow)
            {
                creationFlags |= (int)NativeMethods.CreateProcessFlags.CREATE_NO_WINDOW;
                startupInfo.dwFlags = NativeMethods.STARTF_USESHOWWINDOW;
                startupInfo.wShowWindow = (short)NativeMethods.SW_HIDE;
            }
            else
            {
                creationFlags |= (int)NativeMethods.CreateProcessFlags.CREATE_NEW_CONSOLE;
                switch (startInfo.WindowStyle)
                {
                    case ProcessWindowStyle.Maximized:
                        startupInfo.dwFlags = NativeMethods.STARTF_USESHOWWINDOW;
                        startupInfo.wShowWindow = (short)NativeMethods.SW_MAXIMIZE;
                        break;
                    case ProcessWindowStyle.Minimized:
                        startupInfo.dwFlags = NativeMethods.STARTF_USESHOWWINDOW;
                        startupInfo.wShowWindow = (short)NativeMethods.SW_MINIMIZE;
                        break;
                    case ProcessWindowStyle.Hidden:
                        startupInfo.dwFlags = NativeMethods.STARTF_USESHOWWINDOW;
                        startupInfo.wShowWindow = (short)NativeMethods.SW_HIDE;
                        break;
                }
            }

            string workingDirectory = startInfo.WorkingDirectory;
            if (workingDirectory == string.Empty)
                workingDirectory = ProcessUtils.StartupPath;

            RuntimeHelpers.PrepareConstrainedRegions();
            try { }
            finally
            {
                retVal = NativeMethods.CreateProcess(
                        null,               // we don't need this since all the info is in commandLine 
                        commandLine.ToString(),        // pointer to the command line string
                        IntPtr.Zero,               // pointer to process security attributes, we don't need to inheriat the handle 
                        IntPtr.Zero,               // pointer to thread security attributes 
                        true,               // handle inheritance flag
                        creationFlags,      // creation flags 
                        IntPtr.Zero,               // pointer to new environment block
                        workingDirectory,   // pointer to current directory name
                        ref startupInfo,        // pointer to STARTUPINFO
                        out processInfo         // pointer to PROCESS_INFORMATION 
                    );
                if (!retVal)
                    errorCode = Marshal.GetLastWin32Error();
            }
            if (!retVal)
            {
                if (errorCode == NativeMethods.ERROR_BAD_EXE_FORMAT)
                {
                    throw new Win32Exception(errorCode, "Invalid application");
                }
                throw new Win32Exception(errorCode);
            }


            bool ret = false;
            if (processInfo.hProcess != (IntPtr)0 && processInfo.hProcess != (IntPtr)NativeMethods.INVALID_HANDLE_VALUE)
            {
                _ProcessHandle = processInfo.hProcess;
                _HaveProcessHandle = true;
                if (processInfo.hThread != (IntPtr)0 && processInfo.hThread != (IntPtr)NativeMethods.INVALID_HANDLE_VALUE)
                    NativeMethods.CloseHandle(processInfo.hThread);
                ret = true;
            }

            return ret;
        }

        public bool WaitForExit(int milliseconds)
        {
            if (!_Signaled)
            {
                _Signaled = (NativeMethods.WaitForSingleObject(_ProcessHandle, milliseconds) == NativeMethods.WAIT_OBJECT_0);
            }

            return _Signaled;
        }
    }
}
