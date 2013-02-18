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
using System.Diagnostics;
using System.Text;
using System.Threading;

namespace RandM.RMLib
{
    public class RMProcess : Process
    {
        public event EventHandler<RMProcessStartAndWaitEventArgs> ProcessStartEvent = null;
        public event EventHandler<RMProcessStartAndWaitEventArgs> ProcessWaitEvent = null;

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

        new static public Process Start(ProcessStartInfo startInfo)
        {
            if (startInfo.WindowStyle == ProcessWindowStyle.Hidden) startInfo.CreateNoWindow = true;
            startInfo.UseShellExecute = false;

            Process P = Process.Start(startInfo);

            if (OSUtils.IsWindows)
            {
                if ((startInfo.WindowStyle != ProcessWindowStyle.Hidden) && (startInfo.WindowStyle != ProcessWindowStyle.Normal))
                {
                    int retry = 10;
                    while (retry > 0)
                    {
                        P.Refresh();
                        if (P.MainWindowHandle != IntPtr.Zero)
                        {
                            switch (startInfo.WindowStyle)
                            {
                                case ProcessWindowStyle.Hidden:
                                    NativeMethods.ShowWindow(P.MainWindowHandle, NativeMethods.SW_HIDE);
                                    retry = 0;
                                    break;
                                case ProcessWindowStyle.Maximized:
                                    NativeMethods.ShowWindow(P.MainWindowHandle, NativeMethods.SW_MAXIMIZE);
                                    retry = 0;
                                    break;
                                case ProcessWindowStyle.Minimized:
                                    NativeMethods.ShowWindow(P.MainWindowHandle, NativeMethods.SW_MINIMIZE);
                                    retry = 0;
                                    break;
                            }
                        }
                        Thread.Sleep(100);
                        retry -= 1;
                    }
                }
            }

            return P;
        }

        public void StartAndWait(ProcessStartInfo startInfo)
        {
            // And start the process and wait for it to quit
            using (Process P = RMProcess.Start(startInfo))
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
    }
}
