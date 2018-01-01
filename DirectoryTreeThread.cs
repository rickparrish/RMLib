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
using System.Text;
using RandM.RMLib;
using System.IO;
using System.Text.RegularExpressions;

namespace RandM.RMLib
{
    public class DirectoryTreeThread : RMThread
    {
        public event EventHandler<StringEventArgs> DirectoryChanged = null;
        public event EventHandler<StringEventArgs> LogError = null;
        public event EventHandler<StringEventArgs> LogMessage = null;

        public SortedDictionary<string, string> Directories { get; private set; }
        public SortedDictionary<string, FileData> Files { get; private set; }

        private string _Directory = null;
        private string _ReplaceInKey = null;
        private bool _Recurse = false;
        private bool _WantFiles = false;

        public DirectoryTreeThread(string directory, string replaceInKey, bool recurse, bool wantFiles)
        {
            _Directory = directory;
            if (!_Directory.EndsWith(Path.DirectorySeparatorChar.ToString())) _Directory += Path.DirectorySeparatorChar;

            _ReplaceInKey = Regex.Escape(replaceInKey);

            _Recurse = recurse;

            _WantFiles = wantFiles;

            Directories = new SortedDictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);
            Files = new SortedDictionary<string, FileData>(StringComparer.CurrentCultureIgnoreCase);
        }

        protected override void Execute()
        {
            List<DirectoryInfo> ToSearch = new List<DirectoryInfo>();

            // Add directory to queue
            try
            {
                ToSearch.Add(new DirectoryInfo(_Directory));
            }
            catch (DirectoryNotFoundException dnf)
            {
                RaiseLogMessage("HANDLED DIRECTORYNOTFOUNDEXCEPTION while trying to read directory '" + _Directory + "': " + dnf.Message);
            }
            catch (Exception e)
            {
                RaiseLogError("UNHANDLED EXCEPTION while trying to read directory '" + _Directory + "': " + e.Message);
            }

            while (ToSearch.Count > 0)
            {
                DirectoryInfo DI = ToSearch[0];
                ToSearch.RemoveAt(0);
                Directories.Add(Regex.Replace(DI.FullName, _ReplaceInKey, "", RegexOptions.IgnoreCase), DI.FullName);

                RaiseDirectoryChanged(DI.FullName);

                if (_WantFiles)
                {
                    try
                    {
                        foreach (FileData FD in FastDirectoryEnumerator.EnumerateFiles(DI.FullName, "*.*", _Recurse ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly))
                        {
                            Files.Add(Regex.Replace(FD.Path, _ReplaceInKey, "", RegexOptions.IgnoreCase), FD);
                        }
                    }
                    catch (DirectoryNotFoundException dnf)
                    {
                        RaiseLogMessage("HANDLED DIRECTORYNOTFOUNDEXCEPTION while trying to read files in directory '" + DI.FullName + "': " + dnf.Message);
                    }
                    catch (Exception e)
                    {
                        RaiseLogError("UNHANDLED EXCEPTION while trying to read files in directory '" + DI.FullName + "': " + e.Message);
                    }
                }

                if (_Recurse)
                {
                    try
                    {
                        ToSearch.AddRange(DI.GetDirectories());
                    }
                    catch (DirectoryNotFoundException dnf)
                    {
                        RaiseLogMessage("HANDLED DIRECTORYNOTFOUNDEXCEPTION while trying to read subdirectories in directory '" + DI.FullName + "': " + dnf.Message);
                    }
                    catch (Exception e)
                    {
                        RaiseLogError("UNHANDLED EXCEPTION while trying to read subdirectories in directory '" + DI.FullName + "': " + e.Message);
                    }
                }
            }
        }

        private void RaiseDirectoryChanged(string newDirectory)
        {
            EventHandler<StringEventArgs> Handler = DirectoryChanged;
            if (Handler != null) Handler(this, new StringEventArgs(Regex.Replace(newDirectory, _ReplaceInKey, "", RegexOptions.IgnoreCase)));
        }

        private void RaiseLogError(string message)
        {
            EventHandler<StringEventArgs> Handler = LogError;
            if (Handler != null) Handler(this, new StringEventArgs(message));
        }

        private void RaiseLogMessage(string message)
        {
            EventHandler<StringEventArgs> Handler = LogMessage;
            if (Handler != null) Handler(this, new StringEventArgs(message));
        }
    }
}
