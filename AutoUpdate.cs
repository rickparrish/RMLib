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
using System.Diagnostics;
using System.IO;
using System.Globalization;

namespace RandM.RMLib
{
    static public class AutoUpdate
    {
        static public string Comments { get; set; }
        static public string Url { get; set; }
        static public string Version { get; set; }

        static public bool Available(Uri updateUrl)
        {
            // Don't auto-update in the debugger
            if (Debugger.IsAttached)
            {
                // In debug mode we don't want to actually request the file
                Version = ProcessUtils.ProductVersion;
                Url = null;
                Comments = "";
                return false;
            }
            else
            {
                // TODO Display a form that shows we're doing something, otherwise it looks like the application hangs if it takes awhile to do the check
                string AutoUpdateIniFile = StringUtils.PathCombine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), ProcessUtils.CompanyName, "AutoUpdate.ini");
                using (IniFile AutoUpdateIni = new IniFile(AutoUpdateIniFile))
                {
                    // Check if there's an old installer to delete
                    string OldUrl = AutoUpdateIni.ReadString(ProcessUtils.ProductName, "URL", "");
                    if (OldUrl.Length > 0)
                    {
                        string OldInstaller = StringUtils.PathCombine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), ProcessUtils.CompanyName, Path.GetFileName(OldUrl));
                        FileUtils.FileDelete(OldInstaller);
                    }

                    // Retrieve the new INI from the updater website
                    string NewIniFile = StringUtils.LFtoCRLF(WebUtils.HttpGet(AutoUpdateIni.ReadString("config", "URL", updateUrl.ToString()) + "?Name=" + WebUtils.UrlEncode(ProcessUtils.ProductName) + "&Version=" + WebUtils.UrlEncode(ProcessUtils.ProductVersion)));
                    if (!string.IsNullOrEmpty(NewIniFile))
                    {
                        // Save and re-open the new INI
                        Directory.CreateDirectory(Path.GetDirectoryName(AutoUpdateIniFile));
                        FileUtils.FileWriteAllText(AutoUpdateIniFile, NewIniFile);
                        using (IniFile NewIni = new IniFile(AutoUpdateIniFile))
                        {
                            // Read the version, url, and comments from the latest ini
                            Version = NewIni.ReadString(ProcessUtils.ProductName, "version", ProcessUtils.ProductVersion);
                            Url = NewIni.ReadString(ProcessUtils.ProductName, "URL", "");
                            int CommentCount = NewIni.ReadInt32(ProcessUtils.ProductName, "comments", 0);
                            if (CommentCount > 0)
                            {
                                Comments = NewIni.ReadString(ProcessUtils.ProductName, "comment1", "");
                                for (int i = 2; i <= CommentCount; i++)
                                {
                                    Comments += Environment.NewLine + NewIni.ReadString(ProcessUtils.ProductName, "comment" + i.ToString(), "");
                                }
                            }
                            else
                            {
                                Comments = "";
                            }
                        }
                    }
                    else
                    {
                        // The new INI was not found
                        Version = ProcessUtils.ProductVersion;
                        Url = null;
                        Comments = "";
                    }
                }

                return (ProcessUtils.ProductVersion != Version);
            }
        }
    }
}
