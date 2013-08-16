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
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace RandM.RMLib
{
    static public class FileUtils
    {
        public delegate void ProgressCallback(long position, long fileSize, out bool abort);

        // TODO Implement as event
        static public ProgressCallback OnProgress = null;

        static private void DoProgress(long position, long fileSize, out bool abort)
        {
            abort = false;
            if (OnProgress != null) OnProgress(position, fileSize, out abort);
        }

        /// <summary>
        /// DirectoryDelete is an exception ignoring wrapper for System.IO.Directory.Delete()
        /// </summary>
        /// <param name="directoryName">The name of the directory to delete</param>
        static public void DirectoryDelete(string directoryName)
        {
            try
            {
                // Try to remove the directory
                Directory.Delete(directoryName);
            }
            catch (Exception)
            {
                // This function ignores errors by design
            }
        }

        static public void FileAppendAllText(string fileName, string text)
        {
            FileAppendAllText(fileName, text, Encoding.Default);
        }

        static public void FileAppendAllText(string fileName, string text, Encoding encoding)
        {
            IOException LastException = null;

            for (int i = 0; i < 5; i++)
            {
                try
                {
                    File.AppendAllText(fileName, text, encoding);
                    return;
                }
                catch (IOException ioex)
                {
                    LastException = ioex;
                    Thread.Sleep(1000);
                }
            }

            // If we get here, re-throw the last exception
            throw LastException;
        }

        /// <summary>
        /// FileDelete is an exception ignoring wrapper for System.IO.File.Delete()
        /// </summary>
        /// <param name="fileName">The name of the file to delete</param>
        static public void FileDelete(string fileName)
        {
            try
            {
                // Try to remove the file
                File.Delete(fileName);
            }
            catch (Exception)
            {
                // This function ignores errors by design
            }
        }

        static public void FileMove(string sourceFileName, string destinationFileName)
        {
            IOException LastException = null;

            for (int i = 0; i < 5; i++)
            {
                try
                {
                    File.Move(sourceFileName, destinationFileName);
                    return;
                }
                catch (IOException ioex)
                {
                    LastException = ioex;
                    Thread.Sleep(1000);
                }
            }

            // If we get here, re-throw the last exception
            throw LastException;
        }

        static public void FileCopy(string sourceFileName, string destinationFileName)
        {
            IOException LastException = null;

            for (int i = 0; i < 5; i++)
            {
                try
                {
                    File.Copy(sourceFileName, destinationFileName);
                    return;
                }
                catch (IOException ioex)
                {
                    LastException = ioex;
                    Thread.Sleep(1000);
                }
            }

            // If we get here, re-throw the last exception
            throw LastException;
        }

        static public string[] FileReadAllLines(string fileName)
        {
            return FileReadAllLines(fileName, Encoding.Default);
        }

        static public string[] FileReadAllLines(string fileName, Encoding encoding)
        {
            IOException LastException = null;

            for (int i = 0; i < 5; i++)
            {
                try
                {
                    return File.ReadAllLines(fileName, encoding);
                }
                catch (IOException ioex)
                {
                    LastException = ioex;
                    Thread.Sleep(1000);
                }
            }

            // If we get here, re-throw the last exception
            throw LastException;
        }

        static public string FileReadAllText(string fileName)
        {
            return FileReadAllText(fileName, Encoding.Default);
        }

        static public string FileReadAllText(string fileName, Encoding encoding)
        {
            IOException LastException = null;

            for (int i = 0; i < 5; i++)
            {
                try
                {
                    return File.ReadAllText(fileName, encoding);
                }
                catch (IOException ioex)
                {
                    LastException = ioex;
                    Thread.Sleep(1000);
                }
            }

            // If we get here, re-throw the last exception
            throw LastException;
        }

        static public void FileWriteAllLines(string fileName, string[] lines)
        {
            FileWriteAllLines(fileName, lines, Encoding.Default);
        }

        static public void FileWriteAllLines(string fileName, string[] lines, Encoding encoding)
        {
            IOException LastException = null;

            for (int i = 0; i < 5; i++)
            {
                try
                {
                    File.WriteAllLines(fileName, lines, encoding);
                    return;
                }
                catch (IOException ioex)
                {
                    LastException = ioex;
                    Thread.Sleep(1000);
                }
            }

            // If we get here, re-throw the last exception
            throw LastException;
        }

        static public void FileWriteAllText(string fileName, string text)
        {
            FileWriteAllText(fileName, text, Encoding.Default);
        }

        static public void FileWriteAllText(string fileName, string text, Encoding encoding)
        {
            IOException LastException = null;

            for (int i = 0; i < 5; i++)
            {
                try
                {
                    File.WriteAllText(fileName, text, encoding);
                    return;
                }
                catch (IOException ioex)
                {
                    LastException = ioex;
                    Thread.Sleep(1000);
                }
            }

            // If we get here, re-throw the last exception
            throw LastException;
        }

        public static byte[] GetFileHash(string fileName)
        {
            using (FileStream FS = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
            {
                return SHA1.Create().ComputeHash(FS);
            }
        }

        static public long GetFileSize(string fileName)
        {
            try
            {
                return new FileInfo(fileName).Length;
            }
            catch (Exception)
            {
                return 0;
            }
        }

        static public int GetStringCount(string fileName, string searchString, bool continueAfterFirstMatch, bool caseSensitive)
        {
            bool Abort = false;
            string Line = null;
            int LineCount = 0;
            RegexOptions RegOptions = (caseSensitive) ? RegexOptions.None : RegexOptions.IgnoreCase;
            int MatchCount = 0;

            try
            {
                using (StreamReader SR = File.OpenText(fileName))
                {
                    while ((Line = SR.ReadLine()) != null)
                    {
                        if (Regex.Match(Line, searchString, RegOptions).Success)
                        {
                            // We have a string match, so increment the match counter, and return if we don't care to find all matches
                            MatchCount++;
                            if (!continueAfterFirstMatch) return 1;
                        }

                        // Let the main program know our progress every 1000 lines
                        if (LineCount++ % 1000 == 999)
                        {
                            DoProgress(SR.BaseStream.Position, SR.BaseStream.Length, out Abort);
                            if (Abort) return MatchCount;
                        }
                    }
                }
            }
            catch (Exception)
            {
                // Search failed
            }

            return MatchCount;
        }

        static public int GetTextPercent(string fileName, int numberOfBytesToAnalyze)
        {
            int TextBytes = 0;
            int TextPercent = 0;
            byte[] Buffer = new byte[numberOfBytesToAnalyze];

            try
            {
                using (FileStream FS = new FileStream(fileName, FileMode.Open))
                {
                    int BytesRead = FS.Read(Buffer, 0, Buffer.Length);

                    for (int i = 0; i < BytesRead; i++)
                    {
                        if (((Buffer[i] >= 8) && (Buffer[i] <= 13)) || ((Buffer[i] >= 32) && (Buffer[i] <= 126))) TextBytes++;
                    }

                    TextPercent = (int)(TextBytes * 100.0 / BytesRead);
                }
            }
            catch (Exception)
            {
                // Text Percent failed
            }

            return TextPercent;
        }
    }
}
