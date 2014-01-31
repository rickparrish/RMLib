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
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;

namespace RandM.RMLib
{
    /// <summary>
    /// Provides read/write access to .INI configuration files
    /// </summary>
    /// <remarks>
    /// To prevent disk thrashing changes are buffered until the class is disposed unless:
    ///  - Custom constructor passes TRUE for AAutoSave parameter
    ///  - Property AutoSave is changed to TRUE
    ///  - Save() is called manually
    /// </remarks>
    public class IniFile : IDisposable
    {
        private const string INI_FILE_ENCRYPTED_HEADER = "RMLib.IniFile.Encrypted";
        private const string INI_FILE_SALT = "IniFile.cs";

        private bool _Disposed = false;
        private string _FileName = "";
        private List<string> _HeaderComment = new List<string>();
        private bool _Modified = false;
        private Dictionary<string, IniFileSection> _Sections = new Dictionary<string, IniFileSection>(StringComparer.OrdinalIgnoreCase);
        private List<string> _SectionNames = new List<string>();

        /// <summary>
        /// Default constructor to load the given INI into memory
        /// </summary>
        /// <param name="fileName">The INI to load</param>
        public IniFile(string fileName) : this(fileName, new RMSecureString()) { }

        /// <summary>
        /// Custom constructor to load the given INI into memory, and indicate whether the changes should be buffered or immediately written to disk
        /// </summary>
        /// <param name="fileName">The INI to load</param>
        /// <param name="password">The password used to encrypt/decrypt the contents of the INI file</param>
        public IniFile(string fileName, RMSecureString password)
        {
            AutoSave = false;
            Password = new RMSecureString();

            _FileName = fileName;
            Password = password;

            List<string> CurrentComment = new List<string>();
            string CurrentSection = "";
            if (File.Exists(fileName))
            {
                // Read in the INI file
                string FileText = FileUtils.FileReadAllText(fileName, RMEncoding.Ansi);

                // Decrypt the INI file (if necessary)
                if (password.Length > 0)
                {
                    try
                    {
                        FileText = AES.Decrypt(FileText, password.GetPlainText(), INI_FILE_SALT);
                        Password = password;
                    }
                    catch (Exception)
                    {
                        return;
                    }
                }

                // Split the INI file into separate lines
                string[] Lines = FileText.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n');

                // Loop through each line
                foreach (string Line in Lines)
                {
                    // Make sure the line isn't empty
                    if ((!string.IsNullOrEmpty(Line.Trim())))
                    {
                        // Check if this is a comment
                        if (Line.TrimStart().StartsWith(";"))
                        {
                            if (string.IsNullOrEmpty(CurrentSection))
                            {
                                _HeaderComment.Add(Line.TrimStart().Substring(1));
                            }
                            else
                            {
                                CurrentComment.Add(Line.TrimStart().Substring(1));
                            }
                        }
                        else
                        {
                            // Check if this is a new section
                            if ((Line.TrimStart().StartsWith("[")) && (Line.TrimEnd().EndsWith("]")))
                            {
                                // It is, so add the new section
                                CurrentSection = Line.Trim().Substring(1, Line.Trim().Length - 2);
                                _Sections[CurrentSection] = new IniFileSection(CurrentComment);
                                _SectionNames.Add(CurrentSection);
                                CurrentComment = new List<string>();
                            }
                            else
                            {
                                // It isn't so add the key/value to the current section
                                if (!string.IsNullOrEmpty(CurrentSection))
                                {
                                    // Make sure this line is in the KEY=VALUE form
                                    int EqualPos = Line.IndexOf('=');
                                    if (EqualPos >= 1)
                                    {
                                        // Get the key
                                        string Key = Line.Substring(0, EqualPos);

                                        // Get the value
                                        string Value = Line.Substring(EqualPos + 1);

                                        // Add the key/value pair to the dictionary
                                        _Sections[CurrentSection].WriteString(CurrentComment, Key, Value);
                                        CurrentComment = new List<string>();
                                    }
                                }
                            }
                        }
                    }
                }

                // Ensure the supplied password is valid
                if (Password.Length > 0)
                {
                    if ((_HeaderComment.Count == 0) || (_HeaderComment[0] != INI_FILE_ENCRYPTED_HEADER))
                    {
                        // Password did not properly decrypt the ini file
                        _Sections.Clear();
                        _SectionNames.Clear();
                        return;
                    }
                }
            }
        }

        /// <summary>
        /// Handle saving the INI to disk at disposal (if there were modifications)
        /// </summary>
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

        /// <summary>
        /// Handle saving the INI to disk at disposal (if there were modifications)
        /// </summary>
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
                if (_Modified) Save();

                // Note disposing has been done.
                _Disposed = true;
            }
        }

        /// <summary>
        /// Controls whether changes are buffered (=false) or immediately written to disk (=true)
        /// </summary>
        public bool AutoSave { get; set; }

        /// <summary>
        /// Remove the given key (and it's value) from the given section
        /// </summary>
        /// <param name="sectionName">The section containing the key to remove</param>
        /// <param name="keyName">The key to remove</param>
        public void DeleteKey(string sectionName, string keyName)
        {
            if (_Sections.ContainsKey(sectionName))
            {
                _Modified |= _Sections[sectionName].DeleteKey(keyName);
                if (_Modified && AutoSave) Save();
            }
        }

        /// <summary>
        /// Remove the given section (along with all the keys+values) from the INI
        /// </summary>
        /// <param name="sectionName">The section to remove</param>
        public void EraseSection(string sectionName)
        {
            if (_Sections.ContainsKey(sectionName))
            {
                _Sections.Remove(sectionName);

                // Need to do this in a loop since section names are case insensitive, but List<string> is case sensitive
                for (int i = _SectionNames.Count - 1; i >= 0; i--)
                {
                    if (sectionName.ToUpper() == _SectionNames[i].ToUpper())
                    {
                        _SectionNames.RemoveAt(i);
                    }
                }

                _Modified = true;
                if (_Modified && AutoSave) Save();
            }
        }

        /// <summary>
        /// The comment block that was read from / to be written to the top of the INI file
        /// </summary>
        /// <remarks>
        /// If a comment is found at the top of an INI file, it is assumed to be a header comment, and so is not associated with the first section.  This may not be correct behaviour, but the alternative is to associate the first comment with the first section, but then that comment will be removed if the first section is removed, which would be bad if the first comment really was a header comment
        /// </remarks>
        public List<string> HeaderComment
        {
            get { return _HeaderComment; }
            set
            {
                _HeaderComment = value;
                _Modified = true;
                if (AutoSave) Save();
            }
        }

        /// <summary>
        /// Check whether a given section exists in the INI
        /// </summary>
        /// <param name="sectionName">The section to look in</param>
        /// <param name="keyName">The key to look for</param>
        /// <returns>TRUE = section and key exists, FALSE = it does not exist</returns>
        public bool KeyExists(string sectionName, string keyName)
        {
            return _Sections.ContainsKey(sectionName) && _Sections[sectionName].ContainsKey(keyName);
        }

        public RMSecureString Password { get; set; }

        /// <summary>
        /// Returns a string array of all the keys in the given section
        /// </summary>
        /// <param name="sectionName">The section to retrieve keys from</param>
        /// <returns>The keys in the given section</returns>
        public string[] ReadSection(string sectionName)
        {
            if (_Sections.ContainsKey(sectionName))
            {
                string[] Keys = _Sections[sectionName].ReadSection();
                for (int i = 0; i < Keys.Length; i++)
                {
                    Keys[i] = Keys[i];
                }
                return Keys;
            }
            else
            {
                return new string[0];
            }
        }

        /// <summary>
        /// Returns a string array of all the sections in the INI
        /// </summary>
        /// <returns>The sections in the INI</returns>
        public string[] ReadSections()
        {
            return _SectionNames.ToArray();
        }

        /// <summary>
        /// Reads a string from the given key in the given section, using the provided default if the key does not exist
        /// </summary>
        /// <param name="sectionName">The section to look in</param>
        /// <param name="keyName">The key to retrieve the value for</param>
        /// <param name="defaultValue">The value to use if the key cannot be found</param>
        /// <returns></returns>
        public string ReadString(string sectionName, string keyName, string defaultValue)
        {
            if (_Sections.ContainsKey(sectionName))
            {
                return _Sections[sectionName].ReadString(keyName, defaultValue);
            }
            else
            {
                return defaultValue;
            }
        }

        public string[] ReadString(string sectionName, string keyName, IList defaultValue)
        {
            List<string> Result = new List<string>();

            int i = 0;
            while (KeyExists(sectionName, keyName + i.ToString()))
            {
                Result.Add(ReadString(sectionName, keyName + i.ToString(), (i > defaultValue.Count - 1) ? "" : defaultValue[i].ToString()));
                i += 1;
            }

            return Result.ToArray();
        }

        #region Read* (not string) methods
        public Boolean ReadBoolean(string ASection, string AKey, Boolean ADefault)
        {
            Boolean X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Boolean.TryParse(Result, out X)) ? X : ADefault;
        }

        public Boolean[] ReadBoolean(string ASection, string AKey, IList ADefault)
        {
            List<Boolean> Result = new List<Boolean>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Boolean X;
                Result.Add((Boolean.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? false : (Boolean)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Byte ReadByte(string ASection, string AKey, Byte ADefault)
        {
            Byte X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Byte.TryParse(Result, out X)) ? X : ADefault;
        }

        public Byte[] ReadByte(string ASection, string AKey, IList ADefault)
        {
            List<Byte> Result = new List<Byte>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Byte X;
                Result.Add((Byte.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? (Byte)0 : (Byte)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Char ReadChar(string ASection, string AKey, Char ADefault)
        {
            Char X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Char.TryParse(Result, out X)) ? X : ADefault;
        }

        public Char[] ReadChar(string ASection, string AKey, IList ADefault)
        {
            List<Char> Result = new List<Char>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Char X;
                Result.Add((Char.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? '\0' : (Char)ADefault[i]);
            }
            return Result.ToArray();
        }

        public DateTime ReadDateTime(string ASection, string AKey, DateTime ADefault)
        {
            DateTime X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (DateTime.TryParse(Result, out X)) ? X : ADefault;
        }

        public DateTime[] ReadDateTime(string ASection, string AKey, IList ADefault)
        {
            List<DateTime> Result = new List<DateTime>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                DateTime X;
                Result.Add((DateTime.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? DateTime.MinValue : (DateTime)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Decimal ReadDecimal(string ASection, string AKey, Decimal ADefault)
        {
            Decimal X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Decimal.TryParse(Result, out X)) ? X : ADefault;
        }

        public Decimal[] ReadDecimal(string ASection, string AKey, IList ADefault)
        {
            List<Decimal> Result = new List<Decimal>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Decimal X;
                Result.Add((Decimal.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? 0 : (Decimal)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Double ReadDouble(string ASection, string AKey, Double ADefault)
        {
            Double X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Double.TryParse(Result, out X)) ? X : ADefault;
        }

        public Double[] ReadDouble(string ASection, string AKey, IList ADefault)
        {
            List<Double> Result = new List<Double>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Double X;
                Result.Add((Double.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? 0 : (Double)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Int16 ReadInt16(string ASection, string AKey, Int16 ADefault)
        {
            Int16 X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Int16.TryParse(Result, out X)) ? X : ADefault;
        }

        public Int16[] ReadInt16(string ASection, string AKey, IList ADefault)
        {
            List<Int16> Result = new List<Int16>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Int16 X;
                Result.Add((Int16.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? (Int16)0 : (Int16)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Int32 ReadInt32(string ASection, string AKey, Int32 ADefault)
        {
            Int32 X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Int32.TryParse(Result, out X)) ? X : ADefault;
        }

        public Int32[] ReadInt32(string ASection, string AKey, IList ADefault)
        {
            List<Int32> Result = new List<Int32>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Int32 X;
                Result.Add((Int32.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? 0 : (Int32)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Int64 ReadInt64(string ASection, string AKey, Int64 ADefault)
        {
            Int64 X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Int64.TryParse(Result, out X)) ? X : ADefault;
        }

        public Int64[] ReadInt64(string ASection, string AKey, IList ADefault)
        {
            List<Int64> Result = new List<Int64>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Int64 X;
                Result.Add((Int64.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? 0 : (Int64)ADefault[i]);
            }
            return Result.ToArray();
        }

        public SByte ReadSByte(string ASection, string AKey, SByte ADefault)
        {
            SByte X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (SByte.TryParse(Result, out X)) ? X : ADefault;
        }

        public SByte[] ReadSByte(string ASection, string AKey, IList ADefault)
        {
            List<SByte> Result = new List<SByte>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                SByte X;
                Result.Add((SByte.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? (SByte)0 : (SByte)ADefault[i]);
            }
            return Result.ToArray();
        }

        public Single ReadSingle(string ASection, string AKey, Single ADefault)
        {
            Single X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (Single.TryParse(Result, out X)) ? X : ADefault;
        }

        public Single[] ReadSingle(string ASection, string AKey, IList ADefault)
        {
            List<Single> Result = new List<Single>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                Single X;
                Result.Add((Single.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? 0 : (Single)ADefault[i]);
            }
            return Result.ToArray();
        }

        public UInt16 ReadUInt16(string ASection, string AKey, UInt16 ADefault)
        {
            UInt16 X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (UInt16.TryParse(Result, out X)) ? X : ADefault;
        }

        public UInt16[] ReadUInt16(string ASection, string AKey, IList ADefault)
        {
            List<UInt16> Result = new List<UInt16>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                UInt16 X;
                Result.Add((UInt16.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? (UInt16)0 : (UInt16)ADefault[i]);
            }
            return Result.ToArray();
        }

        public UInt32 ReadUInt32(string ASection, string AKey, UInt32 ADefault)
        {
            UInt32 X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (UInt32.TryParse(Result, out X)) ? X : ADefault;
        }

        public UInt32[] ReadUInt32(string ASection, string AKey, IList ADefault)
        {
            List<UInt32> Result = new List<UInt32>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                UInt32 X;
                Result.Add((UInt32.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? 0 : (UInt32)ADefault[i]);
            }
            return Result.ToArray();
        }

        public UInt64 ReadUInt64(string ASection, string AKey, UInt64 ADefault)
        {
            UInt64 X;
            string Result = ReadString(ASection, AKey, ADefault.ToString());
            return (UInt64.TryParse(Result, out X)) ? X : ADefault;
        }

        public UInt64[] ReadUInt64(string ASection, string AKey, IList ADefault)
        {
            List<UInt64> Result = new List<UInt64>();
            string[] Items = ReadString(ASection, AKey, ADefault);
            for (int i = 0; i < Items.Length; i++)
            {
                UInt64 X;
                Result.Add((UInt64.TryParse(Items[i], out X)) ? X : (i > ADefault.Count - 1) ? 0 : (UInt64)ADefault[i]);
            }
            return Result.ToArray();
        }
        #endregion

        /// <summary>
        /// Check whether a given section exists in the INI
        /// </summary>
        /// <param name="sectionName">The section to look for</param>
        /// <returns>TRUE = section exists, FALSE = it does not exist</returns>
        public bool SectionExists(string sectionName)
        {
            return _Sections.ContainsKey(sectionName);
        }

        /// <summary>
        /// Default method to manually save the INI to disk, using the filename that was used when opening the ini
        /// </summary>
        public void Save()
        {
            Save(_FileName);
        }

        /// <summary>
        /// Custom method to manually save the INI to disk, using the provided filename
        /// </summary>
        /// <remarks>
        /// The old INI is left untouched, and does not get deleted
        /// </remarks>
        /// <param name="AFileName">The new filename to save the INI as</param>
        public void Save(string AFileName)
        {
            _Modified = false;

            if (Password.Length > 0)
            {
                if ((_HeaderComment.Count == 0) || (_HeaderComment[0] != INI_FILE_ENCRYPTED_HEADER))
                {
                    _HeaderComment.Insert(0, INI_FILE_ENCRYPTED_HEADER);
                }
            }

            List<string> Lines = new List<string>();
            if (_HeaderComment.Count > 0)
            {
                for (int i = 0; i < _HeaderComment.Count; i++)
                {
                    Lines.Add(";" + _HeaderComment[i]);
                }
                Lines.Add(""); // A little white space
            }
            foreach (string SectionName in _SectionNames)
            {
                _Sections[SectionName].Save(SectionName, ref Lines);
                Lines.Add(""); // A bit of white space
            }

            string FileText = string.Join("\r\n", Lines.ToArray());
            if (Password.Length > 0) FileText = AES.Encrypt(FileText, Password.GetPlainText(), INI_FILE_SALT);

            Directory.CreateDirectory(Path.GetDirectoryName(AFileName));
            FileUtils.FileWriteAllText(AFileName, FileText);
        }

        /// <summary>
        /// Adds the given value to the given key in the given section
        /// </summary>
        /// <param name="sectionName">The section to add the value to</param>
        /// <param name="keyName">The key to add the value to</param>
        /// <param name="defaultValue">The value to be added</param>
        public void WriteString(string sectionName, string keyName, string defaultValue)
        {
            // Check if the dictionary contains the given section
            if (!_Sections.ContainsKey(sectionName))
            {
                // It doesn't, so we need to create it
                _Sections[sectionName] = new IniFileSection();
                _SectionNames.Add(sectionName);
            }

            // Update the modified flag
            _Modified |= _Sections[sectionName].WriteString(keyName, defaultValue);

            // If we want to write all updates immediately, we should do so here
            if (_Modified && AutoSave) Save();
        }

        /// <summary>
        /// Adds the given value array to the given key in the given section
        /// </summary>
        /// <param name="sectionName">The section to add the value to</param>
        /// <param name="keyName">The key to add the value to</param>
        /// <param name="defaultValue">The value array to be added</param>
        public void WriteString(string sectionName, string keyName, IList defaultValue)
        {
            // First, remove any entry with the keyname + number
            string[] Keys = ReadSection(sectionName);
            foreach (string Key in Keys)
            {
                if (Key.IndexOf(keyName, StringComparison.OrdinalIgnoreCase) == 0)
                {
                    string KeyWithoutPrefix = Key.Substring(keyName.Length);
                    int temp;
                    if (int.TryParse(KeyWithoutPrefix, out temp))
                    {
                        DeleteKey(sectionName, Key);
                    }
                }
            }

            // Save the old autosave value then disable it, since if we have 100 keys to write we don't want to rewrite the file 100 times!
            bool OldAutoSave = AutoSave;
            AutoSave = false;

            // Then save the new keys
            for (int i = 0; i < defaultValue.Count; i++)
            {
                // All the built-in types can be saved via Ini.WriteString()
                WriteString(sectionName, keyName + i.ToString(), defaultValue[i].ToString());
            }

            // And restore the autosave setting
            AutoSave = OldAutoSave;

            // If we want to write all updates immediately, we should do so here
            if (_Modified && AutoSave) Save();

        }
    }

    class IniFileKey
    {
        private List<string> _Comment = new List<string>();
        private bool _Quoted = false;
        private string _Value = "";

        public string ReadString()
        {
            return _Value;
        }

        public void Save(string keyName, ref List<string> lines)
        {
            if (_Comment.Count > 0)
            {
                for (int i = 0; i < _Comment.Count; i++)
                {
                    lines.Add(";" + _Comment[i]);
                }
            }

            if (_Quoted)
            {
                lines.Add(keyName + "=\"" + _Value + "\"");
            }
            else
            {
                lines.Add(keyName + "=" + _Value);
            }
        }

        public bool WriteString(string value)
        {
            // Persist the existing comment, if one exists
            return WriteString(_Comment, value);
        }

        public bool WriteString(List<string> comment, string value)
        {
            _Quoted = false;
            if (value.StartsWith("\"") && value.EndsWith("\""))
            {
                value = value.Substring(1, value.Length - 2);
                _Quoted = true;
            }

            bool Changed = (_Value != value);

            _Comment = comment;
            _Value = value;

            return Changed;
        }
    }

    class IniFileSection
    {
        private List<string> _Comment = new List<string>();
        private Dictionary<string, IniFileKey> _Keys;

        public IniFileSection() : this(new List<string>()) { }

        public IniFileSection(List<string> comment)
        {
            _Comment = comment;
            _Keys = new Dictionary<string, IniFileKey>(StringComparer.OrdinalIgnoreCase);
        }

        public bool ContainsKey(string keyName)
        {
            return _Keys.ContainsKey(keyName);
        }

        public bool DeleteKey(string keyName)
        {
            if (_Keys.ContainsKey(keyName))
            {
                _Keys.Remove(keyName);
                return true;
            }
            else
            {
                return false;
            }
        }

        public string[] ReadSection()
        {
            string[] Lines = new string[_Keys.Count];
            _Keys.Keys.CopyTo(Lines, 0);
            return Lines;
        }

        public string ReadString(string keyName, string defaultValue)
        {
            if (_Keys.ContainsKey(keyName))
            {
                return _Keys[keyName].ReadString();
            }
            else
            {
                return defaultValue;
            }
        }

        public void Save(string sectionName, ref List<string> lines)
        {
            if (_Comment.Count > 0)
            {
                for (int i = 0; i < _Comment.Count; i++)
                {
                    lines.Add(";" + _Comment[i]);
                }
            }

            lines.Add("[" + sectionName + "]");
            foreach (KeyValuePair<string, IniFileKey> KV in _Keys)
            {
                KV.Value.Save(KV.Key, ref lines);
            }
        }

        public bool WriteString(string keyName, string value)
        {
            if (!_Keys.ContainsKey(keyName))
            {
                _Keys[keyName] = new IniFileKey();
            }

            return _Keys[keyName].WriteString(value);
        }

        public bool WriteString(List<string> comment, string keyName, string value)
        {
            if (!_Keys.ContainsKey(keyName))
            {
                _Keys[keyName] = new IniFileKey();
            }

            return _Keys[keyName].WriteString(comment, value);
        }
    }
}