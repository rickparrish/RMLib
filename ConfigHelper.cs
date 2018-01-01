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
using System.Collections;
using System.IO;
using System.Reflection;
using System.Globalization;
using System.Collections.Specialized;
using System.Collections.Generic;
using System.Reflection.Emit;

namespace RandM.RMLib
{
    /// <summary>
    /// Easily read and write configuration options to an INI file
    /// </summary>
    public abstract class ConfigHelper
    {
        /// <summary>
        /// The section to read from / write to when using the parameterless Load and Save methods
        /// </summary>
        protected string SectionName { get; set; }

        /// <summary>
        /// Create an instance of the ConfigurationHelper with default settings
        /// </summary>
        /// <remarks>
        /// The default settings are to store the INI in the user's roaming application data folder, in the Application.CompanyName subdirectory, using the Application.ProductName as the filename.
        /// </remarks>
        protected ConfigHelper() : this(ConfigSaveLocation.UserApplicationData, ProcessUtils.ProductName + ".ini") { }
        /// <summary>
        /// Create an instance of the ConfigurationHelper with a specific location for the INI to be saved
        /// </summary>
        /// <remarks>
        /// The INI will be stored in the requested location with the Application.ProductName as the file name.  If the Global or User Application Data folder is selected, the INI will be created in the Application.CompanyName subdirectory.
        /// </remarks>
        protected ConfigHelper(ConfigSaveLocation saveLocation) : this(saveLocation, ProcessUtils.ProductName + ".ini") { }
        /// <summary>
        /// Create an instance of the ConfigurationHelper with a specific filename for the INI
        /// </summary>
        /// <remarks>
        /// The INI will be stored in the user's roaming application data folder with the given name.  If the Global or User Application Data folder is selected, the INI will be created in the Application.CompanyName subdirectory.
        /// </remarks>
        protected ConfigHelper(string fileName) : this(ConfigSaveLocation.UserApplicationData, fileName) { }
        /// <summary>
        /// Create an instance of the ConfigurationHelper with a specific location and filename for the INI
        /// </summary>
        /// <remarks>
        /// The INI will be stored in the given location with the given name.  If the Global or User Application Data folder is selected, the INI will be created in the Application.CompanyName subdirectory.
        /// </remarks>
        protected ConfigHelper(ConfigSaveLocation saveLocation, string fileName)
        {
            SectionName = "CONFIGURATION";

            IniPassword = new RMSecureString();
            Loaded = false;
            RMSecureStringPassword = new RMSecureString();

            switch (saveLocation)
            {
                case ConfigSaveLocation.Absolute:
                    FileName = fileName;
                    break;
                case ConfigSaveLocation.GlobalApplicationData:
                    FileName = StringUtils.PathCombine(Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), ProcessUtils.CompanyName, fileName);
                    break;
                case ConfigSaveLocation.Relative:
                    FileName = StringUtils.PathCombine(ProcessUtils.StartupPath, fileName);
                    break;
                case ConfigSaveLocation.UserApplicationData:
                    FileName = StringUtils.PathCombine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), ProcessUtils.CompanyName, fileName);
                    break;
            }
        }

        protected void ChangePassword(RMSecureString newPassword)
        {
            if (IniPassword.Length > 0)
            {
                if (!Loaded) Load();
                IniPassword = newPassword;
                Save();
            }
        }

        /// <summary>
        /// Basic delete method that deletes the currently stored section name
        /// </summary>
        /// <remarks>
        /// If no section name has previously been stored, the default "CONFIGURATION" section will be deleted
        /// </remarks>
        protected void Delete()
        {
            Delete(SectionName);
        }

        /// <summary>
        /// Advanced delete method that allows you to specify the section to delete
        /// </summary>
        /// <param name="sectionName">The section to delete within the INI</param>
        protected void Delete(string sectionName)
        {
            // Store the section name
            SectionName = sectionName;

            // Load the application ini
            using (IniFile Ini = new IniFile(FileName, IniPassword))
            {

                // Check if the desired section exists
                if (Ini.SectionExists(sectionName))
                {
                    // Yep, so delete it
                    Ini.EraseSection(sectionName);
                    Ini.Save();
                }
            }
        }

        /// <summary>
        /// The name of the INI file to be read from / saved to
        /// </summary>
        public string FileName { get; protected set; }

        /// <summary>
        /// The password to encrypt the INI file with (using AES)
        /// </summary>
        public RMSecureString IniPassword { private get; set; }

        /// <summary>
        /// Basic load method that reads from the currently stored section name
        /// </summary>
        /// <remarks>
        /// If no section name has previously been stored, the default "CONFIGURATION" section will be read
        /// </remarks>
        /// <returns>true if the INI section existed; false otherwise</returns>
        protected bool Load()
        {
            return Load(SectionName);
        }

        /// <summary>
        /// Advanced load method that allows you to specify the section to read from
        /// </summary>
        /// <param name="sectionName">The section to read within the INI</param>
        /// <returns>true if the INI section existed; false otherwise</returns>
        protected bool Load(string sectionName)
        {
            // Store the section name
            SectionName = sectionName;

            // Load the application ini
            using (IniFile Ini = new IniFile(FileName, IniPassword))
            {

                // Check if the desired section exists
                if (!Ini.SectionExists(sectionName))
                {
                    // Nope, so abort
                    return false;
                }

                // Loop through each field in the inherited class and read the value from the Ini
                PropertyInfo[] Properties = this.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.DeclaredOnly);
                foreach (PropertyInfo Property in Properties)
                {
                    // Ensure we only look at read+write properties (read only helper properties should not be loaded from/saved to an ini)
                    if ((Property.CanRead) && (Property.CanWrite))
                    {
                        switch (Property.PropertyType.Name)
                        {
                            case "Boolean": Property.SetValue(this, Ini.ReadBoolean(sectionName, Property.Name, (Boolean)Property.GetValue(this, null)), null); break;
                            case "Boolean[]": Property.SetValue(this, Ini.ReadBoolean(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Byte": Property.SetValue(this, Ini.ReadByte(sectionName, Property.Name, (Byte)Property.GetValue(this, null)), null); break;
                            case "Byte[]": Property.SetValue(this, Ini.ReadByte(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Char": Property.SetValue(this, Ini.ReadChar(sectionName, Property.Name, (Char)Property.GetValue(this, null)), null); break;
                            case "Char[]": Property.SetValue(this, Ini.ReadChar(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "DateTime": Property.SetValue(this, Ini.ReadDateTime(sectionName, Property.Name, (DateTime)Property.GetValue(this, null)), null); break;
                            case "DateTime[]": Property.SetValue(this, Ini.ReadDateTime(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Decimal": Property.SetValue(this, Ini.ReadDecimal(sectionName, Property.Name, (Decimal)Property.GetValue(this, null)), null); break;
                            case "Decimal[]": Property.SetValue(this, Ini.ReadDecimal(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Double": Property.SetValue(this, Ini.ReadDouble(sectionName, Property.Name, (Double)Property.GetValue(this, null)), null); break;
                            case "Double[]": Property.SetValue(this, Ini.ReadDouble(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Int16": Property.SetValue(this, Ini.ReadInt16(sectionName, Property.Name, (Int16)Property.GetValue(this, null)), null); break;
                            case "Int16[]": Property.SetValue(this, Ini.ReadInt16(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Int32": Property.SetValue(this, Ini.ReadInt32(sectionName, Property.Name, (Int32)Property.GetValue(this, null)), null); break;
                            case "Int32[]": Property.SetValue(this, Ini.ReadInt32(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Int64": Property.SetValue(this, Ini.ReadInt64(sectionName, Property.Name, (Int64)Property.GetValue(this, null)), null); break;
                            case "Int64[]": Property.SetValue(this, Ini.ReadInt64(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "SByte": Property.SetValue(this, Ini.ReadSByte(sectionName, Property.Name, (SByte)Property.GetValue(this, null)), null); break;
                            case "SByte[]": Property.SetValue(this, Ini.ReadSByte(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "Single": Property.SetValue(this, Ini.ReadSingle(sectionName, Property.Name, (Single)Property.GetValue(this, null)), null); break;
                            case "Single[]": Property.SetValue(this, Ini.ReadSingle(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "String": Property.SetValue(this, Ini.ReadString(sectionName, Property.Name, Property.GetValue(this, null).ToString()), null); break;
                            case "String[]": Property.SetValue(this, Ini.ReadString(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "UInt16": Property.SetValue(this, Ini.ReadUInt16(sectionName, Property.Name, (UInt16)Property.GetValue(this, null)), null); break;
                            case "UInt16[]": Property.SetValue(this, Ini.ReadUInt16(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "UInt32": Property.SetValue(this, Ini.ReadUInt32(sectionName, Property.Name, (UInt32)Property.GetValue(this, null)), null); break;
                            case "UInt32[]": Property.SetValue(this, Ini.ReadUInt32(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "UInt64": Property.SetValue(this, Ini.ReadUInt64(sectionName, Property.Name, (UInt64)Property.GetValue(this, null)), null); break;
                            case "UInt64[]": Property.SetValue(this, Ini.ReadUInt64(sectionName, Property.Name, (IList)Property.GetValue(this, null)), null); break;
                            case "RMSecureString":
                                string Enc = Ini.ReadString(sectionName, Property.Name, "");
                                if (Enc.Length > 0)
                                {
                                    RMSecureString RMSS = new RMSecureString();
                                    try
                                    {
                                        if (RMSecureStringPassword.Length == 0)
                                        {
                                            // No password means protected string
                                            RMSS.LoadFromProtectedString(Enc, RMSecureStringPassword);
                                        }
                                        else
                                        {
                                            // Password means encrypted string
                                            RMSS.LoadFromEncryptedString(Enc, RMSecureStringPassword);
                                        }
                                    }
                                    catch (Exception)
                                    {
                                        // Loading failed -- could be that the protection happened under a different user account, or the password is incorrect
                                        // TODO Should really save the exception and throw it later I think
                                        RMSS = new RMSecureString();
                                    }
                                    Property.SetValue(this, RMSS, null);
                                }
                                break;
                            case "StringDictionary":
                                StringDictionary SD = new StringDictionary();
                                
                                string[] Keys = Ini.ReadSection(sectionName);
                                foreach (string Key in Keys)
                                {
                                    if (Key.IndexOf(Property.Name + "_", StringComparison.OrdinalIgnoreCase) == 0)
                                    {
                                        string KeyWithoutPrefix = Key.Substring(Property.Name.Length + 1);
                                        SD.Add(KeyWithoutPrefix, Ini.ReadString(sectionName, Key, ""));
                                    }
                                }

                                Property.SetValue(this, SD, null);
                                //string Section = Property.Name.ToUpper();
                                //string[] Keys = Ini.ReadSection(Section);

                                //StringDictionary SD = new StringDictionary();
                                //foreach (string Key in Keys)
                                //{
                                //    SD.Add(Key, Ini.ReadString(Section, Key, ""));
                                //}
                                //Property.SetValue(this, SD, null);
                                break;
                            default:
                                // Check for enum, which we can try to parse
                                if (Property.PropertyType.BaseType.Name == "Array")
                                {
                                    List<int> EnumValues = new List<int>();
                                    string[] StringValues = Ini.ReadString(sectionName, Property.Name, (IList)Property.GetValue(this, null));
                                    foreach (string StringValue in StringValues)
                                    {
                                        EnumValues.Add((int)Enum.Parse(Property.PropertyType.GetElementType(), StringValue));
                                    }
                                    Property.SetValue(this, EnumValues.ToArray(), null);
                                }
                                else if (Property.PropertyType.BaseType.Name == "Enum")
                                {
                                    Property.SetValue(this, Enum.Parse(Property.PropertyType, Ini.ReadString(sectionName, Property.Name, Property.GetValue(this, null).ToString())), null);
                                }
                                break;
                        }
                    }
                }
            }

            Loaded = true;
            return true;
        }

        /// <summary>
        /// Indicates whether the previous call to Load() succeeded or not
        /// </summary>
        public bool Loaded { get; private set; }

        protected void MoveTo(ConfigSaveLocation saveLocation, string fileName)
        {
            string NewFileName = "";

            switch (saveLocation)
            {
                case ConfigSaveLocation.Absolute:
                    NewFileName = fileName;
                    break;
                case ConfigSaveLocation.GlobalApplicationData:
                    NewFileName = StringUtils.PathCombine(Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), ProcessUtils.CompanyName, fileName);
                    break;
                case ConfigSaveLocation.Relative:
                    NewFileName = StringUtils.PathCombine(ProcessUtils.StartupPath, fileName);
                    break;
                case ConfigSaveLocation.UserApplicationData:
                    NewFileName = StringUtils.PathCombine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), ProcessUtils.CompanyName, fileName);
                    break;
            }

            // If there's a source file, then move it to the new destination
            if (File.Exists(FileName))
            {
                Directory.CreateDirectory(Path.GetDirectoryName(NewFileName));
                FileUtils.FileMove(FileName, NewFileName);
            }

            FileName = NewFileName;
        }

        protected void RemovePassword()
        {
            if (IniPassword.Length > 0)
            {
                if (!Loaded) Load();
                IniPassword.Clear();
                Save();
            }
        }

        /// <summary>
        /// The password to use when storing an RMSecureString in the INI file
        /// </summary>
        /// <remarks>
        /// If a password is supplied, any RMSecureString will be stored in an encrypted fashion (AES), while if the password is left blank, any RMSecureString will be stored in a protected fashion (DPAPI)
        /// </remarks>
        public RMSecureString RMSecureStringPassword { get; set; }

        /// <summary>
        /// Basic save method that saves to the currently stored section name
        /// </summary>
        /// <remarks>
        /// If no section name has previously been stored, the default "CONFIGURATION" section will be saved
        /// </remarks>
        protected void Save()
        {
            Save(SectionName);
        }

        /// <summary>
        /// Advanced save method that allows you to specify the section to save to
        /// </summary>
        /// <param name="sectionName">The section to save within the INI</param>
        protected void Save(string sectionName)
        {
            // Load the Ini
            IniFile Ini = new IniFile(FileName, IniPassword);

            // Loop through each field in the inherited class and write the value to the Ini
            PropertyInfo[] Properties = this.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.DeclaredOnly);
            foreach (PropertyInfo Property in Properties)
            {
                // Ensure we only look at read+write properties (read only helper properties should not be loaded from/saved to an ini)
                if ((Property.CanRead) && (Property.CanWrite))
                {
                    switch (Property.PropertyType.Name)
                    {
                        case "Boolean":
                        case "Byte":
                        case "Char":
                        case "DateTime":
                        case "Decimal":
                        case "Double":
                        case "Int16":
                        case "Int32":
                        case "Int64":
                        case "SByte":
                        case "Single":
                        case "String":
                        case "UInt16":
                        case "UInt32":
                        case "UInt64":
                            // All the built-in types can be saved via Ini.WriteString()
                            Ini.WriteString(sectionName, Property.Name, Property.GetValue(this, null).ToString());
                            break;
                        case "Boolean[]":
                        case "Byte[]":
                        case "Char[]":
                        case "DateTime[]":
                        case "Decimal[]":
                        case "Double[]":
                        case "Int16[]":
                        case "Int32[]":
                        case "Int64[]":
                        case "SByte[]":
                        case "Single[]":
                        case "String[]":
                        case "UInt16[]":
                        case "UInt32[]":
                        case "UInt64[]":
                            Ini.WriteString(sectionName, Property.Name, (IList)Property.GetValue(this, null));
                            break;
                        case "RMSecureString":
                            // RMSecureString should be saved via Ini.WriteString() either protected or encrypted
                            if (RMSecureStringPassword.Length == 0)
                            {
                                // No password means protected
                                Ini.WriteString(sectionName, Property.Name, ((RMSecureString)Property.GetValue(this, null)).GetProtectedString(RMSecureStringPassword));
                            }
                            else
                            {
                                // Password means encrypted
                                Ini.WriteString(sectionName, Property.Name, ((RMSecureString)Property.GetValue(this, null)).GetEncryptedString(RMSecureStringPassword));
                            }
                            break;
                        case "StringDictionary":
                            StringDictionary SD = (StringDictionary)Property.GetValue(this, null);
                            foreach (DictionaryEntry DE in SD)
                            {
                                Ini.WriteString(sectionName, Property.Name + "_" + DE.Key.ToString(), DE.Value.ToString());
                            }
                            break;
                        default:
                            // Check for enum, which we can save the string representation of
                            if (Property.PropertyType.BaseType.Name == "Array")
                            {
                                Ini.WriteString(sectionName, Property.Name, (IList)Property.GetValue(this, null));
                            }
                            else if (Property.PropertyType.BaseType.Name == "Enum")
                            {
                                Ini.WriteString(sectionName, Property.Name, Property.GetValue(this, null).ToString());
                            }
                            break;
                    }
                }
            }

            // Update the INI files
            Ini.Save();
        }
    }

    /// <summary>
    /// The known locations that ConfigurationHelper can save the INI file to
    /// </summary>
    public enum ConfigSaveLocation
    {
        /// <summary>
        /// The filename parameter in the constructor is an absolute path indicating where to save the INI
        /// </summary>
        Absolute,

        /// <summary>
        /// The filename parameter in the constructor is relative to the global roaming application data folder (any user can access this location)
        /// </summary>
        GlobalApplicationData,

        /// <summary>
        /// The filename parameter in the constructor is relative to the application's exe
        /// </summary>
        /// <remarks>
        /// This location is not recommended, as a program installed to Program Files may not have write access to its own folder
        /// </remarks>
        Relative,

        /// <summary>
        /// The filename parameter in the constructor is relative to the user's roaming application data folder (only the current user can access this location)
        /// </summary>
        UserApplicationData
    }
}
