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
using System.Collections.Generic;
using System.Text;
using System.Security.Cryptography;
using System.IO;

namespace RandM.RMLib
{
    /// <summary>
    /// Utility class that handles encryption
    /// </summary>
    public static class AES
    {
        /// <summary>
        /// Encrypts a string
        /// </summary>
        /// <param name="plaintext">Text to be encrypted</param>
        /// <param name="password">Password to encrypt with</param>
        /// <param name="salt">Salt to encrypt with</param>
        /// <returns>An encrypted string</returns>
        public static string Encrypt(string plaintext, string password, string salt)
        {
            string Result = "";

            PasswordDeriveBytes DerivedPassword = new PasswordDeriveBytes(Encoding.ASCII.GetBytes(password), Encoding.ASCII.GetBytes(salt), "SHA512", 12345);

            using (RijndaelManaged SymmetricKey = new RijndaelManaged())
            {
                SymmetricKey.Mode = CipherMode.CBC;
                using (ICryptoTransform Encryptor = SymmetricKey.CreateEncryptor(DerivedPassword.GetBytes(32), DerivedPassword.GetBytes(16)))
                {
                    using (MemoryStream MemStream = new MemoryStream())
                    {
                        using (CryptoStream CryptoStream = new CryptoStream(MemStream, Encryptor, CryptoStreamMode.Write))
                        {
                            byte[] PlainTextBytes = Encoding.ASCII.GetBytes(plaintext);
                            CryptoStream.Write(PlainTextBytes, 0, PlainTextBytes.Length);
                            CryptoStream.FlushFinalBlock();
                            Result = Convert.ToBase64String(MemStream.ToArray());
                        }
                    }
                }
            }

            return Result;
        }

        /// <summary>
        /// Decrypts a string
        /// </summary>
        /// <param name="cipherText">Text to be decrypted</param>
        /// <param name="password">Password to decrypt with</param>
        /// <param name="salt">Salt to decrypt with</param>
        /// <returns>A decrypted string</returns>
        public static string Decrypt(string cipherText, string password, string salt)
        {
            string Result = "";

            PasswordDeriveBytes DerivedPassword = new PasswordDeriveBytes(Encoding.ASCII.GetBytes(password), Encoding.ASCII.GetBytes(salt), "SHA512", 12345);
            using (RijndaelManaged SymmetricKey = new RijndaelManaged())
            {
                SymmetricKey.Mode = CipherMode.CBC;
                using (ICryptoTransform Decryptor = SymmetricKey.CreateDecryptor(DerivedPassword.GetBytes(32), DerivedPassword.GetBytes(16)))
                {
                    using (MemoryStream MemStream = new MemoryStream(Convert.FromBase64String(cipherText)))
                    {
                        using (CryptoStream CryptoStream = new CryptoStream(MemStream, Decryptor, CryptoStreamMode.Read))
                        {
                            byte[] PlainTextBytes = new byte[cipherText.Length];
                            int ByteCount = CryptoStream.Read(PlainTextBytes, 0, PlainTextBytes.Length);
                            Result = Encoding.ASCII.GetString(PlainTextBytes, 0, ByteCount);
                        }
                    }
                }
            }
            
            return Result;
        }
    }
}
