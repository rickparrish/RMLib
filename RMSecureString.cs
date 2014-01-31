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
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Security;
using System.Security.Cryptography;
using System.Text;

namespace RandM.RMLib
{
    public class RMSecureString : IDisposable
    {
        private bool _Disposed = false;
        private object _SecureString = null;
        private bool _SecureStringSupported = true;

        public RMSecureString()
        {
            try
            {
                _SecureString = new SecureString();
            }
            catch (NotSupportedException)
            {
                _SecureStringSupported = false;
                _SecureString = new StringBuilder();
            }
        }

        ~RMSecureString()
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

                // Note disposing has been done.
                _Disposed = true;
            }
        }

        /// <summary>
        /// Creates RMSecureString object from plain text provided
        /// Called for example when using:
        /// RMSecureString rmss = "test";
        /// </summary>
        /// <param name="rhs">Plain text to initialize the RMSecureString to hold</param>
        /// <returns>new RMSecureString</returns>
        public static implicit operator RMSecureString(string rhs)
        {
            if (rhs == null) return null;

            RMSecureString Result = new RMSecureString();
            for (int i = 0; i < rhs.Length; i++)
            {
                Result.AppendChar(rhs[i]);
            }
            return Result;
        }

        /// <summary>
        /// Creates RMSecureString object from RMSecureString provided
        /// Called for example when using:
        /// SecureString ss = new SecureString();
        /// ss.AppendChar('x');
        /// RMSecureString rmss = ss;
        /// </summary>
        /// <param name="secureString">RMSecureString to initialize the RMSecureString to hold</param>
        /// <returns>new RMSecureString</returns>
        public static implicit operator RMSecureString(SecureString rhs)
        {
            if (rhs == null) return null;

            RMSecureString Result = new RMSecureString();
            Result._SecureString = rhs.Copy();
            return Result;
        }

        public static bool operator ==(RMSecureString lhs, RMSecureString rhs)
        {
            return lhs.Equals(rhs);
        }

        public static bool operator !=(RMSecureString lhs, RMSecureString rhs)
        {
            return !lhs.Equals(rhs);
        }

        public void AppendChar(char c)
        {
            if (_SecureStringSupported)
            {
                ((SecureString)_SecureString).AppendChar(c);
            }
            else
            {
                ((StringBuilder)_SecureString).Append(c);
            }
        }

        public void Clear()
        {
            if (_SecureStringSupported)
            {
                ((SecureString)_SecureString).Clear();
            }
            else
            {
                ((StringBuilder)_SecureString).Length = 0;
            }
        }

        // Modified from http://social.msdn.microsoft.com/Forums/en-US/clr/thread/555a5cb6-790d-415d-b079-00d62b3a9632/
        public override bool Equals(object rhs)
        {
            // Ensure we're comparing to another RMSecureString
            if (!(rhs is RMSecureString)) return false;

            // Easy check -- see if lengths differ
            if (this.Length != ((RMSecureString)rhs).Length)
            {
                return false;
            }

            IntPtr bstr1 = IntPtr.Zero;
            IntPtr bstr2 = IntPtr.Zero;

            RuntimeHelpers.PrepareConstrainedRegions();

            try
            {
                if (_SecureStringSupported)
                {
                    bstr1 = Marshal.SecureStringToBSTR((SecureString)_SecureString);
                    bstr2 = Marshal.SecureStringToBSTR((SecureString)((RMSecureString)rhs)._SecureString);
                }
                else
                {
                    bstr1 = Marshal.StringToBSTR(((StringBuilder)_SecureString).ToString());
                    bstr2 = Marshal.StringToBSTR(((StringBuilder)((RMSecureString)rhs)._SecureString).ToString());
                }

                unsafe
                {
                    for (Char* ptr1 = (Char*)bstr1.ToPointer(), ptr2 = (Char*)bstr2.ToPointer(); *ptr1 != 0 && *ptr2 != 0; ++ptr1, ++ptr2)
                    {
                        if (*ptr1 != *ptr2)
                        {
                            return false;
                        }
                    }
                }

                return true;
            }
            finally
            {
                if (bstr1 != IntPtr.Zero)
                {
                    Marshal.ZeroFreeBSTR(bstr1);
                }

                if (bstr2 != IntPtr.Zero)
                {
                    Marshal.ZeroFreeBSTR(bstr2);
                }
            }
        }

        /// <summary>
        /// Encrypt the current SecureString with the AES encryption algorithm, returning the computed value as a base64 string
        /// </summary>
        /// <returns>A base64 encoded string of the encrypted version of the current SecureString</returns>
        /// <remarks>Based on the code from Sly Gryphon's comment at http://weblogs.asp.net/pglavich/archive/2006/10/29/Secure-TextBox-Updated.aspx </remarks>
        public string GetEncryptedString(RMSecureString password)
        {
            string Result = "";

            IntPtr PlainTextPtr = IntPtr.Zero;
            IntPtr PasswordPtr = IntPtr.Zero;

            try
            {
                // Get the secure plaintext string into a memory buffer
                if (_SecureStringSupported)
                {
                    PlainTextPtr = Marshal.SecureStringToGlobalAllocAnsi((SecureString)_SecureString);
                }
                else
                {
                    PlainTextPtr = Marshal.StringToHGlobalAnsi(((StringBuilder)_SecureString).ToString());
                }
                int PlainTextSize = this.Length * sizeof(byte);

                // Get the secure password string into a memory buffer
                PasswordPtr = Marshal.SecureStringToGlobalAllocAnsi(password.GetSecureText());
                int PasswordSize = password.Length * sizeof(byte);

                // Pin the array, copy data in, use it, and then make sure it is clear before unpinning.
                unsafe
                {
                    byte[] PlainTextBytes = new byte[PlainTextSize];
                    fixed (byte* ptr1 = PlainTextBytes)
                    {
                        byte[] PasswordBytes = new byte[PasswordSize];
                        fixed (byte* ptr2 = PasswordBytes)
                        {
                            try
                            {
                                Marshal.Copy(PlainTextPtr, PlainTextBytes, 0, PlainTextSize);
                                Marshal.Copy(PasswordPtr, PasswordBytes, 0, PasswordSize);

                                PasswordDeriveBytes DerivedPassword = new PasswordDeriveBytes(PasswordBytes, Encoding.ASCII.GetBytes("RMSecureString"), "SHA512", 12345);
                                using (RijndaelManaged SymmetricKey = new RijndaelManaged())
                                {
                                    SymmetricKey.Mode = CipherMode.CBC;
                                    using (ICryptoTransform Encryptor = SymmetricKey.CreateEncryptor(DerivedPassword.GetBytes(32), DerivedPassword.GetBytes(16)))
                                    {
                                        using (MemoryStream MemStream = new MemoryStream())
                                        {
                                            using (CryptoStream CryptoStream = new CryptoStream(MemStream, Encryptor, CryptoStreamMode.Write))
                                            {
                                                CryptoStream.Write(PlainTextBytes, 0, PlainTextBytes.Length);
                                                CryptoStream.FlushFinalBlock();

                                                Result = "e" + Convert.ToBase64String(MemStream.ToArray());
                                            }
                                        }
                                    }
                                }
                            }
                            finally
                            {
                                // Ensure managed array is cleared
                                Array.Clear(PlainTextBytes, 0, PlainTextSize);

                                // Ensure managed array is cleared
                                Array.Clear(PasswordBytes, 0, PasswordSize);
                            }
                        }
                    }
                }
            }
            finally
            {
                // Ensure unmanaged memory is released.
                if (PlainTextPtr != IntPtr.Zero)
                {
                    Marshal.ZeroFreeGlobalAllocAnsi(PlainTextPtr);
                }

                // Ensure unmanaged memory is released.
                if (PasswordPtr != IntPtr.Zero)
                {
                    Marshal.ZeroFreeGlobalAllocAnsi(PasswordPtr);
                }
            }

            return Result;
        }

        public override int GetHashCode()
        {
            return _SecureString.GetHashCode();
        }

        /// <summary>
        /// Hash the current SecureString with the SHA512 hash algorithm and saltBytes, returning the computed value as a base64 string
        /// </summary>
        /// <param name="saltBytes">The salt to apply to the hash</param>
        /// <returns>A base64 encoded hash of the current SecureString</returns>
        /// <remarks>Based on the code from Sly Gryphon's comment at http://weblogs.asp.net/pglavich/archive/2006/10/29/Secure-TextBox-Updated.aspx </remarks>
        public string GetHashedString(byte[] saltBytes)
        {
            string Result = "";
            IntPtr secureStringPtr = IntPtr.Zero;

            try
            {
                // Get the secure string into a memory buffer
                if (_SecureStringSupported)
                {
                    secureStringPtr = Marshal.SecureStringToGlobalAllocAnsi((SecureString)_SecureString);
                }
                else
                {
                    secureStringPtr = Marshal.StringToHGlobalAnsi(((StringBuilder)_SecureString).ToString());
                }
                int stringSize = (saltBytes.Length + this.Length) * sizeof(byte);

                // Pin the array, copy data in, use it, and then make sure it is clear before unpinning.
                unsafe
                {
                    byte[] fixedByteArray = new byte[stringSize];
                    fixed (byte* ptr = fixedByteArray)
                    {
                        try
                        {
                            // Add the salt bytes
                            for (int i = 0; i < saltBytes.Length; i++) fixedByteArray[i] = saltBytes[i];

                            Marshal.Copy(secureStringPtr, fixedByteArray, saltBytes.Length, this.Length * sizeof(byte));

                            // Compute the hash
                            using (SHA512Managed SHA = new SHA512Managed())
                            {
                                Result = Convert.ToBase64String(SHA.ComputeHash(fixedByteArray));
                            }
                        }
                        finally
                        {
                            // Ensure managed array is cleared
                            Array.Clear(fixedByteArray, 0, stringSize);
                        }
                    }
                }
            }
            finally
            {
                // Ensure unmanaged memory is released.
                if (secureStringPtr != IntPtr.Zero)
                {
                    Marshal.ZeroFreeGlobalAllocAnsi(secureStringPtr);
                }
            }

            return Result;
        }

        public string GetPlainText()
        {
            if (_SecureStringSupported)
            {
                IntPtr unmanagedString = IntPtr.Zero;
                try
                {
                    unmanagedString = Marshal.SecureStringToBSTR((SecureString)_SecureString);
                    return Marshal.PtrToStringAuto(unmanagedString);
                }
                finally
                {
                    if (unmanagedString != IntPtr.Zero)
                    {
                        Marshal.ZeroFreeBSTR(unmanagedString);
                    }
                }
            }
            else
            {
                return ((StringBuilder)_SecureString).ToString();
            }
        }

        /// <summary>
        /// Encrypt the current SecureString with the ProtectData API, returning the computed value as a base64 string
        /// </summary>
        /// <returns>A base64 encoded string of the protected version of the current SecureString</returns>
        /// <remarks>Based on the code from Sly Gryphon's comment at http://weblogs.asp.net/pglavich/archive/2006/10/29/Secure-TextBox-Updated.aspx </remarks>
        public string GetProtectedString(RMSecureString password)
        {
            string Result = "";

            IntPtr PlainTextPtr = IntPtr.Zero;
            IntPtr PasswordPtr = IntPtr.Zero;

            try
            {
                // Get the secure plaintext string into a memory buffer
                if (_SecureStringSupported)
                {
                    PlainTextPtr = Marshal.SecureStringToGlobalAllocAnsi((SecureString)_SecureString);
                }
                else
                {
                    PlainTextPtr = Marshal.StringToHGlobalAnsi(((StringBuilder)_SecureString).ToString());
                }
                int PlainTextSize = this.Length * sizeof(byte);

                // Get the secure password string into a memory buffer
                PasswordPtr = Marshal.SecureStringToGlobalAllocAnsi(password.GetSecureText());
                int PasswordSize = password.Length * sizeof(byte);

                // Pin the array, copy data in, use it, and then make sure it is clear before unpinning.
                unsafe
                {
                    byte[] PlainTextBytes = new byte[PlainTextSize];
                    fixed (byte* ptr1 = PlainTextBytes)
                    {
                        byte[] PasswordBytes = new byte[PasswordSize];
                        fixed (byte* ptr2 = PasswordBytes)
                        {
                            try
                            {
                                Marshal.Copy(PlainTextPtr, PlainTextBytes, 0, PlainTextSize);
                                Marshal.Copy(PasswordPtr, PasswordBytes, 0, PasswordSize);

                                if (PasswordSize == 0)
                                {
                                    Result = "p" + Convert.ToBase64String(ProtectedData.Protect(PlainTextBytes, null, DataProtectionScope.CurrentUser));
                                }
                                else
                                {
                                    Result = "p" + Convert.ToBase64String(ProtectedData.Protect(PlainTextBytes, PasswordBytes, DataProtectionScope.CurrentUser));
                                }
                            }
                            finally
                            {
                                // Ensure managed array is cleared
                                Array.Clear(PlainTextBytes, 0, PlainTextSize);

                                // Ensure managed array is cleared
                                Array.Clear(PasswordBytes, 0, PasswordSize);
                            }
                        }
                    }
                }
            }
            finally
            {
                // Ensure unmanaged memory is released.
                if (PlainTextPtr != IntPtr.Zero)
                {
                    Marshal.ZeroFreeGlobalAllocAnsi(PlainTextPtr);
                }

                // Ensure unmanaged memory is released.
                if (PasswordPtr != IntPtr.Zero)
                {
                    Marshal.ZeroFreeGlobalAllocAnsi(PasswordPtr);
                }
            }

            return Result;
        }

        public SecureString GetSecureText()
        {
            if (_SecureStringSupported)
            {
                return (SecureString)_SecureString;
            }
            else
            {
                return null;
            }
        }

        public void InsertAt(int AIndex, char c)
        {
            if (_SecureStringSupported)
            {
                ((SecureString)_SecureString).InsertAt(AIndex, c);
            }
            else
            {
                ((StringBuilder)_SecureString).Insert(AIndex, c);
            }
        }

        public int Length
        {
            get
            {
                if (_SecureStringSupported)
                {
                    return ((SecureString)_SecureString).Length;
                }
                else
                {
                    return ((StringBuilder)_SecureString).Length;
                }
            }
        }

        public void LoadFromEncryptedString(string encryptedString, RMSecureString password)
        {
            Clear();

            if (encryptedString.StartsWith("p"))
            {
                LoadFromProtectedString(encryptedString, password);
            }
            else if (encryptedString.StartsWith("e"))
            {
                // Trim leading 'e', which is just an indicator to say that this is an encrypted and not a protected string
                encryptedString = encryptedString.Substring(1);

                IntPtr PasswordPtr = IntPtr.Zero;

                try
                {
                    // Get the secure password string into a memory buffer
                    PasswordPtr = Marshal.SecureStringToGlobalAllocAnsi(password.GetSecureText());
                    int PasswordSize = password.Length * sizeof(byte);

                    // Pin the array, copy data in, use it, and then make sure it is clear before unpinning.
                    unsafe
                    {
                        byte[] Decrypted = null;
                        byte[] PasswordBytes = new byte[PasswordSize];
                        fixed (byte* ptr = PasswordBytes)
                        {
                            try
                            {
                                Marshal.Copy(PasswordPtr, PasswordBytes, 0, PasswordSize);

                                PasswordDeriveBytes DerivedPassword = new PasswordDeriveBytes(PasswordBytes, Encoding.ASCII.GetBytes("RMSecureString"), "SHA512", 12345);
                                using (RijndaelManaged SymmetricKey = new RijndaelManaged())
                                {
                                    SymmetricKey.Mode = CipherMode.CBC;
                                    using (ICryptoTransform Decryptor = SymmetricKey.CreateDecryptor(DerivedPassword.GetBytes(32), DerivedPassword.GetBytes(16)))
                                    {
                                        using (MemoryStream MemStream = new MemoryStream(Convert.FromBase64String(encryptedString)))
                                        {
                                            using (CryptoStream CryptoStream = new CryptoStream(MemStream, Decryptor, CryptoStreamMode.Read))
                                            {
                                                byte[] DecryptedByte = new byte[1];

                                                int ByteCount = CryptoStream.Read(DecryptedByte, 0, 1);
                                                while (ByteCount > 0)
                                                {
                                                    if (_SecureStringSupported)
                                                    {
                                                        ((SecureString)_SecureString).AppendChar((char)DecryptedByte[0]);
                                                    }
                                                    else
                                                    {
                                                        ((StringBuilder)_SecureString).Append((char)DecryptedByte[0]);
                                                    }
                                                    ByteCount = CryptoStream.Read(DecryptedByte, 0, 1);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            finally
                            {
                                // Ensure managed array is cleared
                                if (Decrypted != null) Array.Clear(Decrypted, 0, Decrypted.Length);
                                Array.Clear(PasswordBytes, 0, PasswordSize);
                            }
                        }
                    }
                }
                finally
                {
                    // Ensure unmanaged memory is released.
                    if (PasswordPtr != IntPtr.Zero)
                    {
                        Marshal.ZeroFreeGlobalAllocAnsi(PasswordPtr);
                    }
                }
            }
        }

        public void LoadFromProtectedString(string protectedString, RMSecureString password)
        {
            Clear();

            if (protectedString.StartsWith("e"))
            {
                LoadFromEncryptedString(protectedString, password);
            }
            else if (protectedString.StartsWith("p"))
            {
                // Trim leading 'p', which is just an indicator to say that this is an encrypted and not a protected string
                protectedString = protectedString.Substring(1);

                IntPtr PasswordPtr = IntPtr.Zero;

                try
                {
                    // Get the secure password string into a memory buffer
                    PasswordPtr = Marshal.SecureStringToGlobalAllocAnsi(password.GetSecureText());
                    int PasswordSize = password.Length * sizeof(byte);

                    // Pin the array, copy data in, use it, and then make sure it is clear before unpinning.
                    unsafe
                    {
                        byte[] Decrypted = null;
                        byte[] PasswordBytes = new byte[PasswordSize];
                        fixed (byte* ptr = PasswordBytes)
                        {
                            try
                            {
                                Marshal.Copy(PasswordPtr, PasswordBytes, 0, PasswordSize);

                                if (PasswordSize == 0)
                                {
                                    Decrypted = ProtectedData.Unprotect(Convert.FromBase64String(protectedString), null, DataProtectionScope.CurrentUser);
                                }
                                else
                                {
                                    Decrypted = ProtectedData.Unprotect(Convert.FromBase64String(protectedString), PasswordBytes, DataProtectionScope.CurrentUser);
                                }

                                for (int i = 0; i < Decrypted.Length; i++)
                                {
                                    if (_SecureStringSupported)
                                    {
                                        ((SecureString)_SecureString).AppendChar((char)Decrypted[i]);
                                    }
                                    else
                                    {
                                        ((StringBuilder)_SecureString).Append((char)Decrypted[i]);
                                    }
                                }
                            }
                            finally
                            {
                                // Ensure managed array is cleared
                                if (Decrypted != null) Array.Clear(Decrypted, 0, Decrypted.Length);
                                Array.Clear(PasswordBytes, 0, PasswordSize);
                            }
                        }
                    }
                }
                finally
                {
                    // Ensure unmanaged memory is released.
                    if (PasswordPtr != IntPtr.Zero)
                    {
                        Marshal.ZeroFreeGlobalAllocAnsi(PasswordPtr);
                    }
                }
            }
        }

        public void RemoveAt(int index)
        {
            if (_SecureStringSupported)
            {
                ((SecureString)_SecureString).RemoveAt(index);
            }
            else
            {
                ((StringBuilder)_SecureString).Remove(index, 1);
            }
        }

        public void SetAt(int index, char c)
        {
            if (_SecureStringSupported)
            {
                ((SecureString)_SecureString).SetAt(index, c);
            }
            else
            {
                InsertAt(index, c);
                RemoveAt(index + 1);
            }
        }
    }
}
