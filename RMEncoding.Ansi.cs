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

namespace RandM.RMLib
{
    class RMEncodingAnsi : Encoding
    {
        public override int GetByteCount(char[] chars, int index, int count)
        {
            if (chars == null) throw new ArgumentNullException("chars");
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            if (count < 0) throw new ArgumentOutOfRangeException("count");
            if (index + count > chars.Length) throw new ArgumentOutOfRangeException("chars");

            int Result = 0;

            int EndIndex = index + count;
            for (; index < EndIndex; index++)
            {
                // ANSI encoding allows for bytes 0 to 255
                if ((chars[index] >= 0) && (chars[index] <= 255)) Result += 1;
            }

            return Result;
        }

        public override int GetBytes(char[] chars, int charIndex, int charCount, byte[] bytes, int byteIndex)
        {
            if (chars == null) throw new ArgumentNullException("chars");
            if (bytes == null) throw new ArgumentNullException("bytes");
            if (charIndex < 0) throw new ArgumentOutOfRangeException("charIndex");
            if (charCount < 0) throw new ArgumentOutOfRangeException("charCount");
            if (byteIndex < 0) throw new ArgumentOutOfRangeException("byteIndex");
            if (charIndex + charCount > chars.Length) throw new ArgumentOutOfRangeException("chars");
            if (byteIndex > bytes.Length) throw new ArgumentOutOfRangeException("byteIndex");
            if (byteIndex + charCount > bytes.Length) throw new ArgumentException("bytes does not have enough capacity from byteIndex to the end of the array to accommodate the resulting bytes.", "bytes");

            int Result = 0;

            int EndIndex = charIndex + charCount;
            for (; charIndex < EndIndex; charIndex++)
            {
                // ANSI encoding allows for bytes 0 to 255
                if ((chars[charIndex] >= 0) && (chars[charIndex] <= 255))
                {
                    bytes[byteIndex++] = (byte)chars[charIndex];
                    Result += 1;
                }
            }

            return Result;
        }

        public override int GetCharCount(byte[] bytes, int index, int count)
        {
            if (bytes == null) throw new ArgumentNullException("bytes");
            if (index < 0) throw new ArgumentOutOfRangeException("index");
            if (count < 0) throw new ArgumentOutOfRangeException("count");
            if (index + count > bytes.Length) throw new ArgumentOutOfRangeException("bytes");
            
            int Result = 0;

            int EndIndex = index + count;
            for (; index < EndIndex; index++)
            {
                // ANSI encoding allows for bytes 0 to 255
                if ((bytes[index] >= 0) && (bytes[index] <= 255)) Result += 1;
            }

            return Result;
        }

        public override int GetChars(byte[] bytes, int byteIndex, int byteCount, char[] chars, int charIndex)
        {
            if (bytes == null) throw new ArgumentNullException("bytes");
            if (chars == null) throw new ArgumentNullException("chars");
            if (byteIndex < 0) throw new ArgumentOutOfRangeException("byteIndex");
            if (byteCount < 0) throw new ArgumentOutOfRangeException("byteCount");
            if (charIndex < 0) throw new ArgumentOutOfRangeException("charIndex");
            if (byteIndex + byteCount > bytes.Length) throw new ArgumentOutOfRangeException("bytes");
            if (charIndex > chars.Length) throw new ArgumentOutOfRangeException("charIndex");
            if (charIndex + byteCount > chars.Length) throw new ArgumentException("chars does not have enough capacity from charIndex to the end of the array to accommodate the resulting characters.", "chars");

            int Result = 0;

            int EndIndex = byteIndex + byteCount;
            for (; byteIndex < EndIndex; byteIndex++)
            {
                // ANSI encoding allows for bytes 0 to 255
                if ((bytes[byteIndex] >= 0) && (bytes[byteIndex] <= 255))
                {
                    chars[charIndex++] = (char)bytes[byteIndex];
                    Result += 1;
                }
            }

            return Result;
        }

        public override int GetMaxByteCount(int charCount)
        {
            if (charCount < 0) throw new ArgumentOutOfRangeException("charCount");

            return charCount;
        }

        public override int GetMaxCharCount(int byteCount)
        {
            if (byteCount < 0) throw new ArgumentOutOfRangeException("byteCount");

            return byteCount;
        }
    }
}
