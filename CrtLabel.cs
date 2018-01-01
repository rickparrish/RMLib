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
namespace RandM.RMLib
{
    public sealed class CrtLabel : CrtControl
    {
        public CrtLabel(CrtControl parent, int column, int row, int width) : this(parent, column, row, width, "", CrtAlignment.Left, Crt.LightGray, Crt.Black) { }
        public CrtLabel(CrtControl parent, int column, int row, int width, string text) : this(parent, column, row, width, text, CrtAlignment.Left, Crt.LightGray, Crt.Black) { }
        public CrtLabel(CrtControl parent, int column, int row, int width, string text, CrtAlignment textAlignment) : this(parent, column, row, width, text, textAlignment, Crt.LightGray, Crt.Black) { }
        public CrtLabel(CrtControl parent, int column, int row, int width, string text, CrtAlignment textAlignment, int foreColour) : this(parent, column, row, width, text, textAlignment, foreColour, Crt.Black) { }
        public CrtLabel(CrtControl parent, int column, int row, int width, string text, CrtAlignment textAlignment, int foreColour, int backColour)
            : base(parent, column, row, width, 1)
        {
            _Text = text;
            _TextAlignment = textAlignment;
            _ForeColour = foreColour;
            _BackColour = backColour;

            Paint(true);
        }

        protected override void Paint(bool AForce)
        {
            // Draw the message
            switch (_TextAlignment)
            {
                case CrtAlignment.Center:
                    if (Text.Length >= Width)
                    {
                        // Text is greater than available space so chop it off with PadRight()
                        Crt.FastWrite(StringUtils.PadRight(Text, ' ', Width), ScreenLeft, ScreenTop, _ForeColour, _BackColour);
                    }
                    else
                    {
                        // Text needs to be centered
                        int LeftSpaces = (Width - Text.Length) / 2;
                        int RightSpaces = Width - Text.Length - LeftSpaces;
                        Crt.FastWrite(new string(' ', LeftSpaces) + Text + new string(' ', RightSpaces), ScreenLeft, ScreenTop, _ForeColour, _BackColour);
                    }
                    break;
                case CrtAlignment.Left:
                    Crt.FastWrite(StringUtils.PadRight(Text, ' ', Width), ScreenLeft, ScreenTop, _ForeColour, _BackColour);
                    break;
                case CrtAlignment.Right:
                    Crt.FastWrite(StringUtils.PadLeft(Text, ' ', Width), ScreenLeft, ScreenTop, _ForeColour, _BackColour);
                    break;
            }
        }

        public string Text
        {
            get
            {
                return _Text;
            }
            set
            {
                _Text = value;
                Paint(true);
            }
        }
        private string _Text = "";

        public CrtAlignment TextAlign
        {
            get
            {
                return _TextAlignment;
            }
            set
            {
                if (value != _TextAlignment)
                {
                    _TextAlignment = value;
                    Paint(true);
                }
            }
        }
        private CrtAlignment _TextAlignment;
    }
}
