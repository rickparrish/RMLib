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
using System.Globalization;

namespace RandM.RMLib
{
    public sealed class CrtPanel : CrtControl 
    {

        public CrtPanel(CrtControl parent, int column, int row, int width, int height) : this(parent, column, row, width, height, BorderStyle.Single, Crt.White, Crt.Blue) { }
        public CrtPanel(CrtControl parent, int column, int row, int width, int height, BorderStyle ABorder, int AForeColour, int backColour) : this(parent, column, row, width, height, ABorder, AForeColour, backColour, "") { }
        public CrtPanel(CrtControl parent, int column, int row, int width, int height, BorderStyle ABorder, int AForeColour, int backColour, string text) : this(parent, column, row, width, height, ABorder, AForeColour, backColour, text, CrtAlignment.TopLeft) { }
        public CrtPanel(CrtControl parent, int column, int row, int width, int height, BorderStyle ABorder, int AForeColour, int backColour, string text, CrtAlignment textAlignment) : base(parent, column, row, width, height)
        {
            _Border = ABorder;
            _ForeColour = AForeColour;
            _BackColour = backColour;
            _Text = text;
            _TextAlignment = textAlignment;

            Paint(true);
        }

        public BorderStyle Border
        {
            get
            {
                return _Border;
            }
            set
            {
                if (value != _Border)
                {
                    _Border = value;
                    Paint(true);
                }
            }
        }
        private BorderStyle _Border;

        protected override void Paint(bool AForce)
        {
            // Characters for the box
            char TopLeft = '\0';
            char TopRight = '\0';
            char BottomLeft = '\0';
            char BottomRight = '\0';
            char TopBottom = '\0';
            char LeftRight = '\0';

            // Determine which character set to use
            switch (_Border)
            {
                case BorderStyle.Single:
                    TopLeft = (char)218;
                    TopRight = (char)191;
                    BottomLeft = (char)192;
                    BottomRight = (char)217;
                    TopBottom = (char)196;
                    LeftRight = (char)179;
                    break;
                case BorderStyle.Double:
                    TopLeft = (char)201;
                    TopRight = (char)187;
                    BottomLeft = (char)200;
                    BottomRight = (char)188;
                    TopBottom = (char)205;
                    LeftRight = (char)186;
                    break;
                case BorderStyle.DoubleH:
                case BorderStyle.SingleV:
                    TopLeft = (char)213;
                    TopRight = (char)184;
                    BottomLeft = (char)212;
                    BottomRight = (char)190;
                    TopBottom = (char)205;
                    LeftRight = (char)179;
                    break;
                case BorderStyle.DoubleV:
                case BorderStyle.SingleH:
                    TopLeft = (char)214;
                    TopRight = (char)183;
                    BottomLeft = (char)211;
                    BottomRight = (char)189;
                    TopBottom = (char)196;
                    LeftRight = (char)186;
                    break;
            }

            // Draw top row
            Crt.FastWrite(TopLeft.ToString() + new string(TopBottom, _Width - 2) + TopRight.ToString(), ScreenLeft, ScreenTop, _ForeColour, _BackColour);

            // Draw middle rows
            for (int Line = ScreenTop + 1; Line < ScreenTop + _Height - 1; Line++)
            {
                Crt.FastWrite(LeftRight.ToString() + new string(' ', _Width - 2) + LeftRight.ToString(), ScreenLeft, Line, _ForeColour, _BackColour);
            }

            // Draw bottom row
            Crt.FastWrite(BottomLeft.ToString() + new string(TopBottom, _Width - 2) + BottomRight.ToString(), ScreenLeft, ScreenTop + _Height - 1, _ForeColour, _BackColour);

            // Draw window title
            if (_Text.Trim().Length > 0)
            {
                int TitleX = 0;
                int TitleY = 0;
                string WindowTitle = " " + _Text.Trim() + " ";

                // Get X component
                switch (_TextAlignment)
                {
                    case CrtAlignment.BottomLeft:
                    case CrtAlignment.MiddleLeft:
                    case CrtAlignment.TopLeft:
                        TitleX = ScreenLeft + 2;
                        break;
                    case CrtAlignment.BottomCenter:
                    case CrtAlignment.MiddleCenter:
                    case CrtAlignment.TopCenter:
                        TitleX = ScreenLeft + ((_Width - WindowTitle.Length) / 2);
                        break;
                    case CrtAlignment.BottomRight:
                    case CrtAlignment.MiddleRight:
                    case CrtAlignment.TopRight:
                        TitleX = ScreenLeft + Width - WindowTitle.Length - 2;
                        break;
                }

                // Get the Y component
                switch (_TextAlignment)
                {
                    case CrtAlignment.BottomCenter:
                    case CrtAlignment.BottomLeft:
                    case CrtAlignment.BottomRight:
                        TitleY = ScreenTop + _Height - 1;
                        break;
                    case CrtAlignment.MiddleCenter:
                    case CrtAlignment.MiddleLeft:
                    case CrtAlignment.MiddleRight:
                    case CrtAlignment.TopCenter:
                    case CrtAlignment.TopLeft:
                    case CrtAlignment.TopRight:
                        TitleY = ScreenTop;
                        break;
                }

                // Draw title
                Crt.FastWrite(WindowTitle, TitleX, TitleY, _ForeColour, _BackColour);
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

        /// <summary>
        /// Different border styles used by the CrtPanel class
        /// </summary>
        public enum BorderStyle
        {
            /// <summary>
            /// Single lines all around
            /// </summary>
            Single,

            /// <summary>
            /// Double lines all around
            /// </summary>
            Double,

            /// <summary>
            /// Single lines horizontally, double lines vertically
            /// </summary>
            /// <see>DoubleV</see>
            SingleH,

            /// <summary>
            /// Single lines vertically, double lines horizontally
            /// </summary>
            /// <see>DoubleH</see>
            SingleV,

            /// <summary>
            /// Double lines horizontally, single lines vertically
            /// </summary>
            /// <see>SingleV</see>
            DoubleH,

            /// <summary>
            /// Double lines vertically, single lines horizontally
            /// </summary>
            /// <see>SingleH</see>
            DoubleV
        }
    }
}
