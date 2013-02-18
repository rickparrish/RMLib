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
using System.Globalization;

namespace RandM.RMLib
{
    public class CrtProgressBar : CrtControl
    {
        private int _LastBarWidth = 9999;
        private DateTime _LastMarqueeUpdate = DateTime.Now;
        private string _LastPercentText = "";
        private object _Lock = new object();

        public CrtProgressBar(CrtControl parent, int column, int row, int width) : this(parent, column, row, width, ProgressBarStyle.Blocks) { }

        /// <summary>
        /// Initializes a progress bar with the given name and details, and draws the 0% progress to the screen
        /// </summary>
        /// <param name="AX">The x coordinate of the bar</param>
        /// <param name="AY">The y coordinate of the bar</param>
        /// <param name="AWidth">The width of the bar</param>
        /// <param name="AStyle">The style of the bar</param>
        /// <param name="AFG">The foreground colour of the completed bar</param>
        /// <param name="ABG">The background colour of the bar</param>
        /// <param name="AShaded">The foreground colour of the uncompleted bar</param>
        public CrtProgressBar(CrtControl AParent, int ALeft, int ATop, int AWidth, ProgressBarStyle AStyle)
            : base(AParent, ALeft, ATop, AWidth, 1)
        {
            _Style = AStyle;

            _BackColour = Crt.Blue;
            _BarForeColour = Crt.Yellow;
            _BlankForeColour = Crt.LightGray;
            _LastMarqueeUpdate = DateTime.Now;
            _MarqueeAnimationSpeed = 25;
            _Maximum = 100;
            _PercentPrecision = 2;
            _PercentVisible = true;
            _Value = 0;

            Paint(true);
        }

        public int BarForeColour
        {
            get
            {
                return _BarForeColour;
            }
            set
            {
                if (value != _BarForeColour)
                {
                    _BarForeColour = value;
                    Paint(true);
                }
            }
        }
        private int _BarForeColour;

        public int BlankForeColour
        {
            get
            {
                return _BlankForeColour;
            }
            set
            {
                if (value != _BlankForeColour)
                {
                    _BlankForeColour = value;
                    Paint(true);
                }
            }
        }
        private int _BlankForeColour;

        public int MarqueeAnimationSpeed
        {
            get
            {
                return _MarqueeAnimationSpeed;
            }
            set
            {
                _MarqueeAnimationSpeed = value;
            }
        }
        private int _MarqueeAnimationSpeed;

        public long Maximum
        {
            get
            {
                return _Maximum;
            }
            set
            {
                if (value != _Maximum)
                {
                    _Maximum = value;
                    if (_Value > _Maximum) Value = _Maximum;
                    Paint(true);
                }
            }
        }
        private long _Maximum;

        /// <summary>
        /// Re-Draw the bar and percent text.
        /// </summary>
        /// <param name="AForce">When true, the bar and percent will always be Paintn.  When false, the bar and percent will only be Paintn as necessary, which reduces the number of unnecessary Paints (especially when a large maximum is used)</param>
        protected override void Paint(bool AForce)
        {
            if (Style == ProgressBarStyle.Marquee)
            {
                if (AForce)
                {
                    // Erase the old bar
                    Crt.FastWrite(new string((char)176, _Width), ScreenLeft, ScreenTop, _BlankForeColour, _BackColour);
                }

                // Draw the new bar
                if (_Value > 0)
                {
                    if (_Value > _Width)
                    {
                        Crt.FastWrite(((char)176).ToString(), ScreenLeft + _Width - (15 - (int)(_Value - _Width)), ScreenTop, _BlankForeColour, _BackColour);
                    }
                    else if (_Value >= 15)
                    {
                        string Bar = new string((char)219, (int)Math.Min(_Value, 15));
                        Crt.FastWrite(Bar, ScreenLeft + (int)_Value - 15, ScreenTop, _BarForeColour, _BackColour);
                        Crt.FastWrite(((char)176).ToString(), ScreenLeft + (int)_Value - 15, ScreenTop, _BlankForeColour, _BackColour);
                    }
                    else
                    {
                        string Bar = new string((char)219, (int)Math.Min(_Value, 15));
                        Crt.FastWrite(Bar, ScreenLeft, ScreenTop, _BarForeColour, _BackColour);
                    }
                }
            }
            else
            {
                // Check if we're forcing an update (probably due to a change in Left, Top, Width, etc)
                if (AForce)
                {
                    // Yep, so reset the "Last" variables
                    _LastBarWidth = 9999;
                    _LastPercentText = "";
                }

                bool PaintPercentText = false;
                double Percent = ((double)Value / (double)Maximum);
                int NewBarWidth = (int)(Percent * _Width);
                if (NewBarWidth != _LastBarWidth)
                {
                    // Check if the bar shrank (if so, we need to delete the old bar)
                    if (NewBarWidth < _LastBarWidth)
                    {
                        // Erase the old bar
                        Crt.FastWrite(new string((char)176, _Width), ScreenLeft, ScreenTop, _BlankForeColour, _BackColour);
                    }

                    // Draw the new bar
                    Crt.FastWrite(new string((char)_Style, NewBarWidth), ScreenLeft, ScreenTop, _BarForeColour, _BackColour);

                    _LastBarWidth = NewBarWidth;
                    PaintPercentText = true;
                }

                // Draw the percentage
                if (_PercentVisible)
                {
                    string NewPercentText = Percent.ToString("P" + _PercentPrecision.ToString());
                    if ((NewPercentText != _LastPercentText) || (PaintPercentText))
                    {
                        _LastPercentText = NewPercentText;

                        int ProgressStart = (Width - NewPercentText.Length) / 2;
                        if (ProgressStart >= NewBarWidth)
                        {
                            // Bar hasn't reached the percent text, so draw in the bar's empty color
                            Crt.FastWrite(NewPercentText, ScreenLeft + ProgressStart, ScreenTop, _BlankForeColour, _BackColour);
                        }
                        else if (ProgressStart + NewPercentText.Length <= NewBarWidth)
                        {
                            // Bar has passed the percent text, so draw in the bar's foreground colour (or still use background for Blocks)
                            Crt.FastWrite(NewPercentText, ScreenLeft + ProgressStart, ScreenTop, _BackColour, _BarForeColour);

                        }
                        else
                        {
                            // Bar is in the middle of the percent text, so draw the colour as necessary for each letter in the text
                            for (int i = 0; i < NewPercentText.Length; i++)
                            {
                                int LetterPosition = ProgressStart + i;
                                int FG = (LetterPosition >= NewBarWidth) ? _BlankForeColour : _BackColour;
                                int BG = (LetterPosition >= NewBarWidth) ? _BackColour : _BarForeColour;
                                Crt.FastWrite(NewPercentText[i].ToString(), ScreenLeft + LetterPosition, ScreenTop, FG, BG);
                            }
                        }
                    }
                }
            }
        }

        public int PercentPrecision
        {
            get
            {
                return _PercentPrecision;
            }
            set
            {
                if (value != _PercentPrecision)
                {
                    _PercentPrecision = value;
                    Paint(true);
                }
            }
        }
        private int _PercentPrecision;

        public bool PercentVisible
        {
            get
            {
                return _PercentVisible;
            }
            set
            {
                if (value != _PercentVisible)
                {
                    _PercentVisible = value;
                    Paint(true);
                }
            }
        }
        private bool _PercentVisible;

        public void Step()
        {
            StepBy(1);
        }

        public void StepBy(int stepSize)
        {
            Value += stepSize;
        }

        public ProgressBarStyle Style
        {
            get
            {
                return _Style;
            }
            set
            {
                if (value != _Style)
                {
                    _Style = value;
                    Paint(true);
                }
            }
        }
        private ProgressBarStyle _Style;

        public long Value
        {
            get
            {
                return _Value;
            }
            set
            {
                if (value != _Value)
                {
                    lock (_Lock)
                    {
                        if (_Style == ProgressBarStyle.Marquee)
                        {
                            if (DateTime.Now.Subtract(_LastMarqueeUpdate).TotalMilliseconds >= _MarqueeAnimationSpeed)
                            {
                                // Keep value between 0 and Maximum + 15
                                if (value < 0) value = 0;
                                if (value >= _Width + 15) value = 0;
                                _Value = value;
                                Paint(false);
                                _LastMarqueeUpdate = DateTime.Now;
                            }
                        }
                        else
                        {
                            // Keep value between 0 and Maximum
                            _Value = Math.Max(0, Math.Min(value, Maximum));
                            Paint(false);
                        }
                    }
                }
            }
        }
        private long _Value;

        public enum ProgressBarStyle
        {
            Blocks = 254,
            Continuous = 219,
            Marquee = 0
        }
    }
}
