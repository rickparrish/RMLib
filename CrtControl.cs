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
using System.Collections.Generic;

namespace RandM.RMLib
{
    public abstract class CrtControl
    {
        private NativeMethods.CHAR_INFO[] FBackground = null;
        protected List<CrtControl> _Controls = new List<CrtControl>();

        protected CrtControl(CrtControl parent, int column, int row, int width, int height)
        {
            _Parent = parent;
            _Left = column;
            _Top = row;
            _Width = width;
            _Height = height;

            SaveBackground();

            if (_Parent != null) parent.AddControl(this);
        }

        protected void AddControl(CrtControl child)
        {
            _Controls.Add(child);
        }

        public int BackColour
        {
            get
            {
                return _BackColour;
            }
            set
            {
                if (value != _BackColour)
                {
                    _BackColour = value;
                    Paint(true);
                }
            }
        }
        protected int _BackColour = Crt.Black;

        public int ForeColour
        {
            get
            {
                return _ForeColour;
            }
            set
            {
                if (value != _ForeColour)
                {
                    _ForeColour = value;
                    Paint(true);
                }
            }
        }
        protected int _ForeColour = Crt.LightGray;

        public int Height
        {
            get
            {
                return _Height;
            }
            set
            {
                if (value != _Height)
                {
                    RestoreBackground();
                    _Height = value;
                    SaveBackground();
                    Paint(true);
                }
            }
        }
        protected int _Height;

        public void Hide()
        {
            RestoreBackground();
        }

        public int Left
        {
            get
            {
                return _Left;
            }
            set
            {
                if (value != _Left)
                {
                    RestoreBackground();
                    _Left = value;
                    SaveBackground();
                    Paint(true);

                    for (int i = 0; i < _Controls.Count; i++) _Controls[i].Paint(true);
                }
            }
        }
        protected int _Left;

        protected abstract void Paint(bool force);

        protected CrtControl Parent
        {
            get
            {
                return _Parent;
            }
            set
            {
                RestoreBackground();
                _Parent = value;
                SaveBackground();
                Paint(true);
            }
        }
        protected CrtControl _Parent = null;

        private void RestoreBackground()
        {
            Crt.RestoreScreen(FBackground, _Left, _Top, _Left + _Width - 1, _Top + _Height - 1);
        }

        private void SaveBackground()
        {
            FBackground = Crt.SaveScreen(_Left, _Top, _Left + _Width - 1, _Top + _Height - 1);
        }

        protected int ScreenLeft
        {
            get
            {
                return _Left + ((_Parent == null) ? 0 : _Parent.Left);
            }
        }

        protected int ScreenTop
        {
            get
            {
                return _Top + ((_Parent == null) ? 0 : _Parent.Top);
            }
        }

        public void Show()
        {
            Paint(true);
        }

        public int Top
        {
            get
            {
                return _Top;
            }
            set
            {
                if (value != _Top)
                {
                    RestoreBackground();
                    _Top = value;
                    SaveBackground();
                    Paint(true);

                    for (int i = 0; i < _Controls.Count; i++) _Controls[i].Paint(true);
                }
            }
        }
        protected int _Top;

        public int Width
        {
            get
            {
                return _Width;
            }
            set
            {
                if (value != _Width)
                {
                    RestoreBackground();
                    _Width = value;
                    SaveBackground();
                    Paint(true);
                }
            }
        }
        protected int _Width;
    }
}
