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
using System.Threading;
using System;

namespace RandM.RMLib
{
    public abstract class RMThread : IDisposable
    {
        public event EventHandler FinishEvent = null;

        protected volatile bool _Paused = false;
        protected volatile bool _Stop = false;
        protected AutoResetEvent _StopEvent = new AutoResetEvent(false);
        private Thread _Thread = null;

        public bool Aborted { get { return _Stop; } }

        private void CallExecuteAndRaiseFinish()
        {
            Execute();
            RaiseFinishEvent();
        }

        protected abstract void Execute();

        public virtual bool Join(int milliseconds)
        {
            if (_Thread.IsAlive)
            {
                return _Thread.Join(milliseconds);
            }
            else
            {
                return true;
            }
        }

        public virtual void Pause()
        {
            _Paused = true;
        }

        public virtual void RaiseFinishEvent()
        {
            FinishEvent?.Invoke(this, EventArgs.Empty);
        }

        public virtual void Resume() {
            _Paused = false;
        }

        public virtual void Start()
        {
            // Reset the paused state
            _Paused = false;

            // Create Thread object
            _Thread = new Thread(CallExecuteAndRaiseFinish);

            // And start the thread
            _Thread.Start();
        }

        public virtual void Stop()
        {
            _Stop = true;
            _StopEvent.Set();
        }

        public virtual void WaitFor()
        {
            if (_Thread.IsAlive) _Thread.Join();
        }

        #region IDisposable Support
        protected bool _Disposed = false; // To detect redundant calls

        ~RMThread()
        {
            // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
            Dispose(false);
        }

        // This code added to correctly implement the disposable pattern.
        public void Dispose()
        {
            // Do not change this code. Put cleanup code in Dispose(bool disposing).
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (!_Disposed)
            {
                if (disposing)
                {
                    // dispose managed state (managed objects).
                    if (_StopEvent != null) {
                        _StopEvent.Close();
                        _StopEvent = null;
                    }
                }

                // free unmanaged resources (unmanaged objects)
                // set large fields to null.

                _Disposed = true;
            }
        }
        #endregion
    }
}
