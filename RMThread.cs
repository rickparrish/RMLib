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
            _Paused = !_Paused;
        }

        public virtual void RaiseFinishEvent()
        {
            EventHandler LocalHandler = FinishEvent;
            if (LocalHandler != null) LocalHandler(this, EventArgs.Empty);
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
        private bool disposedValue = false; // To detect redundant calls

        protected virtual void Dispose(bool disposing)
        {
            if (!disposedValue)
            {
                if (disposing)
                {
                    // TODO: dispose managed state (managed objects).
                    if (_StopEvent != null) _StopEvent.Close();
                }

                // TODO: free unmanaged resources (unmanaged objects) and override a finalizer below.
                // TODO: set large fields to null.

                disposedValue = true;
            }
        }

        // TODO: override a finalizer only if Dispose(bool disposing) above has code to free unmanaged resources.
        // ~RMThread() {
        //   // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
        //   Dispose(false);
        // }

        // This code added to correctly implement the disposable pattern.
        public void Dispose()
        {
            // Do not change this code. Put cleanup code in Dispose(bool disposing) above.
            Dispose(true);
            // TODO: uncomment the following line if the finalizer is overridden above.
            // GC.SuppressFinalize(this);
        }
        #endregion
    }
}
