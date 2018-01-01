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
using System.Collections.Generic;
using System.Data;
using System.Data.SQLite;
using System.Diagnostics;
using System.IO;
using System.Globalization;
using System.Web;

namespace RandM.RMLib
{
    public class RMSQLiteConnection : IDisposable
    {
        private bool _Disposed = false;

        private SQLiteConnection _Connection = null;
        private SQLiteTransaction _Transaction = null;

        private List<Parameter> _Parameters = new List<Parameter>();

        public SQLiteDataReader Reader = null;

        // Constructor -- loads CURRENTEXE.sqlite from application's directory, with optional transaction
        public RMSQLiteConnection(bool transaction)
        {
            if (HttpContext.Current == null)
            {
                Init(Path.ChangeExtension(ProcessUtils.ExecutablePath, ".sqlite"), transaction);
            }
            else
            {
                Init(StringUtils.PathCombine(HttpContext.Current.Server.MapPath(".\\App_Data"), "default.sqlite"), transaction);
            }
        }

        // Constructor -- loads ADBile from application's directory, with optional transaction
        public RMSQLiteConnection(string fileName, bool transaction)
        {
            if (HttpContext.Current == null)
            {
                if (!Path.IsPathRooted(fileName))
                {
                    fileName = StringUtils.PathCombine(ProcessUtils.StartupPath, fileName);
                }
                Init(fileName, transaction);
            }
            else
            {
                if (!Path.IsPathRooted(fileName))
                {
                    fileName = StringUtils.PathCombine(HttpContext.Current.Server.MapPath(".\\App_Data"), fileName);
                }
                Init(fileName, transaction);
            }
        }

        // Constructor -- loads ADBFile from ADBPath, with optional transaction
        public RMSQLiteConnection(string directoryName, string fileName, bool transaction)
        {
            Init(StringUtils.PathCombine(directoryName, fileName), transaction);
        }

        ~RMSQLiteConnection()
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
                    Close();
                }

                // Call the appropriate methods to clean up
                // unmanaged resources here.
                // If disposing is false,
                // only the following code is executed.


                // Note disposing has been done.
                _Disposed = true;
            }
        }

        public string AddDateTimeParameter(DateTime value)
        {
            _Parameters.Add(new Parameter("@P" + (_Parameters.Count + 1).ToString(), value));
            return " @P" + (_Parameters.Count).ToString() + " ";
        }

        public string AddDoubleParameter(double value)
        {
            _Parameters.Add(new Parameter("@P" + (_Parameters.Count + 1).ToString(), value));
            return " @P" + (_Parameters.Count).ToString() + " ";
        }

        public string AddIntParameter(int value)
        {
            _Parameters.Add(new Parameter("@P" + (_Parameters.Count + 1).ToString(), value));
            return " @P" + (_Parameters.Count).ToString() + " ";
        }

        public string AddVarCharParameter(string value)
        {
            _Parameters.Add(new Parameter("@P" + (_Parameters.Count + 1).ToString(), value));
            return " @P" + (_Parameters.Count).ToString() + " ";
        }

        public void Close()
        {
            CloseReader();
            if (_Connection != null)
            {
                _Connection.Close();
                _Connection.Dispose();
                _Connection = null;
            }
        }

        private void CloseReader()
        {
            if ((Reader != null) && (!Reader.IsClosed))
            {
                Reader.Close();
            }
            Reader = null;
        }

        public void Commit()
        {
            CloseReader();
            _Transaction.Commit();
            _Transaction.Dispose();
            Close();
        }

        // ExecuteNonQuery executes a query and returns the number of rows affected
        // Useful for INSERT/UPDATE/DELETE operations
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2100:Review SQL queries for security vulnerabilities")]
        public int ExecuteNonQuery(string sqlText)
        {
            using (SQLiteCommand Cmd = new SQLiteCommand(sqlText, _Connection, _Transaction))
            {
                for (int I = 0; I < _Parameters.Count; I++)
                {
                    Cmd.Parameters.Add(_Parameters[I].Name, _Parameters[I].Type, _Parameters[I].Length).Value = _Parameters[I].Value;
                }
                _Parameters.Clear();

                return Cmd.ExecuteNonQuery();
            }
        }

        // ExecuteReader executes a query and returns an SQLDataReader object
        // Useful for SELECT operations returning multiple columns/rows
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2100:Review SQL queries for security vulnerabilities")]
        public bool ExecuteReader(string sqlText)
        {
            using (SQLiteCommand Cmd = new SQLiteCommand(sqlText, _Connection, _Transaction))
            {
                for (int I = 0; I < _Parameters.Count; I++)
                {
                    Cmd.Parameters.Add(_Parameters[I].Name, _Parameters[I].Type, _Parameters[I].Length).Value = _Parameters[I].Value;
                }
                _Parameters.Clear();

                Reader = Cmd.ExecuteReader();
                return Reader.HasRows;
            }
        }

        // ExecuteScalar executes a query and returns a plain old Object
        // Useful for SELECT operations returning 1 row with 1 column
        // Don't forget to Convert.ToWhatever the return value
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2100:Review SQL queries for security vulnerabilities")]
        public object ExecuteScalar(string sqlText)
        {
            using (SQLiteCommand Cmd = new SQLiteCommand(sqlText, _Connection, _Transaction))
            {
                for (int I = 0; I < _Parameters.Count; I++)
                {
                    Cmd.Parameters.Add(_Parameters[I].Name, _Parameters[I].Type, _Parameters[I].Length).Value = _Parameters[I].Value;
                }
                _Parameters.Clear();

                return Cmd.ExecuteScalar();
            }
        }

        private void Init(string fileName, bool transaction)
        {
            // Open connection and initiate transaction (if requested)
            _Connection = new SQLiteConnection("Data Source=" + fileName);
            _Connection.Open();
            if (transaction)
            {
                _Transaction = _Connection.BeginTransaction();
            }
        }

        public void Rollback()
        {

            CloseReader();
            _Transaction.Rollback();
            _Transaction.Dispose();
            Close();
        }

        private struct Parameter
        {
            public string Name;
            public string Value;
            public DbType Type;
            public int Length;

            public Parameter(string parameterName, DateTime value)
            {
                Name = parameterName;
                Value = value.ToString("yyyy-MM-dd HH:mm:ss.fffffff");
                Type = DbType.DateTime;
                Length = 8;
            }

            public Parameter(string parameterName, double value)
            {
                Name = parameterName;
                Value = value.ToString();
                Type = DbType.Double;
                Length = 16;
            }

            public Parameter(string parameterName, int value)
            {
                Name = parameterName;
                Value = value.ToString();
                Type = DbType.Int64;
                Length = 8;
            }

            public Parameter(string parameterName, string value)
            {
                Name = parameterName;
                Value = value;
                Type = DbType.String;
                Length = value.Length;
            }
        }
    }
}
