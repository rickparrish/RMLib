using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

// Inspired by FleckLog: https://github.com/statianzo/Fleck/blob/master/src/Fleck/FleckLog.cs
namespace RandM.RMLib
{
    public enum LogLevel
    {
        /// <summary>
        /// Use for extremely detailed information that isn't likely to be useful for anybody but the developer
        /// </summary>
        Trace,

        /// <summary>
        /// Use for information that may be useful for a user to provide to a developer if there are problems
        /// </summary>
        Debug,

        /// <summary>
        /// Use for information that is "business as usual", which is likely to be output to screen
        /// </summary>
        Info,

        /// <summary>
        /// Use for information related to unexpected non-fatal conditions
        /// </summary>
        Warning,

        /// <summary>
        /// Use for information related to unexpected fatal conditions
        /// </summary>
        Error
    }

    public class RMLog
    {
        /// <summary>
        /// Determine which events get raised to the application
        /// </summary>
        public static LogLevel Level = LogLevel.Info;

        public static event EventHandler<RMLogEventArgs> Handler = null;

        /// <summary>
        /// Raises an Debug-level event
        /// </summary>
        /// <param name="message">The message to raise</param>
        public static void Debug(string message)
        {
            if (Level <= LogLevel.Debug)
            {
                new RMLogEventArgs(LogLevel.Debug, message).Raise(null, Handler);
            }
        }
        
        /// <summary>
        /// Raises an Error-level event
        /// </summary>
        /// <param name="message">The message to raise</param>
        public static void Error(string message)
        {
            if (Level <= LogLevel.Error)
            {
                new RMLogEventArgs(LogLevel.Error, message).Raise(null, Handler);
            }
        }

        /// <summary>
        /// Raises an Error-level event, including information about what caused the exception
        /// </summary>
        /// <param name="ex">The exception that occurred</param>
        /// <param name="message">A message describing what happened</param>
        public static void Exception(Exception ex, string message)
        {
            if (Level <= LogLevel.Error)
            {
                var Trace = new StackTrace(ex, true);
                var Frame = Trace.GetFrame(0);
                var Method = Frame.GetMethod();
                message = string.Format("Message: {0}\r\nFile: {1}:{2},{3}\r\nMethod: {4}::{5}\r\nException: {6}",
                    message,
                    Frame.GetFileName(),
                    Frame.GetFileLineNumber(),
                    Frame.GetFileColumnNumber(),
                    Method.DeclaringType,
                    Method.Name,
                    (Level <= LogLevel.Debug) ? ex.ToString() : ex.Message);

                new RMLogEventArgs(LogLevel.Error, message).Raise(null, Handler);
            }
        }

        /// <summary>
        /// Raises an Info-level event
        /// </summary>
        /// <param name="message">The message to raise</param>
        public static void Info(string message)
        {
            if (Level <= LogLevel.Info)
            {
                new RMLogEventArgs(LogLevel.Info, message).Raise(null, Handler);
            }
        }

        /// <summary>
        /// Raises an Trace-level event
        /// </summary>
        /// <param name="message">The message to raise</param>
        public static void Trace(string message)
        {
            if (Level <= LogLevel.Trace)
            {
                new RMLogEventArgs(LogLevel.Trace, message).Raise(null, Handler);
            }
        }

        /// <summary>
        /// Raises an Warning-level event
        /// </summary>
        /// <param name="message">The message to raise</param>
        public static void Warning(string message)
        {
            if (Level <= LogLevel.Warning)
            {
                new RMLogEventArgs(LogLevel.Warning, message).Raise(null, Handler);
            }
        }
    }
}
