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
        /// Basically the same as Error, but also accepts an exception object whose stack trace will be returned if in trace or debug modes
        /// </summary>
        /// <param name="ex">The exception that occurred</param>
        /// <param name="message">A message describing what happened</param>
        public static void Exception(Exception ex, string message)
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
}
