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
using System.Net.Mail;
using System.Web;
using System.Collections.Specialized;
using System.Web.SessionState;

namespace RandM.RMLib
{
    public static class ExceptionEmailHelper
    {
        private static string AddCollection(string title, HttpCookieCollection cookieCollection)
        {
            string Result = "";
            Result += "<div style='background-color: #999999;'><h1>" + title + "</h1><ol>";
            for (int i = 0; i < cookieCollection.Count; i++)
            {
                Result += "<li>" + (cookieCollection.Keys[i] == null ? "<i>Unnamed key</i>" : HttpUtility.HtmlEncode(cookieCollection.Keys[i])) + " = " + (cookieCollection[i].Value == null ? "<i>null</i>" : HttpUtility.HtmlEncode(cookieCollection[i].Value.ToString()) + "</li>\r\n");
            }
            Result += "</ol></div>\r\n";
            
            return Result;
        }

        private static string AddCollection(string title, HttpSessionState sessionState)
        {
            string Result = "";
            Result += "<div style='background-color: #999999;'><h1>" + title + "</h1><ol>";
            for (int i = 0; i < sessionState.Count; i++)
            {
                Result += "<li>" + (sessionState.Keys[i] == null ? "<i>Unnamed key</i>" : HttpUtility.HtmlEncode(sessionState.Keys[i])) + " = " + (sessionState[i] == null ? "<i>null</i>" : HttpUtility.HtmlEncode(sessionState[i].ToString()) + "</li>\r\n");
            }
            Result += "</ol></div>\r\n";

            return Result;
        }

        private static string AddCollection(string title, NameValueCollection nameValues)
        {
            string Result = "";
            Result += "<div style='background-color: #999999;'><h1>" + title + "</h1><ol>";
            for (int i = 0; i < nameValues.Count; i++)
            {
                Result += "<li>" + (nameValues.Keys[i] == null ? "<i>Unnamed key</i>" : HttpUtility.HtmlEncode(nameValues.Keys[i])) + " = " + (nameValues[i] == null ? "<i>null</i>" : HttpUtility.HtmlEncode(nameValues[i].ToString()) + "</li>\r\n");
            }
            Result += "</ol></div>\r\n";

            return Result;
        }

        public static void Send(string smtp, int port, MailAddress from, MailAddress to, MailAddress sender, string subject, string body, string userName, RMSecureString password, bool ssl)
        {
            body = "<div style='background-color: #cccccc;'><h1>Error Message</h1>" + body + "</div>\r\n";
         
            body += AddCollection("QueryString", HttpContext.Current.Request.QueryString);
            body += AddCollection("Form", HttpContext.Current.Request.Form);
            body += AddCollection("Cookies", HttpContext.Current.Request.Cookies);
            body += AddCollection("Session", HttpContext.Current.Session);
            body += AddCollection("ServerVariables", HttpContext.Current.Request.ServerVariables);

            WebUtils.Email(smtp, port, from, to, sender, subject, body, true, userName, password, ssl);
        }
    }
}
