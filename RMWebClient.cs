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
using System.Collections.Generic;
using System.Text;
using System.Net;

namespace RandM.RMLib
{
    public class RMWebClient : WebClient
    {
        private CookieContainer _CookieContainer = new CookieContainer();

        public string ContentType { get; set; }
        public int Timeout { get; set; }
        public string UserAgent { get; set; }

        public RMWebClient() : base()
        {
            ContentType = "";
            Timeout = 0;
            UserAgent = "";
        }

        protected override WebRequest GetWebRequest(Uri url)
        {
            WebRequest Request = base.GetWebRequest(url);
            
            if (Request is HttpWebRequest)
            {
                (Request as HttpWebRequest).CookieContainer = _CookieContainer; // Enable the client to receive/respond with cookies
                (Request as HttpWebRequest).Proxy = null; // Disable auto-detect, which can really slow things down
                if (!string.IsNullOrEmpty(UserAgent)) (Request as HttpWebRequest).UserAgent = UserAgent;
            }
            if (!string.IsNullOrEmpty(ContentType)) Request.ContentType = ContentType;
            if (Timeout > 0) Request.Timeout = Timeout;

            return Request;
        }
    }
}
