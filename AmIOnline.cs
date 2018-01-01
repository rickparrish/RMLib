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

namespace RandM.RMLib
{
    public static class AmIOnline
    {
        private static object _Lock = new object();
        private static string[] _OnlineCheckHosts = { "akamai.com", "baidu.com", "blogspot.com", "cachefly.com", "cdnetworks.com", "cloudflare.com", "facebook.com", "gmail.com", "google.com", "hotmail.com", "linkedin.com", "qq.com", "twitter.com", "wikipedia.org", "wordpress.com", "yahoo.com", "youtube.com" };
        private static int _OnlineCheckIndex = new Random().Next(0, _OnlineCheckHosts.Length);

        public static bool PingNextHosts(int tries)
        {
            for (int i = 0; i < tries; i++)
            {
                int NextHostIndex = 0;

                lock (_Lock)
                {
                    _OnlineCheckIndex = (++_OnlineCheckIndex % _OnlineCheckHosts.Length);
                    NextHostIndex = _OnlineCheckIndex;
                }

                if (WebUtils.Ping(_OnlineCheckHosts[NextHostIndex], 5000)) return true;
            }

            return false;
        }

        public static void CheckHosts()
        {
            foreach (string Host in _OnlineCheckHosts)
            {
                if (!WebUtils.Ping(Host, 5000))
                {
                    throw new Exception("Failed to ping " + Host);
                }
            }
            throw new Exception("All hosts pingable");
        }
    }
}
