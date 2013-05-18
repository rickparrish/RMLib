using System;

namespace RandM.RMLib
{
    public static class AmIOnline
    {
        static private object _Lock = new object();
        static private string[] _OnlineCheckHosts = { "akamai.com", "baidu.com", "blogspot.com", "cachefly.com", "cdnetworks.com", "cloudflare.com", "facebook.com", "gmail.com", "google.com", "hotmail.com", "linkedin.com", "qq.com", "twitter.com", "wikipedia.org", "wordpress.com", "yahoo.com", "youtube.com" };
        static private int _OnlineCheckIndex = new Random().Next(0, _OnlineCheckHosts.Length);

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
