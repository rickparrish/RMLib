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
using System.Text;
using System.Collections;
using System.Web;

namespace RandM.RMLib
{
    public class SessionCurrentUser
    {
        public string Email
        {
            get { return _Email; }
            set { _Email = value; Save(); }
        }
        private string _Email = "";

        public string FirstName
        {
            get { return _FirstName; }
            set { _FirstName = value; Save(); }
        }
        private string _FirstName = "";

        public int GroupId
        {
            get { return _GroupId; }
            set { _GroupId = value; Save(); }
        }
        private int _GroupId = 0;

        public string LastName
        {
            get { return _LastName; }
            set { _LastName = value; Save(); }
        }
        private string _LastName = "";

        public string LFE
        {
            get { return _LFE; }
            set { _LFE = value; Save(); }
        }
        private string _LFE = "";

        public string Password
        {
            get { return _Password; }
            set { _Password = value; Save(); }
        }
        private string _Password = "";

        public int UserId
        {
            get { return _UserId; }
            set { _UserId = value; Save(); }
        }
        private int _UserId = 0;

        public string Username
        {
            get { return _Username; }
            set { _Username = value; Save(); }
        }
        private string _Username = "";

        public SessionCurrentUser()
        {
            if (HttpContext.Current.Session["SessionCurrentUser"] != null)
            {
                Hashtable HT = (Hashtable)HttpContext.Current.Session["SessionCurrentUser"];
                _Email = HT["Email"].ToString();
                _FirstName = HT["FirstName"].ToString();
                _GroupId = (int)HT["GroupId"];
                _LastName = HT["LastName"].ToString();
                _LFE = HT["LFE"].ToString();
                _Password = HT["Password"].ToString();
                _UserId = (int)HT["UserId"];
                _Username = HT["Username"].ToString();
            }
        }

        public bool IsLoggedIn()
        {
            return ((_GroupId > 0) && (_UserId > 0));
        }

        public void LogOut()
        {
            HttpContext.Current.Session["SessionCurrentUser"] = null;
            _Email = "";
            _FirstName = "";
            _GroupId = 0;
            _LastName = "";
            _LFE = "";
            _Password = "";
            _UserId = 0;
            _Username = "";
        }

        private void Save()
        {
            Hashtable HT = new Hashtable();
            HT["Email"] = _Email;
            HT["FirstName"] = _FirstName;
            HT["GroupId"] = _GroupId;
            HT["LastName"] = _LastName;
            HT["LFE"] = _LFE;
            HT["Password"] = _Password;
            HT["UserId"] = _UserId;
            HT["Username"] = _Username;
            HttpContext.Current.Session["SessionCurrentUser"] = HT;
        }
    }
}
