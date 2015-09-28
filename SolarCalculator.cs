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

  This code file was originally downloaded from the following website:
  Earth Systems Research Laboratory - Global Monitoring Division
  NOAA Solar Calculator
  http://www.esrl.noaa.gov/gmd/grad/solcalc/
  
  It was modified to port (some of) the javascript code to C#, and
  to add an IsSunUp() helper function
*/
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace RandM.RMLib
{
    public class SolarCalculator
    {
        static private double calcTimeJulianCent(double jd)
        {
            var T = (jd - 2451545.0) / 36525.0;
            return T;
        }

        //function calcJDFromJulianCent(t)
        //{
        //  var JD = t * 36525.0 + 2451545.0
        //  return JD
        //}

        //function isLeapYear(yr) 
        //{
        //  return ((yr % 4 == 0 && yr % 100 != 0) || yr % 400 == 0);
        //}

        //function calcDoyFromJD(jd)
        //{
        //  var z = Math.floor(jd + 0.5);
        //  var f = (jd + 0.5) - z;
        //  if (z < 2299161) {
        //    var A = z;
        //  } else {
        //    alpha = Math.floor((z - 1867216.25)/36524.25);
        //    var A = z + 1 + alpha - Math.floor(alpha/4);
        //  }
        //  var B = A + 1524;
        //  var C = Math.floor((B - 122.1)/365.25);
        //  var D = Math.floor(365.25 * C);
        //  var E = Math.floor((B - D)/30.6001);
        //  var day = B - D - Math.floor(30.6001 * E) + f;
        //  var month = (E < 14) ? E - 1 : E - 13;
        //  var year = (month > 2) ? C - 4716 : C - 4715;

        //  var k = (isLeapYear(year) ? 1 : 2);
        //  var doy = Math.floor((275 * month)/9) - k * Math.floor((month + 9)/12) + day -30;
        //  return doy;
        //}

        static private double radToDeg(double angleRad)
        {
            return (180.0 * angleRad / Math.PI);
        }

        static private double degToRad(double angleDeg)
        {
            return (Math.PI * angleDeg / 180.0);
        }

        static private double calcGeomMeanLongSun(double t)
        {
            var L0 = 280.46646 + t * (36000.76983 + t * (0.0003032));
            while (L0 > 360.0)
            {
                L0 -= 360.0;
            }
            while (L0 < 0.0)
            {
                L0 += 360.0;
            }
            return L0;		// in degrees
        }

        static private double calcGeomMeanAnomalySun(double t)
        {
            var M = 357.52911 + t * (35999.05029 - 0.0001537 * t);
            return M;		// in degrees
        }

        static private double calcEccentricityEarthOrbit(double t)
        {
            var e = 0.016708634 - t * (0.000042037 + 0.0000001267 * t);
            return e;		// unitless
        }

        static private double calcSunEqOfCenter(double t)
        {
            var m = calcGeomMeanAnomalySun(t);
            var mrad = degToRad(m);
            var sinm = Math.Sin(mrad);
            var sin2m = Math.Sin(mrad + mrad);
            var sin3m = Math.Sin(mrad + mrad + mrad);
            var C = sinm * (1.914602 - t * (0.004817 + 0.000014 * t)) + sin2m * (0.019993 - 0.000101 * t) + sin3m * 0.000289;
            return C;		// in degrees
        }

        static private double calcSunTrueLong(double t)
        {
            var l0 = calcGeomMeanLongSun(t);
            var c = calcSunEqOfCenter(t);
            var O = l0 + c;
            return O;		// in degrees
        }

        //function calcSunTrueAnomaly(t)
        //{
        //  var m = calcGeomMeanAnomalySun(t);
        //  var c = calcSunEqOfCenter(t);
        //  var v = m + c;
        //  return v;		// in degrees
        //}

        //function calcSunRadVector(t)
        //{
        //  var v = calcSunTrueAnomaly(t);
        //  var e = calcEccentricityEarthOrbit(t);
        //  var R = (1.000001018 * (1 - e * e)) / (1 + e * Math.Cos(degToRad(v)));
        //  return R;		// in AUs
        //}

        static private double calcSunApparentLong(double t)
        {
            var o = calcSunTrueLong(t);
            var omega = 125.04 - 1934.136 * t;
            var lambda = o - 0.00569 - 0.00478 * Math.Sin(degToRad(omega));
            return lambda;		// in degrees
        }

        static private double calcMeanObliquityOfEcliptic(double t)
        {
            var seconds = 21.448 - t * (46.8150 + t * (0.00059 - t * (0.001813)));
            var e0 = 23.0 + (26.0 + (seconds / 60.0)) / 60.0;
            return e0;		// in degrees
        }

        static private double calcObliquityCorrection(double t)
        {
            var e0 = calcMeanObliquityOfEcliptic(t);
            var omega = 125.04 - 1934.136 * t;
            var e = e0 + 0.00256 * Math.Cos(degToRad(omega));
            return e;		// in degrees
        }

        //function calcSunRtAscension(t)
        //{
        //  var e = calcObliquityCorrection(t);
        //  var lambda = calcSunApparentLong(t);
        //  var tananum = (Math.Cos(degToRad(e)) * Math.Sin(degToRad(lambda)));
        //  var tanadenom = (Math.Cos(degToRad(lambda)));
        //  var alpha = radToDeg(Math.atan2(tananum, tanadenom));
        //  return alpha;		// in degrees
        //}

        static private double calcSunDeclination(double t)
        {
            var e = calcObliquityCorrection(t);
            var lambda = calcSunApparentLong(t);

            var sint = Math.Sin(degToRad(e)) * Math.Sin(degToRad(lambda));
            var theta = radToDeg(Math.Asin(sint));
            return theta;		// in degrees
        }

        static private double calcEquationOfTime(double t)
        {
            var epsilon = calcObliquityCorrection(t);
            var l0 = calcGeomMeanLongSun(t);
            var e = calcEccentricityEarthOrbit(t);
            var m = calcGeomMeanAnomalySun(t);

            var y = Math.Tan(degToRad(epsilon) / 2.0);
            y *= y;

            var sin2l0 = Math.Sin(2.0 * degToRad(l0));
            var sinm = Math.Sin(degToRad(m));
            var cos2l0 = Math.Cos(2.0 * degToRad(l0));
            var sin4l0 = Math.Sin(4.0 * degToRad(l0));
            var sin2m = Math.Sin(2.0 * degToRad(m));

            var Etime = y * sin2l0 - 2.0 * e * sinm + 4.0 * e * y * sinm * cos2l0 - 0.5 * y * y * sin4l0 - 1.25 * e * e * sin2m;
            return radToDeg(Etime) * 4.0;	// in minutes of time
        }

        static private double calcHourAngleSunrise(double lat, double solarDec)
        {
            var latRad = degToRad(lat);
            var sdRad = degToRad(solarDec);
            var HAarg = (Math.Cos(degToRad(90.833)) / (Math.Cos(latRad) * Math.Cos(sdRad)) - Math.Tan(latRad) * Math.Tan(sdRad));
            var HA = Math.Acos(HAarg);
            return HA;		// in radians (for sunset, use -HA)
        }

        //function isNumber(inputVal) 
        //{
        //  var oneDecimal = false;
        //  var inputStr = "" + inputVal;
        //  for (var i = 0; i < inputStr.length; i++) 
        //  {
        //    var oneChar = inputStr.charAt(i);
        //    if (i == 0 && (oneChar == "-" || oneChar == "+"))
        //    {
        //      continue;
        //    }
        //    if (oneChar == "." && !oneDecimal) 
        //    {
        //      oneDecimal = true;
        //      continue;
        //    }
        //    if (oneChar < "0" || oneChar > "9")
        //    {
        //      return false;
        //    }
        //  }
        //  return true;
        //}


        static private string zeroPad(double n, int digits)
        {
            var n_str = n.ToString();
            while (n_str.Length < digits)
            {
                n_str = '0' + n_str;
            }
            return n_str;
        }

        //function readTextBox(inputId, numchars, intgr, pad, min, max, def)
        //{
        //  var number = document.getElementById(inputId).value.substring(0,numchars)
        //  if (intgr) {
        //    number = Math.Floor(parseFloat(number))
        //  } else {  // float
        //    number = parseFloat(number)
        //  }
        //  if (number < min) {
        //    number = min
        //  } else if (number > max) {
        //    number = max
        //  } else if (number.toString() == "NaN") {
        //    number = def
        //  }
        //  if ((pad) && (intgr)) {
        //    document.getElementById(inputId).value = zeroPad(number,2)
        //  } else {
        //    document.getElementById(inputId).value = number
        //  }
        //  return number
        //}

        //function month(name, numdays, abbr) 
        //{
        //  this.name = name;
        //  this.numdays = numdays;
        //  this.abbr = abbr;
        //}
        //var monthList = new Array();	
        //var i = 0;
        //monthList[i++] = new month("January", 31, "Jan");
        //monthList[i++] = new month("February", 28, "Feb");
        //monthList[i++] = new month("March", 31, "Mar");
        //monthList[i++] = new month("April", 30, "Apr");
        //monthList[i++] = new month("May", 31, "May");
        //monthList[i++] = new month("June", 30, "Jun");
        //monthList[i++] = new month("July", 31, "Jul");
        //monthList[i++] = new month("August", 31, "Aug");
        //monthList[i++] = new month("September", 30, "Sep");
        //monthList[i++] = new month("October", 31, "Oct");
        //monthList[i++] = new month("November", 30, "Nov");
        //monthList[i++] = new month("December", 31, "Dec");


        static private double getJD()
        {
            var docmonth = (double)DateTime.Now.Month;
            var docday = (double)DateTime.Now.Day;
            var docyear = (double)DateTime.Now.Year;
            // RICK MODIFIED No need to validate since no user input is being used
            //if ( (isLeapYear(docyear)) && (docmonth == 2) ) {
            //  if (docday > 29) {
            //    docday = 29;
            //    document.getElementById("daybox").selectedIndex = docday - 1
            //  } 
            //} else {
            //  if (docday > monthList[docmonth-1].numdays) {
            //    docday = monthList[docmonth-1].numdays
            //    document.getElementById("daybox").selectedIndex = docday - 1
            //  }
            //}
            if (docmonth <= 2)
            {
                docyear -= 1;
                docmonth += 12;
            }
            var A = Math.Floor(docyear / 100);
            var B = 2 - A + Math.Floor(A / 4);
            var JD = Math.Floor(365.25 * (docyear + 4716)) + Math.Floor(30.6001 * (docmonth + 1)) + docday + B - 1524.5;
            return JD;
        }

        static private double getTimeLocal()
        {
            var dochr = (double)DateTime.Now.Hour;
            var docmn = (double)DateTime.Now.Minute;
            var docsc = (double)DateTime.Now.Second;
            //var docpm = document.getElementById("pmbox").checked
            var docdst = true; // TODO TimeZoneInfo.Local.IsDaylightSavingTime(DateTime.Now);
            //if ( (docpm) && (dochr < 12) ) {
            //  dochr += 12
            //}
            if (docdst)
            {
                dochr -= 1;
            }
            var mins = dochr * 60 + docmn + docsc / 60.0;
            return mins;
        }

        //function calcAzEl(output, T, localtime, latitude, longitude, zone)
        //{
        //  var eqTime = calcEquationOfTime(T)
        //  var theta  = calcSunDeclination(T)
        //  if (output) {
        //    document.getElementById("eqtbox").value = Math.Floor(eqTime*100 +0.5)/100.0
        //    document.getElementById("sdbox").value = Math.Floor(theta*100+0.5)/100.0
        //  }
        //  var solarTimeFix = eqTime + 4.0 * longitude - 60.0 * zone
        //  var earthRadVec = calcSunRadVector(T)
        //  var trueSolarTime = localtime + solarTimeFix
        //  while (trueSolarTime > 1440)
        //  {
        //    trueSolarTime -= 1440
        //  }
        //  var hourAngle = trueSolarTime / 4.0 - 180.0;
        //  if (hourAngle < -180) 
        //  {
        //    hourAngle += 360.0
        //  }
        //  var haRad = degToRad(hourAngle)
        //  var csz = Math.Sin(degToRad(latitude)) * Math.Sin(degToRad(theta)) + Math.Cos(degToRad(latitude)) * Math.Cos(degToRad(theta)) * Math.Cos(haRad)
        //  if (csz > 1.0) 
        //  {
        //    csz = 1.0
        //  } else if (csz < -1.0) 
        //  { 
        //    csz = -1.0
        //  }
        //  var zenith = radToDeg(Math.Acos(csz))
        //  var azDenom = ( Math.Cos(degToRad(latitude)) * Math.Sin(degToRad(zenith)) )
        //  if (Math.abs(azDenom) > 0.001) {
        //    azRad = (( Math.Sin(degToRad(latitude)) * Math.Cos(degToRad(zenith)) ) - Math.Sin(degToRad(theta))) / azDenom
        //    if (Math.abs(azRad) > 1.0) {
        //      if (azRad < 0) {
        //    azRad = -1.0
        //      } else {
        //    azRad = 1.0
        //      }
        //    }
        //    var azimuth = 180.0 - radToDeg(Math.Acos(azRad))
        //    if (hourAngle > 0.0) {
        //      azimuth = -azimuth
        //    }
        //  } else {
        //    if (latitude > 0.0) {
        //      azimuth = 180.0
        //    } else { 
        //      azimuth = 0.0
        //    }
        //  }
        //  if (azimuth < 0.0) {
        //    azimuth += 360.0
        //  }
        //  var exoatmElevation = 90.0 - zenith

        //// Atmospheric Refraction correction

        //  if (exoatmElevation > 85.0) {
        //    var refractionCorrection = 0.0;
        //  } else {
        //    var te = Math.Tan (degToRad(exoatmElevation));
        //    if (exoatmElevation > 5.0) {
        //      var refractionCorrection = 58.1 / te - 0.07 / (te*te*te) + 0.000086 / (te*te*te*te*te);
        //    } else if (exoatmElevation > -0.575) {
        //      var refractionCorrection = 1735.0 + exoatmElevation * (-518.2 + exoatmElevation * (103.4 + exoatmElevation * (-12.79 + exoatmElevation * 0.711) ) );
        //    } else {
        //      var refractionCorrection = -20.774 / te;
        //    }
        //    refractionCorrection = refractionCorrection / 3600.0;
        //  }

        //  var solarZen = zenith - refractionCorrection;

        //  if ((output) && (solarZen > 108.0) ) {
        //    document.getElementById("azbox").value = "dark"
        //    document.getElementById("elbox").value = "dark"
        //  } else if (output) {
        //    document.getElementById("azbox").value = Math.Floor(azimuth*100 +0.5)/100.0
        //    document.getElementById("elbox").value = Math.Floor((90.0-solarZen)*100+0.5)/100.0
        //    if (document.getElementById("showae").checked) {
        //      showLineGeodesic("#ffff00", azimuth)
        //    }
        //  }
        //  return (azimuth)
        //}

        //function calcSolNoon(jd, longitude, timezone, dst)
        //{
        //  var tnoon = calcTimeJulianCent(jd - longitude/360.0)
        //  var eqTime = calcEquationOfTime(tnoon)
        //  var solNoonOffset = 720.0 - (longitude * 4) - eqTime // in minutes
        //  var newt = calcTimeJulianCent(jd + solNoonOffset/1440.0)
        //  eqTime = calcEquationOfTime(newt)
        //  solNoonLocal = 720 - (longitude * 4) - eqTime + (timezone*60.0)// in minutes
        //  if(dst) solNoonLocal += 60.0
        //  while (solNoonLocal < 0.0) {
        //    solNoonLocal += 1440.0;
        //  }
        //  while (solNoonLocal >= 1440.0) {
        //    solNoonLocal -= 1440.0;
        //  }
        //  document.getElementById("noonbox").value = timeString(solNoonLocal, 3)
        //}

        static private string dayString(double jd, bool next, int flag)
        {
            // returns a string in the form DDMMMYYYY[ next] to display prev/next rise/set
            // flag=2 for DD MMM, 3 for DD MM YYYY, 4 for DDMMYYYY next/prev
            var output = "";
            if ((jd < 900000) || (jd > 2817000))
            {
                output = "error";
            }
            else
            {
                var z = Math.Floor(jd + 0.5);
                var f = (jd + 0.5) - z;
                var A = 0.0;
                if (z < 2299161)
                {
                    A = z;
                }
                else
                {
                    var alpha = Math.Floor((z - 1867216.25) / 36524.25);
                    A = z + 1 + alpha - Math.Floor(alpha / 4);
                }
                var B = A + 1524;
                var C = Math.Floor((B - 122.1) / 365.25);
                var D = Math.Floor(365.25 * C);
                var E = Math.Floor((B - D) / 30.6001);
                var day = B - D - Math.Floor(30.6001 * E) + f;
                var month = (E < 14) ? E - 1 : E - 13;
                var year = ((month > 2) ? C - 4716 : C - 4715);
                if (flag == 2)
                    output = zeroPad(day, 2) + " " + DateTimeFormatInfo.GetInstance(null).GetAbbreviatedMonthName((int)month);
                if (flag == 3)
                    output = zeroPad(day, 2) + DateTimeFormatInfo.GetInstance(null).GetAbbreviatedMonthName((int)month) + year.ToString();
                if (flag == 4)
                    output = zeroPad(day, 2) + DateTimeFormatInfo.GetInstance(null).GetAbbreviatedMonthName((int)month) + year.ToString() + ((next) ? " next" : " prev");
            }
            return output;
        }

        static private string timeDateString(double JD, double minutes)
        {
            var output = timeString(minutes, 2) + " " + dayString(JD, false, 2);
            return output;
        }

        static private string timeString(double minutes, int flag)
        // timeString returns a zero-padded string (HH:MM:SS) given time in minutes
        // flag=2 for HH:MM, 3 for HH:MM:SS
        {
            var output = "";
            if ((minutes >= 0) && (minutes < 1440))
            {
                var floatHour = minutes / 60.0;
                var hour = Math.Floor(floatHour);
                var floatMinute = 60.0 * (floatHour - Math.Floor(floatHour));
                var minute = Math.Floor(floatMinute);
                var floatSec = 60.0 * (floatMinute - Math.Floor(floatMinute));
                var second = Math.Floor(floatSec + 0.5);
                if (second > 59)
                {
                    second = 0;
                    minute += 1;
                }
                if ((flag == 2) && (second >= 30)) minute++;
                if (minute > 59)
                {
                    minute = 0;
                    hour += 1;
                }
                output = zeroPad(hour, 2) + ":" + zeroPad(minute, 2);
                if (flag > 2) output = output + ":" + zeroPad(second, 2);
            }
            else
            {
                output = "error";
            }
            return output;
        }

        static private double calcSunriseSetUTC(bool rise, double JD, double latitude, double longitude)
        {
            var t = calcTimeJulianCent(JD);
            var eqTime = calcEquationOfTime(t);
            var solarDec = calcSunDeclination(t);
            var hourAngle = calcHourAngleSunrise(latitude, solarDec);
            //alert("HA = " + radToDeg(hourAngle));
            if (!rise) hourAngle = -hourAngle;
            var delta = longitude + radToDeg(hourAngle);
            var timeUTC = 720 - (4.0 * delta) - eqTime;	// in minutes
            return timeUTC;
        }

        static private double calcSunriseSet(bool rise, double JD, double latitude, double longitude, double timezone, bool dst)
        // rise = 1 for sunrise, 0 for sunset
        {
            var id = ((rise) ? "risebox" : "setbox");
            var timeUTC = calcSunriseSetUTC(rise, JD, latitude, longitude);
            var newTimeUTC = calcSunriseSetUTC(rise, JD + timeUTC / 1440.0, latitude, longitude);
            //if (isNumber(newTimeUTC)) {
            var timeLocal = newTimeUTC + (timezone * 60.0);
            //if (document.getElementById(rise ? "showsr" : "showss").checked) {
            //  var riseT = calcTimeJulianCent(JD + newTimeUTC/1440.0);
            //  var riseAz = calcAzEl(0, riseT, timeLocal, latitude, longitude, timezone);
            //  showLineGeodesic(rise ? "#66ff00" : "#ff0000", riseAz);
            //}
            timeLocal += ((dst) ? 60.0 : 0.0);
            if ((timeLocal >= 0.0) && (timeLocal < 1440.0))
            {
                //document.getElementById(id).value = timeString(timeLocal,2);
                return timeLocal;
            }
            //else
            //{
            //    var jday = JD;
            //    var increment = ((timeLocal < 0) ? 1 : -1);
            //    while ((timeLocal < 0.0) || (timeLocal >= 1440.0))
            //    {
            //        timeLocal += increment * 1440.0;
            //        jday -= increment;
            //    }
            //    document.getElementById(id).value = timeDateString(jday,timeLocal);
            //}
            //} else { // no sunrise/set found
            //  var doy = calcDoyFromJD(JD)
            //  if ( ((latitude > 66.4) && (doy > 79) && (doy < 267)) ||
            //  ((latitude < -66.4) && ((doy < 83) || (doy > 263))) )
            //  {   //previous sunrise/next sunset
            //    if (rise) { // find previous sunrise
            //      jdy = calcJDofNextPrevRiseSet(0, rise, JD, latitude, longitude, timezone, dst)
            //    } else { // find next sunset
            //      jdy = calcJDofNextPrevRiseSet(1, rise, JD, latitude, longitude, timezone, dst)
            //    }
            //    document.getElementById(((rise)? "risebox":"setbox")).value = dayString(jdy,0,3)
            //  } else {   //previous sunset/next sunrise
            //    if (rise == 1) { // find previous sunrise
            //      jdy = calcJDofNextPrevRiseSet(1, rise, JD, latitude, longitude, timezone, dst)
            //    } else { // find next sunset
            //      jdy = calcJDofNextPrevRiseSet(0, rise, JD, latitude, longitude, timezone, dst)
            //    }
            //    document.getElementById(((rise)? "risebox":"setbox")).value = dayString(jdy,0,3)
            //  }
            //}
            return 0.0;
        }

        //function calcJDofNextPrevRiseSet(next, rise, JD, latitude, longitude, tz, dst)
        //{
        //  var julianday = JD;
        //  var increment = ((next) ? 1.0 : -1.0);

        //  var time = calcSunriseSetUTC(rise, julianday, latitude, longitude);
        //  while(!isNumber(time)){
        //    julianday += increment;
        //    time = calcSunriseSetUTC(rise, julianday, latitude, longitude);
        //  }
        //  var timeLocal = time + tz * 60.0 + ((dst) ? 60.0 : 0.0)
        //  while ((timeLocal < 0.0) || (timeLocal >= 1440.0))
        //  {
        //    var incr = ((timeLocal < 0) ? 1 : -1)
        //    timeLocal += (incr * 1440.0)
        //    julianday -= incr
        //  }
        //  return julianday;
        //}

        //function calculate() {
        //  //refreshMap()
        //  //clearOutputs()
        //  //map.clearOverlays()
        //  //showMarkers()
        //  var jday = getJD()
        //  var tl = getTimeLocal()
        //  var tz = readTextBox("zonebox", 5, 0, 0, -14, 13, 0)
        //  var dst = document.getElementById("dstCheckbox").checked
        //  var total = jday + tl/1440.0 - tz/24.0
        //  var T = calcTimeJulianCent(total)
        //  var lat = parseFloat(document.getElementById("latbox").value.substring(0,9))
        //  var lng = parseFloat(document.getElementById("lngbox").value.substring(0,10))
        //  calcAzEl(1, T, tl, lat, lng, tz)
        //  calcSolNoon(jday, lng, tz, dst)
        //  var rise = calcSunriseSet(1, jday, lat, lng, tz, dst)
        //  var set  = calcSunriseSet(0, jday, lat, lng, tz, dst)
        //  //alert("JD " + jday + "  " + rise + "  " + set + "  ")
        //}

        /// <summary>
        /// Determine whether the sun is up (has risen but not set) at the given latitude and longitude.
        /// You'll have to manually specify the timezone offset (and optionally indicate whether the location
        /// is currently observing daylight-savings time if your offset hasn't already accounted for this)
        /// </summary>
        /// <param name="lat">Latitude (positive for north, negative for south)</param>
        /// <param name="lng">Longitude (positive for west, negative for east)</param>
        /// <param name="tz">The timezone</param>
        /// <param name="dst">Daylight savings time?</param>
        /// <returns>true if the sun is up at the given location, false otherwise</returns>
        static public bool IsSunUp(double lat, double lng, double tz, bool dst)
        {
            var jday = getJD();
            var rise = calcSunriseSet(true, jday, lat, lng, tz, dst);
            var set = calcSunriseSet(false, jday, lat, lng, tz, dst);
            var localtime = ((double)DateTime.Now.ToUniversalTime().AddHours(tz).Hour * 60) + (double)DateTime.Now.ToUniversalTime().AddHours(tz).Minute + ((double)DateTime.Now.ToUniversalTime().AddHours(tz).Second / 60);
            return ((rise < localtime) && (localtime < set));
        }
    }
}
