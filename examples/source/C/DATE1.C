/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/***********************************************************************\
*                                                                       *
*   DATE.C    Copyright (C) 1998 Sequiter Software Inc.             *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 8 */

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int validDate(char *date)
{
   long rc;
   rc = date4long(date);

   if(rc < 1)
        return 0;
   else
        return 1;
}

void howLongUntil(int month,int day,char *title)
{
   char todayStandard[9],today[25],date[9];
   const char *dow;
   int  year,days;
   long julianToday,julianDate;

   memset(todayStandard,'\0',sizeof(todayStandard));
   memset(today,'\0',sizeof(today));
   memset(date,'\0',sizeof(date));

   date4today(todayStandard);
   date4format(todayStandard,today,"MMM DD/CCYY");

   printf("Today's date is %s\n",today);

   julianToday = date4long(todayStandard);

   year = date4year(todayStandard);
   sprintf(date,"%4d%2d%2d",year,month,day);

   julianDate = date4long(date);

   if(julianDate < julianToday)
   {
      year ++;
      sprintf(date,"%4d%2d%2d",year,month,day);

      julianDate = date4long(date);
   }


   days = julianDate - julianToday;

   printf("There are %d days until %s\n",days,title);
   dow = date4cdow(date);

   printf("(which is a %s this year)\n",dow);
}


void main(void)
{
   char birthdate[80],standard[9];

   howLongUntil(12,25,"Christmas");

   do
   {
      printf("Please enter your birthdate in \"DEC 20/1993\" format.\n");
      gets(birthdate);
      date4init(standard,birthdate,"MMM DD/CCYY");
   } while(!validDate(standard));

   howLongUntil(date4month(standard)
                 ,date4day(standard)
                 ,"your next birthday");
}
