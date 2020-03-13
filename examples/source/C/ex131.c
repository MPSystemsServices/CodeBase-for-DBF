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

/*ex131.c*/
#include <d4all.h>

int reindexDone;

short __stdcall cback(double p)
{
   printf("%f\n",p);
   if (p >= 1.0)
      reindexDone = 1;
   return 0;
}

int main(void)
{
   CODE4 cb;
   DATA4 *data;
   unsigned short actionCode;

   code4init(&cb);
   data = d4open(&cb,"student");

   reindexDone = 0;
   d4reindexWithProgress(data,cback,200);
   while (!reindexDone)
   {
      Sleep(0);
   }

   Sleep(210);  /* Make sure example does not exit before final callback. */
   return code4initUndo(&cb);
}