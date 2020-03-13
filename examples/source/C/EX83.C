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

/*ex83.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

static int daysInMonth[] = { 0,31,28,31,30,31,30,31,31,30,31,30,31 } ;

void main( )
{
   int endOfMonth  ;
   char today[8] ;

   date4today( today ) ;
   endOfMonth  = daysInMonth[ date4month( today ) ] ;
   if( date4month( today ) == 2 && date4isLeap( today ) )
      endOfMonth++ ;
   printf("there are %d days left till the end of the month\n",
                           endOfMonth - date4day( today )) ;
}
