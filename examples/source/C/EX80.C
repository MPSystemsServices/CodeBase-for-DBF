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

/*ex80.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   char dt[8] = "19901002" ;
   char  result[20] ;

   date4format( dt, result, "YY.MM.DD" ) ;   /* 'result' will contain "90.10.02"*/
   printf("%s\n", result ) ;
   date4format( dt, result, "CCYY.MM.DD" ) ; /* 'result' will contain "1990.10.02"*/
   printf("%s\n", result ) ;
   date4format( dt, result, "MM/DD/YY" ) ;   /* 'result' will contain "10/02/90"*/
   printf("%s\n", result ) ;
   date4format( dt, result, "MMM DD/CCYY" ) ;
   printf("%s\n", result) ;  /* outputs "Oct 02/1990"*/
}
