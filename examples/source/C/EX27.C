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

/*ex27.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

long function( char *string, int len )
{
   /*convert the field data into an 'int' */
   return c4atol( string, len) ;
}

void main()
{
   long retLong ;
   char string[] = "35476cat5" ;

   /* 'long result' will be "35" since it only converts the 2 characters as specified by the second parameter. */
   retLong = function( string, 2 ) ;

   printf("Return from c4atol() = %i\n\n", retLong) ;
   retLong = 0 ;

   retLong = function( string, strlen(string) ) ;
   printf("Return from c4atol() = %i\n", retLong) ;

   return ;
}
