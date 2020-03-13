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

/* 'f4int()' uses 'c4atoi' because database field data is not null terminated */
#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

int function( char *string, int len )
{
	/*convert the field data into an 'int' */
	return c4atoi( string, len ) ;
}


void main()
{
   int retInt ;
   char string[] = "12356cat5" ;

   retInt = function( string, 5 ) ;
   printf("Return from c4atoi() = %i\n\n", retInt) ;
   retInt = 0 ;

   retInt = function( string, strlen(string) ) ;
   printf("Return from c4atoi() = %i\n", retInt) ;

   return ;
}