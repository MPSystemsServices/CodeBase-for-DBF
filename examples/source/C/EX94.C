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

/*ex94.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4  cb ;
   DATA4  *info ;
   FIELD4 *birthDate ;
   char today[8], result[13] ;
   long ageInDays ;
   int rc ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;
   info = d4open( &cb , "INFO" ) ;
   birthDate = d4field( info, "BIRTH_DATE" ) ;
   error4exitTest( &cb ) ;

   d4go( info, 1L ) ;

   date4today( today ) ;

   ageInDays =  date4long( today ) - date4long( f4str( birthDate ) ) ;

   printf( "Age in days: %d\n", ageInDays ) ;

   /* display all current birth dates in a formatted manner */
   for(rc = d4top( info ); rc == 0; rc = d4skip( info, 1L ) )
   {
      date4format( f4str( birthDate ), result, "MMM DD, CCYY" ) ;
      printf( "%s\n", result ) ;
   }

   /* assign today's date to all birth dates in the data file*/

   for( rc = d4top( info ); rc == 0; rc = d4skip( info, 1L ) )
   {
      f4assign( birthDate, today ) ;
   }

   code4initUndo( &cb );
}
