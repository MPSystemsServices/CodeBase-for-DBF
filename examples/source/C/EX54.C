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

#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *ageField ;
   long age ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;

   d4optimizeWrite( data, OPT4ALL ) ;
   /* when doing write optimization on shared files, it is necessary to
         lock the file, preferably with d4lockAll( ) */
   d4lockAll( data ) ;

   code4optStart( &cb ) ; /* begin optimization */

   age = 20 ;
   ageField = d4field( data, "AGE") ;

   /* append a copies of the first record, assigning the age field's
       value from 20 to 65*/
   for( d4top( data ) ;  age < 65 ; d4append( data ) )
   {
      d4appendStart( data, 0 ) ;
      f4assignLong( ageField, age++ ) ;
   }

   code4initUndo( &cb ) ; /* flushes, closes, and unlocks */
}
