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

/*ex64.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *people ;
   FIELD4 *age, *birth ;
   int rc ;
   char result[12] ;

   code4init( &cb ) ;
   people = d4open( &cb, "people.dbf" ) ;

   /* Assume 'PEOPLE.DBF' has a production index file with tags
      PPL_NAME, PPL_AGE, PPL_BRTH */
   d4tagSelect( people, d4tag( people, "PPL_NAME" ) ) ;

   if( d4seek( people,  "fred" ) == r4success )
       printf("fred is in record # %d\n",d4recNo( people )) ;

   if( d4seek( people, "HANK STEVENS" ) == r4success )
       printf( "HANK STEVENS is in record # %d\n",d4recNo( people )) ;

   d4tagSelect( people, d4tag( people, "PPL_AGE" ) ) ;
   age = d4field( people, "AGE" ) ;

   rc = d4seekDouble( people, 0.0 ) ;

   if( rc == r4success || rc == r4after )
       printf( "The youngest age is: %d\n", f4int( age )) ;

    /* Seek using the char * version */
   rc = d4seek( people, "0" ) ;

   if( rc == r4success || rc == r4after )
       printf( "The youngest age is: %d\n", f4int( age )) ;

   /* Assume PPL_BRTH is a Date key expression */
   d4tagSelect( people, d4tag( people, "PPL_BRTH" )) ;
   birth = d4field( people, "BIRTH_DATE" ) ;
   date4format( "19600415", result, "MMM DD, CCYY" );

   if( d4seek( people, "19600415") == r4success)
                     /* Char. array in CCYYMMDD format*/
      printf( "Found: %s\n", result ) ;

   if( d4seekDouble( people, date4long( "19600415" )) == r4success )
      printf( "Found: %s\n", result ) ;

   d4close( people ) ;
   code4initUndo( &cb ) ;
}
