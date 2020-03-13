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

//ex63.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 people( cb, "people.dbf" ) ;

   /* Assume 'PEOPLE.DBF' has a production index file with tags
      PPL_NAME, PPL_AGE, PPL_BRTH */
   people.select(  Tag4( people, "PPL_NAME" ) ) ;

   if( people.seek( "fred " ) == r4success )
      cout << "fred is in record # " << people.recNo( ) << endl ;

   if( people.seek( "HANK STEVENS" ) == r4success )
      cout << "HANK STEVENS is in record #" << people.recNo( ) << endl ;

   people.select( Tag4( people, "PPL_AGE" ) ) ;
   Field4 age( people, "AGE" ) ;

   int rc = people.seek( 0.0 ) ;

   if( rc == r4success || rc == r4after )
      cout << "The youngest age is: " << (double) age << endl ;

   // Seek using the char * version
   rc = people.seek( "0" ) ;
   if( rc == r4success || rc == r4after )
      cout << "The youngest age is: " << (double) age << endl ;

   // Assume PPL_BRTH is a Date key expression
   people.select( Tag4( people, "PPL_BRTH" )) ;
   Date4 birth( "19600415" ) ;

   if( people.seek( birth.str( ) ) == r4success ) // Char. array in CCYYMMDD format
      cout << "Found: " << birth.format( "MMM DD, CCYY" ) << endl;

   if( people.seek( (long) birth ) == r4success )
      cout << "Found: " << birth.format( "MMM DD, CCYY" ) << endl;

   people.close( ) ;
   cb.initUndo( ) ;
}
