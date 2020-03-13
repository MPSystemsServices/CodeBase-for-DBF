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

//ex74.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Date4 today ;
   today.today( ) ; // set today equal to the system clock date.

   cout << "Today is " << today.cdow( ) << ", "
        << today.format( "MMMMMMMM DD, CCYY") << endl ;

   Date4 tomorrow = today + 1L ;

   cout << "Tomorrow is " << tomorrow.cdow( ) << ", "
        << tomorrow.format( "MMMMMMMM DD, CCYY") << endl ;

   tomorrow -= 2L ;
   cout << "Yesterday was " << tomorrow.cdow( ) << ", "
        << tomorrow.format( "MMMMMMMM DD, CCYY") << endl ;

   if( today == tomorrow )
      cout << "This will never happen" << endl ;

   if( today > tomorrow )
      cout << "This is always true" << endl ;
     // note tomorrow was decremented by two

   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   Date4 myBirthDate( "19690225" ) ;
   Field4 birthField( data, "BIRTH_DATE" ) ;
   data.select(Tag4 (data,"INF_BRTH")) ;

   if( data.seek( myBirthDate ) == 0 )
      cout << "I'm in record " << data.recNo( ) << endl ;
   data.lockAll( ) ;
  // change all birthdate fields to my birth date.
   for( data.top( ) ; !data.eof( ) ; data.skip( ) )
      birthField.assign( myBirthDate ) ;
   cb.initUndo( ) ;
}
