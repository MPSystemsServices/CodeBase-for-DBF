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

//ex30.cpp
#include "d4all.hpp"

#ifdef __TURBOC__  // for all Borland compilers
   extern unsigned _stklen = 10000 ;
#endif

Code4 cb ; // Code4 may be constructed globally.
Data4 data( cb, "INFO" ) ;

void main()
{
   data.lockAll( ) ;
   cb.optStart( ) ;
   data.appendBlank( ) ;

   // Append a copy of record two.  (Assume record two exists.)
   data.go( 2 ) ;
   data.appendStart( ) ;  // use_memo_entries defaults to zero
   data.append( ) ;

   // Append a copy of record 2 including existing memo entries.
   data.go( 2 ) ;
   data.appendStart( 1 ) ; // a true parameter means use memo entries
   data.append( ) ;

   // Set the record buffer to blank, change a field's value, and append
   // the resulting record.
   data.appendStart( ) ;
   data.blank( ) ;
   Field4 field( data, "NAME" ) ;
   field.assign( "New field value" ) ;
   data.append( ) ;

   // close all open files and release any allocated memory
   cb.initUndo( ) ;
}
