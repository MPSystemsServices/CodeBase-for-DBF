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

//ex67.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // Borland only

void main( void )
{
   Code4 cb ;
   Data4 data( cb, "DBF" ) ;
   Tag4 tag( data, "DBF_NAME" ) ; // A tag with '.NOT.DELETED()' filter

   data.lockAll( ) ;
   data.select( tag ) ;
   data.top( ) ; // Position to the first record that is not deleted.
   cout << "first record that is not deleted is " << data.recNo() << endl ;

   data.deleteRec( ) ; // The current record no longer is located in the tag

   data.tagSync( ) ; // The change to the record is flushed, and the datafile is
                     // positioned on a valid record.

   data.top( ) ;
   cout << "the new first record that is not deleted is " << data.recNo() << endl ;
   data.skip( ) ;

   //... some other code ...

   cb.initUndo( ) ;
}
