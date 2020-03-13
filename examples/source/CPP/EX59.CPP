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

//ex59.cpp
// Copy records from one database to another.
#include "d4all.hpp"

extern unsigned _stklen = 10000;

void main()
{
   Code4  cb ;
   Data4 from( cb, "DATA1" ) ;
   cb.optStart( ) ;

   Data4 to(cb, "DATA2" ) ;

   /* Database 'DATA2' has an identical database
      structure as database 'DATA1'. */

   if ( from.recWidth( ) != to.recWidth( ) )
   {
      cb.error( e4result, 0,  "Structures not identical" ) ;
      cb.exit( ) ;
   }

   cb.exitTest(  ) ;
   for ( long i_rec = 1L; i_rec <= from.recCount( ); i_rec++ )
   {
      /* Read the database record. */
      from.go( i_rec ) ;

      /* Copy the database buffer. */
      to.appendStart( 0 ) ;
      memcpy( to.record( ), from.record( ), to.recWidth( ) ) ;

      /* Append the database record. */
      to.append( ) ;
   }
   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
