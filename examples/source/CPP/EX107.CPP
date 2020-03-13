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

#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Field4info fields( cb ) ; // initialize with no fields
   
   fields.add( "NAME", 'C', 20 ) ;
   fields.add( "AGE",  'N',  3 ) ;
   fields.add( "REGISTER", 'L' ) ;
   fields.add( "birth",'D' ) ;
   
   // change your mind and delete them all
   if( fields.del( "nome   " ) < 0 )
      cout << "This will always happen" << endl ;
   fields.del( "name" ) ;
   fields.del( 0 ) ; // delete the 'AGE' field
   fields.del( 0 ) ; // delete the 'REGISTER' field
   
   fields.free( ) ; // delete all remaining
   
   cb.initUndo( ) ;
}
