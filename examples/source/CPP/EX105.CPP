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
extern unsigned _stklen = 10000 ;

static FIELD4INFO statFields[] = 
{
   { "NAME", 'C', 20, 0 },
   { "AGE",  'N',  3, 0 },
   { "BDATE", 'D', 8, 0 },
   { 0,0,0,0 } 
} ;

void main( )
{
   Code4 cb ;
   Data4 oneDbf, twoDbf ;
   Field4info fields( cb ) ;
   cb.safety = 0 ;

   fields.add( "NAME",  'C', 20 ) ;
   fields.add( "AGE",   'N', 3 ) ;
   fields.add( "BDATE", 'D' ) ;
   
   // these two lines create identical datafiles
   oneDbf.create( cb, "EX105_1", fields.fields( ) ) ;
   twoDbf.create( cb, "EX105_2", statFields ) ;
   
   if( ! cb.errorCode )
      cout << "Created successfully" << endl ;
   else
      cout << "An error occurred" << endl ;    
      
   fields.free( ) ;
   cb.initUndo( ) ;
}
