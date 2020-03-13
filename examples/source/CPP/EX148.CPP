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

void main( )
{
   Code4 cb ;
   Sort4 sort( cb, sizeof(char), sizeof(char)*4 ) ;
   
   sort.put( "A", "BEN", 1) ;
   sort.put( "C", "DAN", 2) ;
   sort.put( "B", "ROB", 3) ;
   sort.put( "A", "ACE", 4) ;
   sort.getInit( ) ;

  /* Displays:
     A BEN 1
     A ACE 4
     B ROB 3
     C DAN 2   
 */ 
   while( sort.get( ) == 0 )
   {
      cout << *(char *) sort.result << " " 
           << (char *) sort.resultOther  
           << " " << sort.resultRec << endl ;
   }
   sort.free( ) ;
   cb.initUndo( ) ;
}
