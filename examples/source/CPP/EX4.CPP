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

   // Do not add duplicate records to unique tags or the data file and 
   // return r4unique when attempted.
   cb.errDefaultUnique = r4unique ; 

   Data4 data( cb, "INFO" ) ;

   data.top( ) ;
   data.appendStart( ) ;     
   int rc = data.append( ) ; // append a duplicate copy of the top record
   
   if( rc == r4unique ) 
      cout << "Attempt to add a duplicate record failed." << endl ;
   else
   {
      cout << "Attempt to add a duplicate record succeeded" << endl ;
      cout << "Record in both data and index file" << endl ;
   }

   data.close( ) ;
   cb.initUndo( ) ;
}
