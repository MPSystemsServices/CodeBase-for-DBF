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

// ex65.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ; // automatically open data & index file.
   Tag4 nameTag( data, "INF_NAME" ), firstTag ;
   firstTag.initFirst(data) ;

   data.top();
   data.recall();

   data.select( nameTag ) ; // Select the 'INF_NAME'
   data.seek( "JONES" ) ;    // Seek using 'INF_NAME'

   data.select( firstTag ) ; // Select the 'AGE' tag which is the
                             // first tag of the first open index
   data.seek( 32 ) ;       // Seek using selected tag 'AGE'

   data.select( ) ;        // Select record ordering
   data.seek( "ginger" ) ; // The seek uses the first tag of the first index
                           // when no tag is selected, so the seek fails even
                           // if "ginger" is in the data file

   data.top( ) ;          //Physical top of the data file

   cb.initUndo( ) ;   // close all files and free up Code4 memory
}
