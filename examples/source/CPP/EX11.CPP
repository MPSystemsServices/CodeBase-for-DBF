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

//ex11.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

int itemsToRestock( Code4 &cb )
{

   int oldOpt = cb.optimize ; // save old optimization setting.
   int oldExcl = cb.accessMode ;
   cb.optimize = OPT4EXCLUSIVE ;
   cb.accessMode = OPEN4DENY_RW ;//open files exclusively

   Data4 inventory( cb, "INVENT.DBF" ) ; // Read optimized
   Field4 minOnHand( inventory, "MIN_ON_HND" ) ;
   Field4 onHand( inventory, "ON_HAND" ) ;
   Field4 stockName( inventory, "ITEM" )  ;
   int count = 0 ;

   if( cb.errorCode >= 0 )
   {
      cb.optStart( ) ;

      for( inventory.top( ) ; ! inventory.eof( ) ; inventory.skip( ) )
         if( (long) onHand < (long) minOnHand )
           count++ ;
      cout << count << " items must be restocked" << endl ;
   }
   cb.optSuspend( ) ;
   cb.optimize = oldOpt ;
   cb.accessMode = oldExcl ;
   inventory.close( ) ;
   return count ;
}

void main()
{
   Code4 codebase ;
   itemsToRestock( codebase ) ;
   codebase.initUndo() ;
}
