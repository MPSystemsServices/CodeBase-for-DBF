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

/* ex27.cpp */
#include "d4all.hpp"
#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; // for all Borland compilers
#endif

void main()
{
   Code4 settings ;

   Data4 info( settings, "DATA2.DBF") ;

   settings.optStart() ;

   Field4 field( info, "NAME") ;
   info.lockAll( ) ; //the record must be locked before a field can be changed
   for( long iRec = 1L ; iRec <= info.recCount() ; iRec++ )
   {
      info.go( iRec ) ;
      field.assign( "New Data" ) ;
   }

   info.close( ) ;
   settings.initUndo( ) ;
   settings.exit( ) ;
}
