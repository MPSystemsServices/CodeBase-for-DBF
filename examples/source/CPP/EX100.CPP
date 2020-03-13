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

//ex100.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 settings ;
   Data4 info( settings, "INFO" ) ;
   Data4 data( settings, "DATA" ) ;
   settings.exitTest( ) ;

   Field4 infoName( info, "NAME" ) ;
   Field4 dataLname( data, "LNAME" ) ;

   info.lockAddAll( ) ;
   data.lockAddAll( ) ;
   settings.lock( ) ;
   for( info.top( ) , data.top( ) ; !info.eof( ) && !data.eof( ) ;
        info.skip( ) ,data.skip( ) )
      dataLname.assignField( infoName ) ; // copy 'LNAME' into 'NAME'

   settings.closeAll( ) ;
   settings.initUndo( ) ;
}
