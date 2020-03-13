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

//ex131.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

typedef struct myStructSt
{
   char id[6] ;
   int checkSum ;
   char password[15] ;
} MY_STRUCT ;

int getNextStructure( File4seqRead &seqFile, MY_STRUCT *ms )
{
   if( seqFile.read( ms, sizeof( MY_STRUCT ) ) == sizeof( MY_STRUCT ) )
     if((int) Str4ptr(ms->id) == ms->checkSum )
        return 0 ;

   memset( ms, '\0', sizeof(MY_STRUCT ) ) ;

   return 1 ;
}

void main( )
{
   Code4 cb ;
   File4 file( cb, "PASS.FIL" ) ;
   char buf[0x1400] ;

   memset( buf, 0, sizeof(buf) ) ;

   File4seqRead readFile( file, 0L, buf, sizeof(buf)-1 ) ;

   MY_STRUCT info ;
   getNextStructure( readFile, &info ) ;

   cb.initUndo( ) ;
}
