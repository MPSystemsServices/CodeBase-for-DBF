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

//ex129.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

typedef struct myStructSt
{
   char id[6] ;
   int checkSum ;
   char password[15] ;
} MY_STRUCT ;

void main( )
{
   Code4 cb ;
   File4seqWrite writePassFile ;
   File4 passFile ;
   MY_STRUCT person ;
   Str4max id( person.id, sizeof(person.id) ) ;
   Str4max pass( person.password, sizeof( person.password) ) ;
   char buffer[ 0x1400 ] ; // 5K... space for 200 structures
   int i ;

   cb.safety = 0 ;

   passFile.create( cb, "PASS.FIL" ) ;
   writePassFile.init( passFile, 0, buffer, sizeof( buffer ) ) ;

   for( i = 200 ; i ; i -- )
   {
      id.assignLong( i ) ;
      person.checkSum = i ;
      pass.assign( "PASSWORD" ) ;
      pass.add( id ) ;
      writePassFile.write( &person, sizeof( MY_STRUCT ) ) ;
   } // physically write only once.
   writePassFile.flush( ) ;

   File4seqRead readPassFile( passFile, 0, buffer, sizeof(buffer) );

   for( i = 200 ; i ; i -- )
   {
      // only one physical read occurs... the rest are in memory
      readPassFile.read( &person, sizeof( MY_STRUCT ) ) ;
      if( (int) id == person.checkSum )
         cout << "Valid Password: " << person.password << endl ;
   }
   passFile.close( ) ; // writePassFile and readPassFile are invalid now
   cb.initUndo( ) ;
}
