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

/***********************************************************************\
*                                                                       *
*   DELETION.CPP     Copyright (C) 1999 Sequiter Software Inc.          *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 4 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; // for all Borland compilers
#endif

FIELD4INFO  fieldInfo[]=
{
    {"DUMMY",'C',10,0},
    {"MEMO",'M',10,0},
    {0,0,0,0},
} ;

void printDeleteStatus(int status,long recNo)
{
    if(status)
        cout << "Record " << recNo << " - DELETED" << endl ;
    else
        cout << "Record " << recNo << " - NOT DELETED" << endl ;
}

void printRecords(Data4 dataFile)
{
    int  rc = 0, status;
    long recNo;

    cout << endl ;

    dataFile.top( ) ;
    while( rc != r4eof )
    {
        recNo = dataFile.recNo( ) ;
        status = dataFile.deleted( ) ;
        printDeleteStatus( status, recNo ) ;
        rc = dataFile.skip( ) ;
    }
}

void main( void )
{
    Code4       codeBase ;
    Data4       dataFile ;
    int count;

    codeBase.safety = 0 ;
    codeBase.errCreate = 0 ;
    codeBase.lockEnforce = 1 ;

    dataFile.create( codeBase, "TUTOR5", fieldInfo, 0 ) ;
    codeBase.exitTest( ) ;

    for(count = 0; count < 5; count ++)
        dataFile.appendBlank( ) ;

    printRecords( dataFile ) ;

    dataFile.lockAll( ) ;
    dataFile.go( 3L ) ;
    dataFile.deleteRec( ) ;
    dataFile.go( 1L ) ;
    dataFile.deleteRec( ) ;
    printRecords( dataFile ) ;

    dataFile.go( 3L ) ;
    dataFile.recall( ) ;
    printRecords( dataFile ) ;

    dataFile.pack( ) ;
    dataFile.memoCompress( ) ;
    printRecords( dataFile ) ;

    codeBase.closeAll( ) ;
    codeBase.initUndo( ) ;
}

