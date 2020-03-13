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
*   RELATE2.CPP     Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Manual, chapter 6 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

Code4 codeBase ;
Data4 student ;

void openDataFiles()
{
    student.open( codeBase, "student" ) ;

    codeBase.exitTest( ) ;
}

void printRecord(Data4 dataFile )
{
    for( int j=1; j <= dataFile.numFields( ); j++ )
       cout << Field4memo( dataFile, j ).str( ) << " " ;

    cout << endl ;
}

void query(Data4 dataFile
          , char *expr
          , char *order = NULL )
{
    Relate4set relation( dataFile ) ;
    if( ! relation.isValid( ) ) codeBase.exit( ) ;

    relation.querySet( expr ) ;
    relation.sortSet( order ) ;

    for( int rc = relation.top( ); rc != r4eof; rc = relation.skip( ) )
        printRecord( dataFile ) ;

    cout << endl ;

    codeBase.unlock( ) ;
    relation.free( ) ;
}

void main( void )
{
    openDataFiles( ) ;

    query( student, "AGE > 30" ) ;

    query( student, "UPPER(L_NAME) = 'MILLER'", "L_NAME + F_NAME" ) ;

    codeBase.closeAll( ) ;
    codeBase.initUndo( ) ;
}
