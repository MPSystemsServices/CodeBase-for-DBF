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
