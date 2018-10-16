/***********************************************************************\
*                                                                       *
*   RELATE1.CPP     Copyright (C) 1999 Sequiter Software Inc.           *
*                                                                       *
\***********************************************************************/
/* See User's Guide, chapter 6 */

#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

Code4 codeBase( 0 ) ;
Data4 student, enrolment ;
Relate4 slave ;
Relate4set master ;
Tag4 idTag, nameTag ;

void openDataFiles( void ) ;
void setRelation( void ) ;
void printRecord( void ) ;

void openDataFiles( )
{
   codeBase.init( ) ;

   student.open( codeBase,"student" ) ;
   enrolment.open( codeBase,"enroll");

   nameTag.init( student,"STU_NAME");
   idTag.init( enrolment,"ENR_ID");

   codeBase.exitTest( ) ;
}

void setRelation()
{
    master.init( student ) ;
    if( !master.isValid( )) exit(1);

    Relate4 slave( master, enrolment, "ID", idTag) ;

    slave.type( relate4scan ) ;
    master.top( ) ;
    // 'slave' falls out of scope, but the relation remains
}

void printRecord()
{
    Relate4iterator relation( master) ;
    Data4   data ;
    Field4memo  field;

    for( relation;  relation.isValid( )
                          ; relation.next( ) )
    {
        data = relation.data( ) ;

        for(int j = 1; j <= data.numFields( ); j++ )
        {
           field.init( data, j ) ;
           cout << field.str( ) << " " ;
        }
    }
    cout << endl ;
}

void listRecords( void )
{
    int rc;

    for(rc = master.top( ); rc != r4eof; rc = master.skip( ) )
        printRecord( ) ;

    cout << endl ;

    codeBase.unlock( ) ;
}

void main( void )
{
    openDataFiles( ) ;

    setRelation( ) ;

    listRecords( ) ;

    master.free( ) ;

    codeBase.closeAll( ) ;
    codeBase.initUndo( ) ;
}
