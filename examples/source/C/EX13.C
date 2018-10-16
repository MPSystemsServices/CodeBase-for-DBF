/*ex13.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 cb ;
DATA4 *d ;

int retry( void )
{
    char rc ;
    printf("Record locked by another user.\n") ;
    printf("Retry? (Y or N)\n") ;
    rc = getchar( ) ;
    if( rc == 'N' ) return 0 ;
    return 1 ;
}

void modifyRecordValues( void )
{
   int rc ;
   char buf[8+2] ;
   FIELD4 *field ;

    cb.readLock = 1 ;
    cb.lockAttempts = 3 ;

    field = d4field( d, "DATE") ;

    while( ((rc = d4top( d )) == r4locked) && (retry( ) == 1) ) ;
    if( rc == r4locked ) return ;

    while( !d4eof( d ) )
    {
       printf("\nEnter new record value: \n") ;
       fgets(buf, sizeof(buf), stdin) ;
       f4assign( field, buf ) ;

       while( ((rc = d4skip( d, 1L )) == r4locked) && (retry( ) == 1)) ;
       if( rc == r4locked ) return ;
    }
}

void main()
{
   code4init( &cb ) ;
   d = d4open( &cb, "DATEFILE" ) ;
   modifyRecordValues();
   code4initUndo( &cb );
}
