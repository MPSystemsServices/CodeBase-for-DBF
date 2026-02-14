/*ex90.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#define VOTE_AGE 18.0

void main( )
{
   CODE4 cb ;
   DATA4 *data ;
   EXPR4 *expr ;
   long count = 0;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   expr = expr4parse( data, "AGE" ) ;

   for( rc = d4top( data ) ; rc != r4eof ; rc = d4skip( data, 1 ) )
      if( expr4double( expr ) >= VOTE_AGE )
         count ++ ;

   printf( "Possible voters: %d\n", count ) ;

   expr4free( expr ) ;
   code4initUndo( &cb ) ;
}
