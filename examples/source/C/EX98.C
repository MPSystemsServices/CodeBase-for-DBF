/*ex98.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *comments ;
   long count ;

   code4init( &cb ) ;
   data = d4open( &cb , "DATA3" ) ;
   comments = d4field( data, "COMMENTS" ) ;

   error4exitTest( &cb ) ;
   count = 0 ;
   for( d4top( data ) ; !d4eof( data ) ; d4skip( data, 1) )
      if( f4memoLen( comments ) )
          count ++ ;

   printf( "There were %ld memo entries out of %ld records\n", count,
                                    d4recCount( data ));
   code4initUndo( &cb ) ;
}
