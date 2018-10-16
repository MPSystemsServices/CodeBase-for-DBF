/*ex71.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info ;
   TAG4 *defaultTag ;
   long count = 0 ;

   code4init( &cb ) ;
   info = d4open( &cb, "INFO" ) ; /* automatically open data & index file*/
   defaultTag = d4tagDefault( info ) ;
   d4tagSelect( info, defaultTag ) ; /* Select default tag*/

   for( d4top( info ) ; ! d4eof( info ) ; d4skip( info, 1 ) )
      count ++ ;

   printf( "%d records in tag ", count ) ;
   printf( "%s\n", t4alias( defaultTag ) );

   d4close( info ) ;
   code4initUndo( &cb ) ;
}
