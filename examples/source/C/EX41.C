#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 codeBase ;
   DATA4 *file ;
   long count ;

   code4init( &codeBase ) ;
   file = d4open( &codeBase, "INFO") ;

   code4optStart( &codeBase ) ;

   count = 0 ;

   for( d4top( file ); !d4eof( file ); d4skip( file, 1L ) )
      if( d4deleted( file ) )
         count++ ;
   printf( "INFO has %d deleted records\n", count) ;
   code4initUndo( &codeBase ) ;
}
