#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void compressAll( DATA4 *d )
{
    if( d4pack( d ) == r4success )
       printf( "Records marked for deletion are removed\n" ) ;
    if( d4memoCompress( d ) == r4success )
       printf( "Memo entries are compressed\n") ;
 }

void main ()
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA" ) ;
   compressAll( data ) ;
   code4initUndo( &cb ) ;
}
