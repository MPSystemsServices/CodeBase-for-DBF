#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info, *data;

   code4init( &cb );
   info = d4open( &cb, "INFO" ) ;
   data = d4open( &cb, "DATA" ) ;

   d4lock( info, 1L ) ;
   d4lockAll( data ) ;

   code4unlock( &cb ) ;  /*unlocks all open files.*/

   code4initUndo( &cb ) ;
}
