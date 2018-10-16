#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO.DBF" ) ;

   printf( "%s is the alias of the file INFO.DBF \n", d4alias( data )) ;

	code4initUndo( &cb ) ;
}
