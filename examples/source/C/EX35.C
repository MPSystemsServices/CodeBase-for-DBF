#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
	FIELD4 *field ;

	code4init( &cb ) ;
	data = d4open( &cb, "INFO" );
 	field = d4fieldJ( data, 1 ) ;

   /* output the first field of every record in reverse sequential order.*/
   for( d4bottom( data ) ; !d4bof( data ) ; d4skip( data, -1 ) )
         printf("%s \n",f4str( field)) ;
   d4close( data ) ;
	code4initUndo( &cb ) ;
}

