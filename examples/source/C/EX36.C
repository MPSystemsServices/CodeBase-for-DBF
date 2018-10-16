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
	data = d4open( &cb, "INFO" ) ;
	field = d4field( data, "NAME" ) ;

   d4bottom( data ) ;
   printf("Last Name added: %s\n", f4str( field ) ) ;
   d4close( data ) ;
	code4initUndo( &cb ) ;
}

