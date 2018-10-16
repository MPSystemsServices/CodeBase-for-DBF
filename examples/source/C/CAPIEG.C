#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

FIELD4INFO fields[ ] =
{
 	{ "NAME", 'C', 20, 0 },
 	{ "AGE", 'N', 3, 0 },
	{ 0,0,0,0 },
} ;

void main( )
{
	CODE4 cb ;
	DATA4 *data ;
	code4init( &cb ) ;
	cb.safety = 0 ;
	data = d4create( &cb, "PERSON", fields, 0 ) ;
	if( data )
		printf( "PERSON data file successfully created.\n" ) ;
	code4close( &cb ) ;
	code4initUndo( &cb ) ;
}

