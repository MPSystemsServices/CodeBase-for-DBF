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
	data = d4open( &cb, "DATA2" ) ;
 	field = d4field( data, "NAME" ) ;
   d4top( data ) ;
	if ( d4changed( data, -1 ) != 0 )     /*Displays FALSE */
		printf( "Changed status: TRUE\n") ;
	else
    	printf( "Changed status: FALSE\n") ;
	d4lockAll( data ) ;  /*CODE4.lockEnforce default value is true, */
								/* so you must explicitly lock the record before*/
								/* modifying it */
   f4assign( field, "TEMP DATA") ;
	if ( d4changed( data, -1 ) != 0 )     /*Displays TRUE */
		printf( "Changed status: TRUE\n") ;
	else
    	printf( "Changed status: FALSE\n") ;

   d4changed( data, 0 ) ;

   d4close( data ) ;      /* The top record is not flushed.*/
   code4initUndo( &cb ) ;
 }

