#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 cb ; /* CODE4 may be constructed globally.*/
DATA4  *data ;
FIELD4 *field ;

void main( void )
{
    code4init( &cb ) ;
	 cb.accessMode = OPEN4DENY_RW ;
 	 data = d4open( &cb, "DATABASE" ) ;
    code4optStart( &cb ) ;

    d4appendBlank( data ) ;

    /* Append a copy of record two.  (Assume record two exists.)*/
    d4go( data, 2 ) ;
    d4appendStart( data, 0) ;/* a false useMemoEntries parameter */
				/* Append a copy of record 2 including existing memo entries.*/
    d4append( data ) ;

    d4go( data, 2 ) ;
    d4appendStart( data, 1 ) ; /* a true parameter means use memo entries */
    d4append( data ) ;

    /* Set the record buffer to blank, change a field's value, and append
		the resulting record.*/
 	 d4appendStart( data, 0 ) ;
    d4blank( data ) ;

 	 field = d4field( data, "NAME" ) ;
    f4assign( field, "New field value" ) ;

    d4append( data ) ;
    /* close all open files and release any allocated memory */
    code4initUndo( &cb ) ;
}

