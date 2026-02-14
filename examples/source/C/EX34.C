#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
    CODE4 cb ;
    DATA4 *data ;

    code4init( &cb ) ;
    data = d4open( &cb, "DATABASE") ;

    for( d4top( data ) ; !d4eof( data ) ; d4skip( data, 1L ) )
       d4blank( data )  ;
  /* blank out all records in the data file*/
    code4initUndo( &cb ) ;  /* close all files and release any memory */
}
