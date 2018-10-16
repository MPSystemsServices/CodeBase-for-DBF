#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

static FIELD4INFO fieldArray[ ] =
{
    { "NAME_FIELD", 'C', 20, 0 },
    { "AGE_FIELD",  'N',  3, 0 },
    { "BIRTH_DATE", 'D',  8, 0 },
    { 0,0,0,0 }
};


void main( void )
{
    CODE4 cb ;
    DATA4 *data ;

	 code4init( &cb ) ;
    cb.safety = 0 ;       /* overwrite the file if it exists*/
    data = d4create( &cb, "NEWDBF", fieldArray, 0 ) ;
    error4exitTest( &cb ) ;

    if( cb.errorCode )
       printf( "An error occurred, NEWDBF not created\n") ;
    else
       printf( "Created successfully!\n") ;

    code4close( &cb ) ;
    code4initUndo( &cb ) ;
 }

