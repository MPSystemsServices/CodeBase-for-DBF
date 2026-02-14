#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

/* Check the validity of an index file. */
void main( int argc, char *argv[ 2 ] )
{
    if ( argc > 2 )
    {
     	CODE4 cb ;
		DATA4 *checkData ;
		INDEX4 *testIndex ;

		code4init( &cb ) ;
		cb.accessMode = OPEN4DENY_RW ;
      	cb.autoOpen = 0 ;  /* open index file manually.*/

 		checkData = d4open( &cb, argv[1] ) ;
 		testIndex = i4open( checkData, argv[2]);

       	error4exitTest( &cb ) ;
       	code4optStart( &cb ) ;

       	if ( d4check( checkData ) == r4success )
           printf("\nIndex is OK !!\n") ;
       	else
			printf("\nProblem with Index !!\n") ;
		code4initUndo( &cb );
    }
    else
       printf( "\nUsage: PROGRAM  DataFile  IndexFile\n") ;
 }

