#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 codeBase ;
   DATA4 *dataFile ;

   code4init( &codeBase ) ;
   dataFile = d4open( &codeBase, "INFO" ) ;

   d4optimize( dataFile, OPT4ALL ) ;
   code4optStart( &codeBase ) ;

   d4top( dataFile ) ;
   printf( "Press ENTER when you want to refresh your data.\n") ;
   getchar( ) ;

   d4refresh( dataFile ) ;
   d4top( dataFile ) ;
 /* re-read the record from disk. */
   printf( "The latest information is: %s \n",d4record( dataFile ) ) ;

   d4close( dataFile ) ;
   code4initUndo( &codeBase ) ;
}
