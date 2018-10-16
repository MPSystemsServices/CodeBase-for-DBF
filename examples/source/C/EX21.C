#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 code ;
	DATA4 *dataFile ;
   int delCount, rc ;

   code4init( &code ) ;
   code.accessMode = OPEN4DENY_RW ;

   dataFile = d4open( &code, "INFO" ) ;
   error4exitTest( &code ) ;

   /* initialize optimization with default settings. */
   code4optStart( &code ) ;

   delCount = 0 ;
   for( rc = d4top( dataFile ); rc == r4success ; rc = d4skip( dataFile, 1L ) )
      if( d4deleted( dataFile) )
         delCount++ ;

   printf("%d records are marked for deletion.\n", delCount ) ;

   code4initUndo( &code ) ;
}
