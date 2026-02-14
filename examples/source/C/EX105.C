/*ex105.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 textFile ;
   int rc ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;
   cb.errCreate = 0 ; /* Handle the errors at the application level*/
   rc = file4create( &textFile, &cb, "C:\\TEMP\\TEXT.FIL", 0 ) ;

   if( rc < 0 || rc == r4noCreate)
   {
      printf( "File 'C:\\TEMP\\TEXT.FIL' NOT created. Make sure it does not exist.\n") ;
      code4exit( &cb ) ;
   }
   file4write( &textFile, 0, "Some Sample Text", 16 ) ;

   file4close( &textFile ) ;
   code4initUndo( &cb ) ; /* flush changes and close file. */
}
