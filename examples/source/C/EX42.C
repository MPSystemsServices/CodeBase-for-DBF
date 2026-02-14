#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *infoFile ;

   code4init( &cb ) ;
   infoFile = d4open( &cb, "INFO" ) ;

   /* Go to the end of file and set the End of file flag*/
   d4goEof( infoFile ) ;

   /* Check to see if the end of file flag is set*/
   if( d4eof( infoFile )  )
   {
      printf( "This is always true\n") ;
      d4bottom( infoFile ) ; /* reset the eof flag*/
   }

   d4close( infoFile ) ;
   code4initUndo( &cb ) ;
}
