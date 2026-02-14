#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4  cb ;
   DATA4 *data ;

   code4init( &cb );
   data = d4open( &cb, "INFO" ) ;

   if ( d4lockFile( data ) == r4success)
   {
      printf( "Other users can read this data file, but can make ") ;
      printf( "no modifications until this lock is removed.\n") ;
   }

   code4initUndo( &cb ) ; /* implicitly unlock the file. */
}
