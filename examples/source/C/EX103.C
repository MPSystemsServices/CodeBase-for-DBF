/*ex103.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   FILE4 file ;
   char readInfo[50] ;
   unsigned lenRead ;

   code4init( &cb ) ;
   cb.safety = 0 ;

   file4create( &file, &cb, "TEXT.FIL", 0 ) ;
   error4exitTest( &cb ) ;

   file4write( &file, 0, "Some File Information", 21 ) ;
   lenRead = file4read( &file, 10, readInfo, sizeof( readInfo) ) ;

   if( memcmp (readInfo, "Information" , lenRead ) == 0 )
      printf( "This is always true.\n" ) ;
   if( lenRead == 11 )
      printf( "This is always true, too.\n" ) ;
   file4close( &file ) ;
   code4initUndo( &cb ) ;
}
