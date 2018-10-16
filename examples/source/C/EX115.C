/*ex115.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

#define DATE_OFFSET 16

void main( void )
{
   CODE4 cb ;
   FILE4 file ;
   char runDate[9] ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;

   date4today( runDate ) ; /* initialize to the system clock*/
   runDate[8] = 0 ;
   if( file4open( &file, &cb, "TEXT.FIL", 0 ) !=0 )
      return ;

   /* file is opened exclusively - no need to lock*/
   file4write( &file, DATE_OFFSET, runDate, sizeof(runDate) ) ;
   file4close( &file ) ;
   code4initUndo( &cb );
}
