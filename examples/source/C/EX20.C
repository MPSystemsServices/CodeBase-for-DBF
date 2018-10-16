#include "d4all.h"


#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4  *data1, *data2;
   long numRec;

   code4init( &cb ) ;
   data1 = d4open( &cb, "DATABASE" ) ;
   data2 = d4open( &cb, "DATA2" ) ;

   d4top( data1 ) ;
   d4top( data2 ) ;

   d4lockAddFile( data1 ) ;
   d4lockAddAppend( data2 ) ;

   numRec = d4recCount( data2 ) ;
   d4lockAdd( data2, numRec ) ;
   d4lockAdd( data2, numRec-1 ) ;

   if( code4lock( &cb ) == r4success )
      printf( "All locks were successfully performed\n") ;

   code4initUndo( &cb ) ;
}
