#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *infoFile ;
   FIELD4 *field ;
   short num ;

   code4init( &cb ) ;
   infoFile = d4open( &cb, "INFO" );

   for(num = d4numFields( infoFile ) ; num ; num-- )
   {
      field = d4fieldJ( infoFile, num ) ;
         if( num == d4fieldNumber( infoFile, f4name( field )) )
            printf( "This is always true.\n") ;
   }
   code4initUndo( &cb );
}
