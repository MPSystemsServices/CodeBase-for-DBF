#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 cb ;
DATA4 *data ;
FIELD4 *value ;

FIELD4INFO fieldInfo[] =
{
   { "VALUE", 'C', 10, 0 },
   { 0,0,0,0 },
} ;

void main(void)
{
   code4init( &cb ) ;
   cb.safety = 0 ;
   data = d4create( &cb, "VALUES", fieldInfo, 0 ) ;
   if ( data )
   {
      value = d4field( data, "VALUE" ) ;

      d4appendStart( data, 0 ) ;
      f4assign( value, "12.5" ) ;
      if ( d4append( data ) )
         printf( "ERROR: Could not append record to VALUES database.\n" ) ;
   }
   else
      printf( "ERROR: Could not create VALUES database.\n" ) ;

   code4initUndo( &cb ) ;
}
