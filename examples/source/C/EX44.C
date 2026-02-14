#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *age ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" );
   d4top( data ) ;
   d4go( data, 2 ) ;
   age = d4field( data, "AGE" ) ;
   d4lockAll( data ) ;
   f4assignLong( age, 49 ) ;

   /* Explicitly flush the change to disk in case power goes out*/
   d4flush( data ) ;

   /* some other code*/

   d4close( data ) ;
   code4initUndo( &cb ) ;
}
