#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *ageField ;
   long age ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;

   d4optimizeWrite( data, OPT4ALL ) ;
   /* when doing write optimization on shared files, it is necessary to
         lock the file, preferably with d4lockAll( ) */
   d4lockAll( data ) ;

   code4optStart( &cb ) ; /* begin optimization */

   age = 20 ;
   ageField = d4field( data, "AGE") ;

   /* append a copies of the first record, assigning the age field's
       value from 20 to 65*/
   for( d4top( data ) ;  age < 65 ; d4append( data ) )
   {
      d4appendStart( data, 0 ) ;
      f4assignLong( ageField, age++ ) ;
   }

   code4initUndo( &cb ) ; /* flushes, closes, and unlocks */
}
