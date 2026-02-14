#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

FIELD4INFO fields[] =
{
   { "NAME_FLD", 'C', 20, 0 },
   { "AGE_FLD", 'N', 3, 0 },
   { 0,0,0,0 }
} ;

void main( )
{
   CODE4 cb ;
   DATA4 *data ;

   code4init( &cb ) ;
   cb.errOpen = 0 ;
   /* no error message is displayed if NO_FILE does not exist*/

   data = d4open( &cb, "NO_FILE" ) ;

   if( data == NULL )
   {

      /* Data file does not exist   */
      cb.safety = 0 ;
      data = d4create( &cb, "NO_FILE", fields, 0 ) ;
      if( data == NULL )
         printf( "Could not create NO_FILE\n") ;
   }
   code4initUndo( &cb ) ;
}
