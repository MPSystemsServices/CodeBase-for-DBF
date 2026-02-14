#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main()
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *field ;
   short j ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA1") ;

   /* list the fields that are character fields */
   for (j = 1 ; j < d4numFields( data ) ; j++ )
   {
      field = d4fieldJ( data, j ) ;
      if ( f4type( field ) == r4str )
         printf( "%s\n", f4name( field ) ) ;
   }

   code4initUndo( &cb ) ;
}
