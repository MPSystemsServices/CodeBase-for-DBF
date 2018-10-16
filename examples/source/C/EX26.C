/*ex26.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main()
{
   CODE4 codeBase ;
   FIELD4 *value ;
   DATA4 *data ;
   long l ;

   code4init( &codeBase ) ;

   data = d4open( &codeBase, "VALUES" ) ;
   value = d4field( data, "VALUE" ) ;

   d4go( data, 1L ) ;
   l = c4atol( f4ptr( value ), f4len( value ) ) ;
   printf( "Value: %ld\n", l ) ;
   code4initUndo( &codeBase );
}
