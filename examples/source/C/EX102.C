/* ex102.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main ()
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *field1, *field2 ;

   code4init( &cb ) ;
   data = d4open( &cb , "INFO" ) ;
   field1 = d4fieldJ( data, 1 ) ;
   field2 = d4fieldJ( data, 2 ) ;
   d4top( data ) ;
   printf( "Field 1: %s\n", f4str(field1) );
   printf( "Field 2: %s\n", f4str(field2) );
   code4initUndo( &cb ) ;
}
