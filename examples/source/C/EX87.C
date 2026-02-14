/* ex87.c */
#include "d4all.h"

int display( CODE4 *cb, char *p )
{
   if( p == NULL )
      return error4describe( cb, e4parm, 0, "Null display string", 0, 0 ) ;
   printf( "%s\n", p ) ;
   return 0 ;
}

void main()
{
   CODE4 cb ;
   char p[] = "some string" ;

   code4init( &cb ) ;
   display( &cb, p ) ;
   display( &cb, 0 ) ;
   code4initUndo( &cb ) ;
}
