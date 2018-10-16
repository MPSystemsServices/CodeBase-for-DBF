//ex20.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

int display( Code4 &cb, char *p )
{
   if( p == NULL )
      return cb.error( e4parm, 0, "Null display string", NULL, NULL ) ;

   cout << p << endl ;
   return 0 ;
}

void main()
{
   Code4 code ;
   char someString[] = "Hello There" ;
   display( code, someString );
   display( code, NULL ) ;
   code.initUndo( ) ;
}
