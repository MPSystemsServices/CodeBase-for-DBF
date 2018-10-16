/*ex74.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

long zapLast( DATA4 *info, long toDelete )
{
   printf( "%s has %d records\n",d4alias( info ), d4recCount( info )) ;

   d4zap( info, d4recCount( info) - toDelete + 1L, 1000000) ;
   printf( "%s now has %d records\n",d4alias( info ), d4recCount( info ));
   return d4recCount( info ) ;
}

void main()
{
   CODE4 cb ;
   DATA4 *data ;
   int rc ;
   long toDelete = 3;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;
   data = d4open( &cb, "INFO" ) ;
   rc = zapLast( data, toDelete ) ;
   code4initUndo( &cb ) ;
}
