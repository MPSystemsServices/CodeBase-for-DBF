#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main(  )
{
   CODE4 cb ;
   DATA4 *data ;
   int rc ;
   short fieldNum ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;

   code4optStart( &cb ) ;

   for( rc = d4top( data ); rc != r4eof ; rc = d4skip( data, 1L ) )
   {
      printf( "\n" ) ;

      for( fieldNum = 1 ; fieldNum <= d4numFields( data ) ; fieldNum++ )
          printf( " %s \n", d4fieldJ(data, fieldNum)) ;
   }
   code4initUndo( &cb ) ;
 }
