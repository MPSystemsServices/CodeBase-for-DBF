/* ex72.c */
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *name ;
   TAG4 *nameTag ;
   int rc ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   nameTag = d4tag( data, "INF_NAME" ) ;
   name = d4field( data, "NAME" ) ;

   error4exitTest( &cb ) ; /* check for errors*/
   d4lockAll( data ) ;
   d4tagSelect( data, nameTag ) ;

   for(rc = d4seek( data, "Fred" ) ; rc == r4success ; rc = d4skip( data, 1) )
   {
      if( memcmp( f4ptr( name ), "Fred", 4 ) == r4success )
         printf( "Fred in record %d.\n", d4recNo( data ) );
      else
         break ;
   }
   d4unlock( data ) ;

   d4close( data ) ;
   code4initUndo( &cb ) ;
}
