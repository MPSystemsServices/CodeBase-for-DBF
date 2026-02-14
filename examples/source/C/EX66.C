#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *name ;

   code4init( &cb ) ;
   data = d4open( &cb, "NAMES" ) ;
   /* Skip to the last record in the file whose NAME field is "John"*/

   code4optStart( &cb ) ;

   name = d4field( data, "F_NAME" ) ;

   for( d4bottom( data ) ;! d4bof( data ) ; d4skip( data, -1L ) )
      if ( strcmp( "John        ", f4str( name )) == 0 )  /* 0 indicates a find */
         break ;

   if( d4bof( data ) )
      printf( "John not located\n") ;
   else
      printf( "The last John is in record: %d\n",d4recNo( data ) ) ;

   printf(" the total number of records in file is %d\n", d4recCount(data)) ;
   d4close( data ) ;
   code4initUndo( &cb ) ;
}
