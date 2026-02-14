/*ex99.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *comments, *name ;

   code4init( &cb ) ;
   data = d4open( &cb, "DATA3" ) ;
   comments = d4field( data, "COMMENTS" ) ;
   name = d4field( data, "NAME" ) ;

   d4top( data ) ;
   /* display the null terminated contents of the memo field*/
   printf( "Memo field contents: %s\n", f4memoPtr( comments ) ) ;

   /* display the non-null terminated contents of the NAME field.
      this displays NAME plus any additional fields in the record buffer */
   printf( "NAME field contents: %s\n", f4ptr( name ) ) ;

   code4initUndo( &cb ) ; /* close all files and free memory */
}
