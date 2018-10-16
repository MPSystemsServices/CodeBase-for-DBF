#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 code ;
   DATA4 *data ;
   FIELD4 *field ;
   char badField[] = "notAField" ;

   code4init( &code ) ;
   data = d4open( &code, "INFO" ) ;
   field = d4field( data, badField ) ;
   printf( "\nAn error message just displayed\n") ;

   code.errorCode = code.errFieldName = 0 ;

   field = d4field( data, badField) ;
   printf( "\nNo error message displayed.\n" );

   code4initUndo( &code ) ;
}
