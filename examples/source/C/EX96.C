/*ex96.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void displayFieldStats( FIELD4 *f )
{
   DATA4 *db ;

   db = f4data( f ) ;
   printf( "-----------------------------------------------------\n") ;
   printf( "DataFile: %s Field: %s\n", d4alias( db ), f4name( f ) ) ;
   printf( "Length: %d     Type : %c\n", f4len( f ), f4type( f ) ) ;
   printf( "Decimals: %d\n", f4decimals( f ) ) ;
   printf( "-----------------------------------------------------\n" ) ;
   return ;
}
void main()
{
   CODE4 cb ;
   DATA4 *data ;
   FIELD4 *field ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   field = d4field( data, "NAME" ) ;

   displayFieldStats( field );
   code4initUndo( &cb ) ;
}

