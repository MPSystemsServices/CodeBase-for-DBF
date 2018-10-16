/*ex97.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

char *createBufCopy( FIELD4 *f )
{
   char *buf ;

   buf = (char *) malloc(f4len( f ) +1 ) ;
   memcpy( buf, f4ptr( f ), f4len( f ) );
   buf[f4len( f )] = 0;
   return buf ;
}
void main()
{
   CODE4 cb ;
   DATA4 *data;
   FIELD4 *field ;
   char *buffer ;

   code4init( &cb ) ;
   data = d4open( &cb, "INFO" ) ;
   field = d4field( data, "NAME" ) ;
   d4top( data ) ;
   buffer = createBufCopy( field ) ;
   printf( "the copy of the buffer is %s\n", buffer ) ;
   code4initUndo( &cb ) ;
}
