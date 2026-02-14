/*ex95.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info, *data ;
   FIELD4 *infoName, *dataLname ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW ;
   info = d4open( &cb, "INFO" ) ;
   data = d4open( &cb, "DATA" ) ;
   error4exitTest( &cb ) ;

   infoName = d4field( info, "NAME" ) ;
   dataLname = d4field( data, "LNAME" ) ;

   for( d4top( info ), d4top( data ) ; !d4eof( info ) && !d4eof( data ) ;
   d4skip( info, 1 ), d4skip( data, 1 ) )
   f4assignField( infoName, dataLname ) ; /* copy 'LNAME' into 'NAME' */

   code4initUndo( &cb ) ;
}
