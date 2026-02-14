/*ex30.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( void )
{
   CODE4 cb ;
   DATA4 *info ;
   FIELD4 *field ;
   long iRec ;

   code4init( &cb ) ;
   cb.accessMode = OPEN4DENY_RW;
   info = d4open( &cb, "DATABASE.DBF") ;
   code4optStart( &cb ) ;

   field = d4field( info, "NAME") ;

   for(iRec = 1L ; iRec <= d4recCount( info ) ; iRec++ )
   {
      d4go( info, iRec ) ;
      f4assign( field, "New Data" ) ;
   }

   d4close( info ) ;
   code4initUndo( &cb ) ;
   code4exit( &cb ) ;
 }
