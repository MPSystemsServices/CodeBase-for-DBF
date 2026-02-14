#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

CODE4 cb ;
DATA4 *data ;
FIELD4 *fname ;

FIELD4INFO fieldInfo[] =
{
   { "FNAME", 'C', 20, 0 },
   { 0,0,0,0 },
} ;

#define NO_RECORDS 4

char *records[NO_RECORDS] =
{
   "HARRY",
   "JULES",
   "CLINT",
   "ROGER",
} ;

void main(void)
{
   int i ;

   code4init( &cb ) ;
   cb.safety = 0 ;
   data = d4create( &cb, "TO_DBF", fieldInfo, 0 ) ;
   if ( data )
   {
      fname = d4field( data, "fname" ) ;

      for ( i = 0 ; i < NO_RECORDS ; i++ )
      {
         d4appendStart( data, 0 ) ;
         f4assign( fname, records[i] ) ;
         if ( d4append( data ) )
            printf( "ERROR: Could not append record to database.\n" ) ;
      }
      printf( "The TO_DBF has been created.\n" ) ;
   }
   else
      printf( "ERROR: Could not create database.\n" ) ;

   code4initUndo( &cb ) ;
}



