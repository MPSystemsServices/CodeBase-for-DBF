/*ex0.c*/
#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main( )
{
   CODE4 settings ;
   DATA4 *dataFile ;

   code4init( &settings ) ;
   settings.autoOpen = 0 ;
   settings.memSizeBlock = 0x800 ; /* 2048*/
   settings.memSizeBuffer = 0x2000 ; /* 8192*/

   settings.errDefaultUnique = r4unique ;

   dataFile = d4open( &settings, "INFO.DBF" ) ;

   /* this is equivalent to calling error4exitTest( )*/
   if( settings.errorCode < 0 )  code4exit( &settings ) ;

   /* ... */
   code4initUndo( &settings ) ;
}
