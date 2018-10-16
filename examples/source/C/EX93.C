#include "d4all.h"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void showExpr( EXPR4 *ex )
{
   switch( expr4type( ex ) )
   {
      case  r4date:
         printf( "type is r4date\n" ) ;
         break ;
      case  r4dateDoub:
         printf( "type is r4dateDoub\n" ) ;
         break ;
      case  r4log:
         printf( "type is r4log\n" ) ;
         break ;
      case  r4num:
         printf( "type is r4num\n" ) ;
         break ;
      case  r4numDoub:
         printf( "type is r4numDoub\n" ) ;
         break ;
      case  r4str:
         printf( "type is r4str\n" ) ;
         break ;
      case  r4memo:
         printf( "type is r4memo\n" ) ;
         break ;
   }
}

void main( )
{
   CODE4 cb ;
   DATA4 *db ;
   EXPR4 *ex ;

   code4init( &cb ) ;
   db = d4open( &cb, "info" ) ;
   d4top( db ) ;

   ex = expr4parse( db, "NAME" ) ;
   showExpr( ex ) ;
   expr4free( ex ) ;

   ex = expr4parse( db, "AGE" ) ;
   showExpr( ex ) ;
   expr4free( ex ) ;

   ex = expr4parse( db, "BIRTH_DATE" ) ;
   showExpr( ex ) ;
   expr4free( ex ) ;

   code4close( &cb ) ;
   code4initUndo( &cb ) ;
}
