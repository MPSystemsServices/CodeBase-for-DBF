//ex137.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

static FIELD4INFO fieldInfo[ ] =
{
   { "FIELD_NAME", 'C', 10, 0 },
   { "VALUE", 'N', 7, 2 },
   { 0,0,0,0 }
} ;

TAG4INFO tagInfo[ ] =
{
   { "T_NAME", "FIELD_NAME", "FIELD_NAME > 'A'", 0,0 },
   { "NAME_TWO", "VALUE", "", e4unique, r4descending },
   { 0,0,0,0,0 }
} ;

void main( )
{
   Code4 cb ;
   Data4 data ;
   Index4 index ;

   cb.safety = 0 ;
   data.create( cb, "DB_NAME", fieldInfo ) ;
   if ( data )
      index.create( data, "name", tagInfo ) ;

   data.close( ) ;
   cb.initUndo( ) ;
}
