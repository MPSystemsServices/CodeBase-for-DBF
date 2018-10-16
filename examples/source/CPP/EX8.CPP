//ex8.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

FIELD4INFO fields[] =
{
   { "NAME_FLD", 'C', 20, 0 },
   { "AGE_FLD", 'N', 3, 0 },
   { 0,0,0,0 }
} ;

void main( )
{
   Code4 cb ;
   cb.errOpen = 0 ;
   // no error message is displayed if NO_FILE does not exist

   Data4 data( cb, "NO_FILE" ) ;

   if( data.isValid() == 0 )
   {
      // Data file does not exist
      cb.safety = 0 ;
      data.create( cb, "NO_FILE", fields ) ;
      if( ! data.isValid( ) )
         cout << "Could not create NO_FILE" << endl ;
   }
   cb.initUndo( ) ;
}
