//ex50.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main(  )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   cb.optStart( ) ;

   for( int rc = data.top( ); rc != r4eof ; rc = data.skip( ) )
   {
      int fieldNum ;
      for( fieldNum = 1 ; fieldNum <= data.numFields( ) ; fieldNum++ )
         cout << Field4(data, fieldNum).str( ) << "   " ;
      cout << endl ;
   }
   cb.initUndo( ) ;
}
