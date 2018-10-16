#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   cb.errOpen = 0 ;
   File4 textfile( cb, "TEXT.FIL") ;

   if( textfile.isValid( ) )
      cout << "File was opened" << endl ;
   else
      cout << "File was NOT opened" << endl ;
   cb.initUndo( ) ;
}
