#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 temp ;

   cb.errCreate = 0 ;

   if( temp.create( cb, "NEWFILE.TXT" ) == r4noCreate)
      // File exists. Try in the temp directory.
      temp.create( cb, "C:\\temp\\NEWFILE.TXT" ) ;

   if( cb.errorCode < 0 )
      cb.exit( ) ;

   // Some other code

   cb.initUndo( ) ;
}
