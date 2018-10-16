//ex118.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   File4 textFile ;

   cb.accessMode = OPEN4DENY_RW ;
   cb.errCreate = 0 ; // Handle the errors at the application level
   int rc = textFile.create( cb, "C:\\TEMP\\TEXT.FIL" ) ;

   if( rc < 0 || rc == r4noCreate)
   {
      cout << "File 'C:\\TEMP\\TEXT.FIL' NOT created. Make sure it does not exist." << endl ;
      cb.exit( ) ;
   }
   textFile.write( 0, "Some Sample Text", 16 ) ;

   textFile.close( ) ;
   cb.initUndo( ) ; // flush changes and close file.
}
