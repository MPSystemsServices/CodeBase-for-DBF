//ex6.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000; // for all Borland compilers

void main( )
{
   Code4 code ;
   Data4 data( code, "INFO" ) ;
   char badField[] = "notAField" ;

   Field4  field( data, badField ) ;
   cout << endl << "An error message just displayed" << endl ;

   code.errorCode = code.errFieldName = 0 ;

   field.init( data, badField) ;
   cout << "No error message displayed." << endl ;

   code.initUndo( ) ;
}
