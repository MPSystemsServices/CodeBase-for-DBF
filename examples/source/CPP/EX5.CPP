//ex5.cpp
#include "d4all.hpp"
extern unsigned _stklen = 15000; // for all Borland complilers

void main( )
{
   Code4 code ;
   Data4 data( code, "INFO" ) ;

   char badExpr[] = "NAME = 5" ;

   Expr4 expression( data, badExpr ) ;

   cout << endl << "An error message just displayed" << endl ;

   code.errorCode = code.errExpr = 0 ;

   expression.parse( data, badExpr ) ;
   cout << "No error message displayed." << endl ;

   if (expression.isValid())
      expression.free( ) ;

   code.initUndo( ) ;
}
