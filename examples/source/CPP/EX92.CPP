//ex92.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main()
{
   Code4  cb ;
   Data4 data( cb, "DATA" ) ;

   data.go( 1L ) ;

// "FNAME" and "LNAME" are Character field names of data file "DATA.DBF"
    Expr4 expr( data, "FNAME+\' \'+LNAME") ;

    cout << "FNAME and LNAME for Record One: " << expr.vary( ) << endl ;

    expr.free( ) ;
    data.close( ) ;
    cb.initUndo( ) ;
}
