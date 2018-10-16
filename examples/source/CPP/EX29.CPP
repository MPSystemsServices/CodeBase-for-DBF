//ex29.cpp
#include "d4all.hpp"

#ifdef __TURBOC__  // for all Borland compilers
   extern unsigned _stklen = 10000 ;
#endif

void main()
{
   Code4 cb ;
   Data4 data( cb, "INFO.DBF" ) ;

   Str4ten st(data.alias());
   Str4ptr sp("INFO");
   if( st == sp )
      cout << "This is always true" << endl;

   cb.initUndo() ;
}
