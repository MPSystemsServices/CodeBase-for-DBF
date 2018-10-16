//ex52.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000;

void main( )
{
   Code4 cb ;
   cb.accessMode = OPEN4DENY_RW ;

   // Open the file exclusively, default optimization is the same as if
   // Data4::optimize( OPT4EXCLUSIVE ) were called.
   Data4  info( cb, "INFO" ) ;

   // open a shared file.
   cb.accessMode = OPEN4DENY_NONE ;
   Data4 extra( cb, "DATA" ) ;

   extra.optimize( OPT4ALL ) ;  // read optimize the "DATA" file

   int rc = cb.optStart( ) ;  // Begin the memory optimizations.
   if ( rc == r4success )
      cout << "Memory optimiztion is implemented" << endl ;
   else
      cout << "Either the S4OFF_OPTIMIZE switch is defined or "
            << "there is insufficient memory for optimization" << endl ;

   // .... Some other code ....

   cb.closeAll( ) ;
   cb.initUndo( ) ;
}
