//ex48.cpp
#include "d4all.hpp"
extern unsigned  _stklen =  10000 ;  // for all Borland compilers

void main()
{
   Code4  cb ;
   Data4 data( cb, "INFO" ) ;

   /* Lock all of the records in the data file as well as
      the append bytes all at once.  Existing locks are
      removed. */

   int rc = data.lockFile( ) ;
   if ( rc == r4success )
   {
      cout << "Other users can read this data file, " ;
      cout << "but can make no modifications until\n" ;
      cout << "this lock is removed." << endl ;
   }

   cb.initUndo( ) ; // implicitly unlock the file.
}
