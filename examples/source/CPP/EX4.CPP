#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;

   // Do not add duplicate records to unique tags or the data file and 
   // return r4unique when attempted.
   cb.errDefaultUnique = r4unique ; 

   Data4 data( cb, "INFO" ) ;

   data.top( ) ;
   data.appendStart( ) ;     
   int rc = data.append( ) ; // append a duplicate copy of the top record
   
   if( rc == r4unique ) 
      cout << "Attempt to add a duplicate record failed." << endl ;
   else
   {
      cout << "Attempt to add a duplicate record succeeded" << endl ;
      cout << "Record in both data and index file" << endl ;
   }

   data.close( ) ;
   cb.initUndo( ) ;
}
