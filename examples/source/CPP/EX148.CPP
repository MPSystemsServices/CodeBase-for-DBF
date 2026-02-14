#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Sort4 sort( cb, sizeof(char), sizeof(char)*4 ) ;
   
   sort.put( "A", "BEN", 1) ;
   sort.put( "C", "DAN", 2) ;
   sort.put( "B", "ROB", 3) ;
   sort.put( "A", "ACE", 4) ;
   sort.getInit( ) ;

  /* Displays:
     A BEN 1
     A ACE 4
     B ROB 3
     C DAN 2   
 */ 
   while( sort.get( ) == 0 )
   {
      cout << *(char *) sort.result << " " 
           << (char *) sort.resultOther  
           << " " << sort.resultRec << endl ;
   }
   sort.free( ) ;
   cb.initUndo( ) ;
}
