// ex112.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "DATA3" ) ;
   Field4memo comments( data, "COMMENTS" ) ;
   Field4memo name( data, "NAME" ) ;

   data.top( ) ;
   // display the null terminated contents of the memo field
   cout << "Memo field contents: " << comments.ptr( ) << endl ;

   // display the non-null terminated contents of the NAME field.
   // this displays NAME plus any additional fields in the record buffer
   cout << "NAME field contents: " << name.ptr( ) << endl ;

   cb.initUndo( ) ; // close all files and free memory
}
