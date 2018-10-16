//ex95.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 settings ;
   Data4 db( settings, "DATA" ) ;

   db.top( ) ;
   Str4large name ;
   Expr4 fullName( db, "TRIM( LNAME )+', '+FNAME" ) ;

   name.assign( fullName.vary( ), fullName.len( ) ) ;
   // copy the contents of the internal buffer.  Expr4::vary does not
   // guarantee a null termination, so Expr4::len is necessary.
   // For illustration purposes only:
   // Avoid using the Expr4 class when a Str4 or Field4 class will suffice

  name.trim( ) ;
  cout << name.ptr( ) << " is the first person in the data file" << endl ;

  settings.initUndo( ) ;
}
