//ex82.cpp
#include "d4all.hpp"

void main( )
{
   Date4 birthDate ;

   cout << "Enter your birthdate in CCYYMMDD format" << endl ;
   cin >> birthDate.ptr( ) ;

   cout << "You were born on a " << birthDate.cdow( ) << endl ;
   // displays "You were born on a Monday" if a monday was entered.
}
