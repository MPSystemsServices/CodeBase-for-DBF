#include "d4all.hpp"

void main()
{
   Str4ten name( "BOB" ) ;
   Str4ptr address( "1234 Anytown Street Apt. 12" ) ;

   /* the following cout does not work correctly, since the internal 
      CodeBase buffer used for name.str( ) is overwritten by the address.str()
      function call */
   cout << name.str( ) << " lives at " << address.str( ) <<endl ; 
 
   // these two lines produce the desired result
   cout << name.str( ) << " lives at " ;
   cout << address.str( ) << endl ; 
}
