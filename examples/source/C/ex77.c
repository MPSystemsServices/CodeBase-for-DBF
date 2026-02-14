/*ex77.c*/
#include "d4all.h"

void main( )
{
   char birthDate[8+2] ;

   printf( "Enter your birthdate in CCYYMMDD format\n") ;
   fgets( birthDate, 10, stdin ) ;

   printf( "You were born on a %s\n", date4cdow( birthDate )) ;
   /* displays "You were born on a Monday" if a monday was entered. */
}
