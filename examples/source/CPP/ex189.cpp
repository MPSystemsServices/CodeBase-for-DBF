#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000;
#endif

void main()
{
   Code4 cb ;
   Data4 data( cb, "DATA1" ) ;
   Field4 field ;
   short j ;

   /* list the fields that are character fields */
   for (j = 1 ; j < data.numFields( ) ; j++ )
   {
      field.init( data, j ) ;
      if ( field.type( ) == r4str )
         cout << field.name( ) << endl ;
   }

   cb.initUndo( ) ;
}
