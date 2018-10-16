//ex63.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 people( cb, "people.dbf" ) ;

   /* Assume 'PEOPLE.DBF' has a production index file with tags
      PPL_NAME, PPL_AGE, PPL_BRTH */
   people.select(  Tag4( people, "PPL_NAME" ) ) ;

   if( people.seek( "fred " ) == r4success )
      cout << "fred is in record # " << people.recNo( ) << endl ;

   if( people.seek( "HANK STEVENS" ) == r4success )
      cout << "HANK STEVENS is in record #" << people.recNo( ) << endl ;

   people.select( Tag4( people, "PPL_AGE" ) ) ;
   Field4 age( people, "AGE" ) ;

   int rc = people.seek( 0.0 ) ;

   if( rc == r4success || rc == r4after )
      cout << "The youngest age is: " << (double) age << endl ;

   // Seek using the char * version
   rc = people.seek( "0" ) ;
   if( rc == r4success || rc == r4after )
      cout << "The youngest age is: " << (double) age << endl ;

   // Assume PPL_BRTH is a Date key expression
   people.select( Tag4( people, "PPL_BRTH" )) ;
   Date4 birth( "19600415" ) ;

   if( people.seek( birth.str( ) ) == r4success ) // Char. array in CCYYMMDD format
      cout << "Found: " << birth.format( "MMM DD, CCYY" ) << endl;

   if( people.seek( (long) birth ) == r4success )
      cout << "Found: " << birth.format( "MMM DD, CCYY" ) << endl;

   people.close( ) ;
   cb.initUndo( ) ;
}
