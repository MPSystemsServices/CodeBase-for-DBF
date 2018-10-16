//ex67.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // Borland only

void main( void )
{
   Code4 cb ;
   Data4 data( cb, "DBF" ) ;
   Tag4 tag( data, "DBF_NAME" ) ; // A tag with '.NOT.DELETED()' filter

   data.lockAll( ) ;
   data.select( tag ) ;
   data.top( ) ; // Position to the first record that is not deleted.
   cout << "first record that is not deleted is " << data.recNo() << endl ;

   data.deleteRec( ) ; // The current record no longer is located in the tag

   data.tagSync( ) ; // The change to the record is flushed, and the datafile is
                     // positioned on a valid record.

   data.top( ) ;
   cout << "the new first record that is not deleted is " << data.recNo() << endl ;
   data.skip( ) ;

   //... some other code ...

   cb.initUndo( ) ;
}
