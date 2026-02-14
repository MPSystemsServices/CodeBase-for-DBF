//ex113.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 cb ;
   Data4 data( cb, "DATA3" ) ;

   data.top( ) ;
   Field4memo comments( data, "COMMENTS" ) ;
   data.lockAll( ) ;
   comments.setLen( 0x4000 ) ; // 16K
   comments.assign( "First characters of a 16k memo field" ) ;

   // Flush changes to disk and close the data and memo files
   data.close( ) ;

   data.open( cb, "DATA2" ) ;
   comments.init( data, "COMMENT" ) ;
   data.top( ) ;

   cout << "Memo field Value:" << endl << comments.str( ) << endl
        << "Length of memo field: " << comments.len( ) << endl ;

   data.close( ) ;
   cb.initUndo( ) ;
}
