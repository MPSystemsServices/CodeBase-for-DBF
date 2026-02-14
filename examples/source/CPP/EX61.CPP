//ex61.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void update( Data4 d )
{
   if( d.changed( ) )
      cout << "Changes not discarded." << endl ;
   else
      d.refreshRecord( ) ;
}

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;
   data.top( ) ;
   data.lock(1) ;
   Field4 field( data, "NAME" ) ;
   field.assign( "Marvin" ) ;
   update( data ) ;
   data.skip( ) ;
   update( data ) ;
   cb.initUndo( ) ;
}
