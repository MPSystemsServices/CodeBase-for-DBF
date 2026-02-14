//ex100.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;

void main( )
{
   Code4 settings ;
   Data4 info( settings, "INFO" ) ;
   Data4 data( settings, "DATA" ) ;
   settings.exitTest( ) ;

   Field4 infoName( info, "NAME" ) ;
   Field4 dataLname( data, "LNAME" ) ;

   info.lockAddAll( ) ;
   data.lockAddAll( ) ;
   settings.lock( ) ;
   for( info.top( ) , data.top( ) ; !info.eof( ) && !data.eof( ) ;
        info.skip( ) ,data.skip( ) )
      dataLname.assignField( infoName ) ; // copy 'LNAME' into 'NAME'

   settings.closeAll( ) ;
   settings.initUndo( ) ;
}
