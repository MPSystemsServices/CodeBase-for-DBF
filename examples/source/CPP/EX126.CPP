//ex126.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

void main(  )
{
   Code4 cb ;
   cb.optStart( ) ;
   cb.safety = 0 ;

   File4 primary, secondary ;
   primary.create( cb, "PRI" ) ;
   secondary.create( cb, "SEC" ) ;

   primary.write(   0, "PRIMARY FILE", 12 ) ;
   secondary.write( 0, "SECONDARY FILE", 14 ) ;

   int rc = primary.replace( secondary ) ;

   if( rc < 0 )
      cout << "An error occurred in File4::replace" << endl ;

   if( secondary.isValid( ) )
      cout << "This should never happen" << endl ;

   Str4large buffer ;
   buffer.setLen( 14 ) ;
   primary.read( 0, buffer ) ;

   Str4ptr str( "SECONDARY FILE" ) ;
   if( buffer == str )
      cout << "This should always be true." << endl ;

   primary.close( ) ;
   cb.initUndo( ) ;
}
