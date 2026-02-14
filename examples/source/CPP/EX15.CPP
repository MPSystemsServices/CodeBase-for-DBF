// ex15.cpp
#include "d4all.hpp"

extern unsigned _stklen = 10000 ; // for all Borland compilers

int createFiles( Code4 &cb )
{
   Field4info fields( cb ) ;

   fields.add( "NAME", 'C', 20 ) ;
   fields.add( "AGE", 'N', 3 ) ;
   fields.add( "BIRTHDATE", 'D' ) ;

   Tag4info tags ( cb) ;
   tags.add( "INF1_NME", "NAME" ) ;

   cb.safety = 0 ; // Turn off safety -- overwrite files
   Data4 data ;
   data.create( cb, "INFO1.DBF", fields.fields( ), tags.tags( ) ) ;

   return cb.errorCode ;

}

void main()
{
   Code4 code ;
   createFiles( code ) ;
   code.initUndo( ) ;
}
