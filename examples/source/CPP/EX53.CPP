//ex53.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ;  // for all Borland compilers

void main( )
{
   Code4 cb ;
   Data4 data( cb, "INFO" ) ;

   data.optimizeWrite( OPT4ALL ) ;
   // when doing write optimization on shared files, it is necessary to
   // lock the file, preferably with Data4::lockAll( )
   data.lockAll( ) ;

   cb.optStart( ) ; // begin optimization

   long age = 20 ;
   Field4 ageField( data, "AGE") ;

   for( data.top( ) ;  age < 65 ; data.append( ) )
   {
      data.appendStart( ) ;
      ageField.assignLong( age++) ;
   }

   cb.initUndo( ) ; // flushes, closes, and unlocks
}
