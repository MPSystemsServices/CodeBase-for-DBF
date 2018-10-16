#include "d4all.hpp"

#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ;  // for all Borland compilers
#endif

void main( )
{
   Code4 codeBase ;
   codeBase.accessMode = OPEN4DENY_RW ;
   codeBase.safety = 0 ;  // Ensure the create overwrites any existing file

   Field4info fields( codeBase )  ;

   fields.add( "NAME", 'C', 20 ) ;
   fields.add( "AGE", 'N', 3, 0 ) ;
   fields.add( "BIRTHDATE", 'D') ;

   Data4 newDataFile ;
   newDataFile.create( codeBase, "NEWDBF", fields.fields( ) ) ;

   newDataFile.close( ) ;

  // open in shared mode
   codeBase.accessMode = OPEN4DENY_NONE ;
   newDataFile.open( codeBase, "NEWDBF" ) ;

   // ... some other code ...

   codeBase.closeAll( ) ;
   codeBase.initUndo( ) ;
}
