/* ex27.cpp */
#include "d4all.hpp"
#ifdef __TURBOC__
   extern unsigned _stklen = 10000 ; // for all Borland compilers
#endif

void main()
{
   Code4 settings ;

   Data4 info( settings, "DATA2.DBF") ;

   settings.optStart() ;

   Field4 field( info, "NAME") ;
   info.lockAll( ) ; //the record must be locked before a field can be changed
   for( long iRec = 1L ; iRec <= info.recCount() ; iRec++ )
   {
      info.go( iRec ) ;
      field.assign( "New Data" ) ;
   }

   info.close( ) ;
   settings.initUndo( ) ;
   settings.exit( ) ;
}
