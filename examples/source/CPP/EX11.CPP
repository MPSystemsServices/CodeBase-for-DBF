//ex11.cpp
#include "d4all.hpp"
extern unsigned _stklen = 10000 ; // for all Borland compilers

int itemsToRestock( Code4 &cb )
{

   int oldOpt = cb.optimize ; // save old optimization setting.
   int oldExcl = cb.accessMode ;
   cb.optimize = OPT4EXCLUSIVE ;
   cb.accessMode = OPEN4DENY_RW ;//open files exclusively

   Data4 inventory( cb, "INVENT.DBF" ) ; // Read optimized
   Field4 minOnHand( inventory, "MIN_ON_HND" ) ;
   Field4 onHand( inventory, "ON_HAND" ) ;
   Field4 stockName( inventory, "ITEM" )  ;
   int count = 0 ;

   if( cb.errorCode >= 0 )
   {
      cb.optStart( ) ;

      for( inventory.top( ) ; ! inventory.eof( ) ; inventory.skip( ) )
         if( (long) onHand < (long) minOnHand )
           count++ ;
      cout << count << " items must be restocked" << endl ;
   }
   cb.optSuspend( ) ;
   cb.optimize = oldOpt ;
   cb.accessMode = oldExcl ;
   inventory.close( ) ;
   return count ;
}

void main()
{
   Code4 codebase ;
   itemsToRestock( codebase ) ;
   codebase.initUndo() ;
}
