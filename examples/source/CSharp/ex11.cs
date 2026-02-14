using System ;
using CodeBase ;

namespace c4ap
{
   class ex11
   {
      static int itemsToRestock( ref Code4 cb )
      {
         short oldOpt = cb.optimize ; // save old optimization setting.
         short oldExcl = cb.accessMode ;
         cb.optimize = Code4.OPT4EXCLUSIVE ;
         cb.accessMode = Code4.OPEN4DENY_RW ;//open files exclusively

         Data4 inventory = new Data4( cb, "INVENT.DBF" ) ; // Read optimized
         Field4 minOnHand = new Field4( inventory, "MIN_ON_HND" ) ;
         Field4 onHand = new Field4( inventory, "ON_HAND" ) ;
         Field4 stockName = new Field4( inventory, "ITEM" )  ;
         int count = 0 ;

         if ( cb.errorCode >= 0 )
         {
            cb.optStart( ) ;

            for( inventory.top( ) ; inventory.eof( ) == 0 ; inventory.skip( ) )
               if( onHand.getInt() < minOnHand.getInt() )
                  count++ ;
            Console.WriteLine( count + " items must be restocked" ) ;
         }
         cb.optSuspend( ) ;
         cb.optimize = oldOpt ;
         cb.accessMode = oldExcl ;
         inventory.close( ) ;
         return count ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 codebase = new Code4() ;
         
         itemsToRestock( ref codebase ) ;
         codebase.initUndo() ;
      }

   }
}