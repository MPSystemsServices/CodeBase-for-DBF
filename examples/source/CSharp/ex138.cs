using System;
using CodeBase;
 
namespace c4ap
{
   class ex138
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         cb.autoOpen = 0 ; // don't automatically open index file
         Data4 data = new Data4( cb, "INFO" ) ;
 
         Index4 index = new Index4( ) ;
         index.open( data, "INFO2" ) ; // open a secondary index file
 
         cb.lockAttempts = Code4.WAIT4EVER ; // wait until the lock succeeds
         data.lockAll( ) ;
         if( index.reindex( ) == 0 )
            Console.WriteLine( "Reindexed successfully" ) ;
 
         cb.closeAll( ) ;
         cb.initUndo( ) ;
      }
   }
}