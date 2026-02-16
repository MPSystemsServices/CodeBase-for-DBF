using System ;
using CodeBase ;

namespace c4ap
{
   class ex26
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 info = new Data4( cb, "INFO" ) ;
         Data4 data = new Data4( cb, "DATAFILE" ) ;

         info.lockRecord( 1 ) ;
         data.lockAll( ) ;

         if ( cb.unlock() == Code4.r4success )
            Console.WriteLine( "Successfully unlocked" ) ;

         cb.initUndo( ) ;
      }
   }
}