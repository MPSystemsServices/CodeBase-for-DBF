using System ;
using CodeBase ;

namespace c4ap
{
   class ex46
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4() ;

         data.open( ref cb, "INFO" ) ;

         cb.lockAttempts = 4 ; // Try four times

         int rc = data.lockRecord( 5 ) ;
         if( rc == Code4.r4success )
            Console.WriteLine( "Record 5 is now locked." ) ;
         else if( rc == Code4.r4locked )
            Console.WriteLine( "Record 5 is locked by another user" ) ;

         cb.lockAttempts = Code4.WAIT4EVER ; // Try forever
         rc = data.lockRecord( 5 ) ;

         if ( rc == Code4.r4success )
            Console.WriteLine( "This will always happen." ) ;

         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}