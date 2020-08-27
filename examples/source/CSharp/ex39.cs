using System ;
using CodeBase ;

namespace c4ap
{
   class ex39
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4() ;

         cb.accessMode = Code4.OPEN4DENY_RW ; // open file exclusively to speed pack

         data.open( ref cb, "INFO" ) ;
         cb.exitTest( ) ;
         cb.optStart( ) ;

         for( data.top( ) ; data.eof( ) == 0 ; data.skip( 2 ) )
            data.deleteRec( ) ;  // Mark the record for deletion

         data.pack( ) ; // Physically remove the deleted records from the disk

         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}