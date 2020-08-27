using System ;
using CodeBase ;

namespace c4ap
{
   class ex54
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4() ;

         cb.accessMode = Code4.OPEN4DENY_RW ; // open file exclusively

         data.open( ref cb, "INFO" ) ;
         cb.exitTest( ) ;
         data.lockAll( ) ;
         cb.optStart( ) ;

         // Mark every other record for deletion
         for( data.top( ) ; data.eof( ) == 0 ; data.skip( 2 ) )
            data.deleteRec( ) ;

         // Remove the deleted records from the physical disk
         data.pack( ) ;

         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}