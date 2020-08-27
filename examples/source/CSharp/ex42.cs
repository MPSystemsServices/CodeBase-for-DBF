using System ;
using CodeBase ;

namespace c4ap
{
   class ex42
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         cb.accessMode = Code4.OPEN4DENY_RW ;
         Data4 data = new Data4( cb, "INFO" );
         Field4 age = new Field4( data, "AGE" ) ;

         data.go( 2 ) ;
         age.assignInt( 49 ) ;

         // Explicitly flush the change to disk in case power goes out
         data.flush( ) ;

         // some other code

         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}