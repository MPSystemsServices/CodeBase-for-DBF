using System ;
using CodeBase ;

namespace c4ap
{
   class ex141
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4() ;  // Do not open the file with the constructor

         cb.accessMode = Code4.OPEN4DENY_RW ;
         int rc = data.open( ref cb, "INFO" ) ;

         if( rc == Code4.r4success )
         {
            Console.Write( "Data file INFO.DBF has " + data.recCount( ) ) ;
            Console.WriteLine( " records. " ) ;
         }
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}