using System ;
using CodeBase ;

namespace c4ap
{
   class ex24
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 code = new Code4() ;
         code.accessMode = Code4.OPEN4DENY_RW ;

         Data4 dataFile = new Data4( code, "INFO" ) ;
         code.exitTest( ) ;

         // initialize optimization with default settings.
         code.optStart( ) ;
         int delCount = 0 ;
         for ( int rc = dataFile.top( ); rc == Code4.r4success ; rc = dataFile.skip( ) )
            if( dataFile.deleted( ) != 0 )
               delCount++ ;

         Console.WriteLine( delCount + " records are marked for deletion." ) ;
         code.initUndo( ) ;
      }
   }
}