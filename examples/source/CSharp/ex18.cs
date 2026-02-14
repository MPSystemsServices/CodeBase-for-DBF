using System ;
using CodeBase ;

namespace c4ap
{
   class ex18
   {
      static void openAFile( ref Code4 cb )
      {
         // 'd' falls out of scope.  Data file is still open
         Data4 d = new Data4( cb, "INFO" ) ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         cb.autoOpen = 0 ;
         openAFile( ref cb ) ;

         Data4 d = new Data4( cb, "DATAFILE" ) ; // open a second file
         Console.WriteLine( "Number of records in DATAFILE: " + d.recCount( ) ) ;

         cb.closeAll( ) ; // INFO and DATAFILE are both closed
         cb.initUndo( ) ;
      }
   }
}