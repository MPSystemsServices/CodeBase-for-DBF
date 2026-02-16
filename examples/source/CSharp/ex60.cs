using System ;
using CodeBase ;

namespace c4ap
{
   class ex60
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 codeBase = new Code4() ;
         Data4 dataFile = new Data4( codeBase, "INFO" ) ;

         dataFile.optimize( Code4.OPT4ALL ) ;
         codeBase.optStart( ) ;

         dataFile.top( ) ;
         Console.WriteLine( "Press Enter when you want to refresh your data." ) ;
         Console.Read() ;

         dataFile.refresh( ) ;
         dataFile.top( ) ;  // re-read the record from disk.

         Console.Write( "The latest information is: " ) ;
         for ( short i = 1 ; i <= dataFile.numFields() ; i++ )
            Console.Write( new Field4( dataFile, i ).str() + " " ) ;
         Console.WriteLine() ;

         dataFile.close( ) ;
         codeBase.initUndo( ) ;
      }
   }
}