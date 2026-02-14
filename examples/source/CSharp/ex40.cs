using System ;
using CodeBase ;

namespace c4ap
{
   class ex40
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4() ;
         Data4 infoFile = new Data4( settings, "INFO" ) ;

         // Go to the end of file and set the End of file flag
         infoFile.goEof( ) ;

         // Check to see if the end of file flag is set
         if ( infoFile.eof( ) != 0  )
         {
            Console.WriteLine( "This is always true" ) ;
            infoFile.bottom( ) ; // reset the eof flag
         }

         infoFile.close( ) ;
         settings.initUndo( ) ;
      }
   }
}