using System ;
using CodeBase ;

namespace c4ap
{
   class ex2
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4();

         // Do not automatically open production index file
         cb.autoOpen = 0 ;
         Data4 info = new Data4( cb, "INFO.DBF" ) ;
         Index4 infoIndex = info.index("INFO") ;

         if( infoIndex.isValid() == 0 )
            Console.WriteLine( "Production index file is not opened" ) ;

         // DATA.DBF has a production index.  Open it
         cb.autoOpen = 1 ;
         Data4 data = new Data4( cb, "DATA.DBF" ) ;

         //  Some other code

         cb.closeAll( ) ;
         cb.initUndo( ) ;
      }
   }
}