using System ;
using CodeBase ;

namespace c4ap
{
   class ex44
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 settings = new Code4() ;
         Data4 data = new Data4( settings, "INFO" ) ;

         // Since Code4::autoOpen is by default TRUE, the INFO index file
         // should have been opened.

         Index4 index = new Index4() ;

         index = data.index( "INFO" ) ;

         if( index.isValid( ) != 0 )
            Console.WriteLine( "INDEX: INFO has been opened" ) ;

         index = data.index( "JUNK" ) ;

         if ( index.isValid( ) == 0 )
            Console.WriteLine( "INDEX: JUNK has not been opened" ) ;

         data.close( ) ;
         settings.initUndo( ) ;
      }
   }
}