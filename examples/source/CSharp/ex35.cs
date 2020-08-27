using System ;
using CodeBase ;

namespace c4ap
{
   class ex35
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         data.top() ;
         Console.WriteLine( "Changed status: " + data.changed ) ;  // Displays 0
         data.lockAll( ) ;
         new Field4( data, 1 ).assign( "TEMP DATA" ) ;
         Console.WriteLine( "Changed status: " + data.changed ) ;  // Displays 1
         data.changed = 0 ;

         data.close( ) ;
         // The top record is not flushed.
         cb.initUndo( ) ;
      }
   }
}