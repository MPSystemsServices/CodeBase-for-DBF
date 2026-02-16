using System ;
using CodeBase ;

namespace c4ap
{
   class ex34
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         data.bottom( ) ;
         Console.WriteLine( "Last Name added: " + new Field4( data, "NAME" ).str() ) ;

         data.close() ;
         cb.initUndo() ;
      }
   }
}