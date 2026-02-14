using System ;
using CodeBase ;

namespace c4ap
{
   class ex21
   {
      static void exitToSystem( ref Code4 cb )
      {
         Console.WriteLine( "Shutting down application ... " ) ;
         cb.closeAll( ) ;
         cb.exit( ) ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         exitToSystem( ref cb ) ;
      }
   }
}