using System ;
using CodeBase ;

namespace c4ap
{
   class ex47
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 code = new Code4() ;
         Data4 df = new Data4( code, "INFO" ) ;

         int rc = df.lockAll( ) ;

         if( rc == Code4.r4success )
            Console.WriteLine( "Lock is successful." ) ;
         else
            Console.WriteLine( "Lock was unsuccessful" ) ;

         code.initUndo( ) ;
      }
   }
}