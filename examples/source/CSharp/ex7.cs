using System ;
using CodeBase ;

namespace c4ap
{
   class ex7
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;

         data.go( data.recCount( ) + 1 ) ;

         Console.WriteLine( "An error message was displayed" ) ;   
         cb.errorCode = cb.errGo = 0 ;

         data.go( data.recCount( ) + 1 ) ;
         Console.WriteLine( "No error message was displayed" ) ;

         cb.initUndo( ) ;
      }
   }
}