using System ;
using CodeBase ;

namespace c4ap
{
   class ex6
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 code = new Code4() ;
         Data4 data = new Data4( code, "INFO" ) ;
         string badField = "notAField" ;

         Field4 field = new Field4( data, badField ) ;
         Console.WriteLine( "\nAn error message just displayed" ) ;

         code.errorCode = code.errFieldName = 0 ;

         field.init( data, badField ) ;
         Console.WriteLine( "No error message displayed." ) ;

         code.initUndo( ) ;
      }
   }
}