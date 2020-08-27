using System ;
using CodeBase ;

namespace c4ap
{
   class ex5
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 code = new Code4() ;
         Data4 data = new Data4( code, "INFO" ) ;

         string badExpr = "NAME = 5" ;

         Expr4 expression = new Expr4( data, badExpr ) ;

         Console.WriteLine( "\nAn error message just displayed" ) ;

         code.errorCode = code.errExpr = 0 ;

         expression.parse( data, badExpr ) ;
         Console.WriteLine( "No error message displayed." ) ;

         if ( expression.isValid() != 0 )
            expression.free( ) ;

         code.initUndo( ) ;
      }
   }
}