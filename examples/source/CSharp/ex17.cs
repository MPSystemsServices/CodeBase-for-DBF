using System ;
using CodeBase ;

namespace c4ap
{
   class ex17
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 db = new Data4( cb, "DATA" ) ;
         db.top( ) ;

         Expr4 ex = new Expr4( db, "TRIM(LNAME)+', '+TRIM(FNAME)" ) ;
         cb.calcCreate( ex, "NAMES" ) ;
         Console.WriteLine( ex.str( ) ) ;

         Expr4 ex2 = new Expr4() ;
         ex2.parse( db, "'HELLO '+NAMES()" ) ; // no space in dBASE function calls.

         Console.WriteLine( ex2.str( ) ) ;

         ex2.free( ) ;
         cb.calcReset( ) ;
         cb.initUndo( ) ;
      }
   }
}