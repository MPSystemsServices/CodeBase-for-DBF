using System;
using CodeBase;
 
namespace c4ap
{
   class ex97
   {
      static void showExpr( Expr4 ex )
      {
         Console.WriteLine( "\nValue: " );
 
         switch( ex.type( ) )
         {
            case  Code4.r4date:
            Console.WriteLine( "{0}\nr4date\n", ex.str( ) ) ;
            break ;
            case  Code4.r4dateDoub:
            Console.WriteLine( "{0}\nr4dateDoub\n", (double) ex ) ;
            break ;
            case  Code4.r4log:
            Console.WriteLine( "{0}\nr4log\n", (double) ex ) ;
            break ;
            case  Code4.r4num:
            Console.WriteLine( "{0}\nr4num\n", (double) ex ) ;
            break ;
            case  Code4.r4numDoub:
            Console.WriteLine( "{0}\nr4numDoub\n", (double) ex ) ;
            break ;
            case  Code4.r4str:
            Console.WriteLine( "{0}\nr4str\n", ex.str( ) ) ;
            break ;
            case  Code4.r4memo:
            Console.WriteLine( "{0}\nr4memo\n", ex.str( ) ) ;
            break ;
         }
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 db = new Data4( cb, "info" ) ;
         db.top( ) ;
 
         Expr4 ex = new Expr4( db, "NAME" ) ;
         showExpr( ex ) ;
         ex.free( ) ;
 
         ex.parse( db, "AGE" ) ;
         showExpr( ex ) ;
         ex.free( ) ;
 
         ex.parse( db, "AGE+1" ) ;
         showExpr( ex ) ;
         ex.free( ) ;
 
         ex.parse( db, "BIRTH_DATE" ) ;
         showExpr( ex ) ;
         ex.free( ) ;
 
         ex.parse( db, "BIRTH_DATE+1" ) ;
         showExpr( ex ) ;
         ex.free( ) ;
 
         cb.closeAll( ) ;
         cb.initUndo( ) ;
      }
   }
}