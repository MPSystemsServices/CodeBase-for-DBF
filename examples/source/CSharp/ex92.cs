using System;
using CodeBase;

namespace c4ap
{
   class ex92
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4  cb = new Code4( ) ;
         Data4 data = new Data4( cb, "DATA" ) ;
 
         data.go( 1 ) ;
 
         // "FNAME" and "LNAME" are Character field names of data file "DATA.DBF"
         Expr4 expr = new Expr4( data, "FNAME+\' \'+LNAME") ;
 
         Console.WriteLine( "FNAME and LNAME for Record One: {0}\n", expr.str( ) ) ;
 
         expr.free( ) ;
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}