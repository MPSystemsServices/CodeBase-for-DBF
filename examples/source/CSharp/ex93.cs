using System;
using CodeBase;

namespace c4ap
{
   class ex93
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 code = new Code4( ) ;
         Data4 db = new Data4( code, "DATA" ) ;
         Expr4 exp = new Expr4( db, "LNAME=\'Smith\'" ) ; // a logical dBASE expression
         int count = 0 ;
 
         for( db.top( ) ; db.eof( ) == 0 ; db.skip( ) )
            if( exp.isTrue( ) > 0 )
               count++ ;
 
         Console.WriteLine( "{0} record(s) had a last name of Smith\n", count ) ;
 
         code.initUndo( ) ;
      }
   }
}