using System;
using CodeBase;

namespace c4ap
{
   class ex94
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 db = new Data4( cb, "INFO" ) ;
         Expr4 expr = new Expr4( db, "AGE" ) ;
 
         double VOTE_AGE = 18.0 ;
         long count = 0 ;
         for( int rc = db.top( ) ; rc != Code4.r4eof ; rc = db.skip( ) )
            if( (double) expr >= VOTE_AGE )
               count ++ ;
 
         Console.WriteLine( "Possible voters: {0}\n", count ) ;
            
         expr.free( ) ;
         cb.initUndo( ) ;
      }
   }
}