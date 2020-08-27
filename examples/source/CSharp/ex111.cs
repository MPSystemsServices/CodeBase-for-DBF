using System;
using CodeBase;
 
namespace c4ap
{
   class ex111
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb  = new Code4( ) ;
         Data4 data = new Data4( cb, "DATA3" ) ;
         Field4memo comments = new Field4memo( data, "COMMENTS" ) ;
 
         cb.exitTest( ) ;
         int count = 0 ;
         for( data.top( ) ; data.eof( ) == 0 ; data.skip( ) )
            if( comments.len( ) > 0 )
               count ++ ;
 
         Console.WriteLine( "There were {0} memo entries out of {1} records.", count, data.recCount( ) ) ;
         cb.initUndo( ) ;
      }
   }
}