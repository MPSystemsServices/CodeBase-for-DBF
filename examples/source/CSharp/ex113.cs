using System;
using CodeBase;
 
namespace c4ap
{
   class ex113
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "DATA3" ) ;
 
         data.top( ) ;
         Field4memo comments = new Field4memo( data, "COMMENTS" ) ;
         data.lockAll( ) ;
         comments.setLen( 0x4000 ) ; // 16K
         comments.assign( "First characters of a 16k memo field" ) ;
 
         // Flush changes to disk and close the data and memo files
         data.close( ) ;
 
         data.open( ref cb, "DATA3" ) ;
         comments = new Field4memo( data, "COMMENTS" ) ;
         data.top( ) ;
 
         Console.WriteLine( "Memo field Value:\n{0}\nLength of memo field: {1}", 
            comments.str( ), comments.len( ) ) ;
 
         data.close( ) ;
         cb.initUndo( ) ;
      }
   }
}