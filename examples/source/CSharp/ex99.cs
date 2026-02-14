using System;
using CodeBase;
 
namespace c4ap
{
   class ex99
   {
      static void dumpDataFileToScreen( Data4 d )
      {
         Console.WriteLine( "Contents of: {0}\n", d.alias ) ;
         for( d.top( ) ; d.eof( ) == 0 ; d.skip( ) )
         {
            for( short j = 1 ; j <=d.numFields( ) ; j++ )
            {
               Field4 field = new Field4( d, j ) ;
               Console.WriteLine( field.str( ) ) ;
            }
            Console.Write("\n") ;
         }
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4(cb, "DATA");
 
         dumpDataFileToScreen( data );
 
         data.close();
         cb.initUndo();
      }
   }
}