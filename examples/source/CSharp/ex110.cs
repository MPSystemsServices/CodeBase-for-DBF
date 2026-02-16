using System;
using CodeBase;
 
namespace c4ap
{
   class ex110
   {
      static void dumpDataFileToScreen( Data4 d )
      {
         // dump all fields -- including memo fields -- to screen
         Console.WriteLine( "Contents of: {0}", d.alias ) ;
         for( d.top( ) ; d.eof( ) == 0 ; d.skip( ) )
         {
            for( short j = 1 ; j <= d.numFields( ) ; j++ )
            {
               Field4memo field = new Field4memo( d, j ) ;
               Console.WriteLine( field.str( ) ) ;
            }
            Console.WriteLine( );
         }
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         cb.readOnly = 1 ;
         Data4 data = new Data4( cb, "INFO" ) ;
         dumpDataFileToScreen( data ) ;
         cb.initUndo( ) ;
      }
   }
}