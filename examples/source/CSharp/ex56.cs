using System ;
using CodeBase ;

namespace c4ap
{
   class ex56
   {
      static int recallAll( Data4 d )
      {

         Tag4 saveSelected = new Tag4() ;
         saveSelected.initSelected( d ) ;

         d.select( ) ; // use record ordering

         d.lockAll( ) ;

         int count = 0 ;

         for( d.top( ) ; d.eof( ) != 0 ; d.skip( 1 ) )
         {
            d.recall( ) ;
            count++ ;
         }

         d.select( saveSelected ) ; // reset the selected tag.
         return count ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4() ;
         Data4 data = new Data4( cb, "INFO" ) ;
         recallAll( data ) ;
         cb.initUndo( ) ;
      }
   }
}