using System;
using CodeBase;
 
namespace c4ap
{
   class ex143
   {
      static void displayRelationTree( Relate4 relate )
      {
         int pos = 0 ;
         Relate4iterator tree = new Relate4iterator( relate ) ;
 
         for(; tree.isValid( ) != 0; )
         {
            for( int i = pos; i > 0; i-- )
               Console.Write( "   " );
            Console.Write( "{0}\n", tree.data( ).fileName( ) ) ;
 
            pos +=  tree.nextPosition( ) ;
         }
      }
 
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 master = new Data4( cb, "M1" ) ;
         Data4 sl1 = new Data4( cb, "SL1" ) ;
         Data4 sl2 = new Data4( cb, "SL2" ) ;
         Data4 sl3 = new Data4( cb, "SL3" ) ;
         Data4 sl4 = new Data4( cb, "SL4" ) ;
 
         Relate4set MasterRelation = new Relate4set( master ) ;
 
         // create the tree
         Relate4 relate1 = new Relate4( MasterRelation, sl1, "TOSL1", new Tag4( sl1, "FRM" ) ) ;
         Relate4 relate2 = new Relate4( MasterRelation, sl2, "TOSL2", new Tag4( sl2, "FRM") ) ;
         Relate4 relate3 = new Relate4( relate2, sl3, "TOSL3", new Tag4( sl3, "FRMSL2" ) ) ;
         Relate4 relate4 = new Relate4( MasterRelation, sl4, "TOSL4", new Tag4( sl4, "FRM" ) ) ;
         cb.exitTest( ) ;
 
         MasterRelation.top( ) ;
         displayRelationTree( MasterRelation ) ;
 
         MasterRelation.free( 1 ) ;
         cb.initUndo( ) ;
      }
   }
}