using System;
using CodeBase;
 
namespace c4ap
{
   class ex107
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Field4info fields = new Field4info( ref cb ) ; // initialize with no fields
 
         fields.add( "NAME", 'C', 20, 0, 0 ) ;
         fields.add( "AGE",  'N',  3, 0, 0 ) ;
         fields.add( "REGISTER", 'L', 1, 0, 0 ) ;
         fields.add( "birth", 'D', 8, 0, 0 ) ;
 
         // change your mind and delete them all
         if( fields.del( "nome   " ) < 0 )
            Console.WriteLine( "This will always happen" ) ;
         fields.del( "name" ) ;
         fields.del( 0 ) ; // delete the 'AGE' field
         fields.del( 0 ) ; // delete the 'REGISTER' field
 
         fields.free( ) ; // delete all remaining
 
         cb.initUndo( ) ;
      }
   }
}