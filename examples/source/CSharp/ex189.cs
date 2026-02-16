using System;
using CodeBase;
 
namespace c4ap
{
   class ex189
   {
      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "DATA1" ) ;
         short j ;
 
         /* list the fields that are character fields */
         for (j = 1 ; j < data.numFields( ) ; j++ )
         {
            Field4 field = new Field4( data, j ) ;
            if ( field.type( ) == Code4.r4str )
               Console.WriteLine( field.name( ) ) ;
         }
 
         cb.initUndo( ) ;
      }
   }
}