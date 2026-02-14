using System;
using CodeBase;

namespace c4ap
{
   class ex64
   {
      static int SeekSeries( Data4 d, string s )
      {
         int rc ;
         rc = d.seekNext( s ) ;

         if( rc == Code4.r4noTag || rc == Code4.r4entry || rc == Code4.r4locked || rc < 0 )
            return rc ;

         if( rc == Code4.r4after || rc == Code4.r4eof )
            rc = d.seek( s ) ;

         return rc ;
      }

      [STAThread]
      static void Main(string[] args)
      {
         Code4 cb = new Code4( ) ;
         Data4 data = new Data4( cb, "PEOPLE" ) ;
         Tag4 nametag = new Tag4( data, "PPL_NAME" ) ;

         data.select( nametag ) ;
         int rc = data.seek( "mickey" ) ;
         for ( ; rc == Code4.r4success ; rc = SeekSeries( data, "mickey" ) )
            Console.WriteLine( "found search string\n" ) ;

         cb.initUndo( ) ;
      }
   }
}