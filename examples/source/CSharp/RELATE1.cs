using System;
using CodeBase;

namespace c4ap
{
	class RELATE1
	{
		static Code4 codeBase = new Code4( 0 ) ;
		static Data4 student = new Data4() ;
		static Data4 enrolment = new Data4() ;
		static Relate4 slave = new Relate4();
		static Relate4set master = new Relate4set();
		static Tag4 idTag = new Tag4() ;
		static Tag4 nameTag = new Tag4();

		static void openDataFiles( )
		{
			codeBase.init( ) ;

			student.open( ref codeBase,"student" ) ;
			enrolment.open( ref codeBase,"enroll");

			nameTag.init( student,"STU_NAME");
			idTag.init( enrolment,"ENR_ID");

			codeBase.exitTest( ) ;
		}

		static void setRelation()
		{
			master.init( student ) ;
			if( master.isValid( ) == 0) codeBase.exit();
			
			Relate4 slave = new Relate4( master, enrolment, "ID", idTag) ;

			slave.type( Code4.relate4scan ) ;
			master.top( ) ;
			// 'slave' falls out of scope, but the relation remains
		}

		static void printRecord()
		{
			Relate4iterator relation = new Relate4iterator( master) ;
			Data4 data = new Data4() ;

			for( ; relation.isValid( ) != 0; relation.next( ) )
			{
				data = relation.data( ) ;

				for(short j = 1; j <= data.numFields( ); j++ )
				{
					Field4memo field = new Field4memo(data, j) ;
					Console.WriteLine( field.str( ) ) ;
				}
			}
			Console.WriteLine( "\n" ) ;
		}

		static void listRecords()
		{
			int rc;

			for(rc = master.top( ); rc != Code4.r4eof; rc = master.skip( 1 ) )
				printRecord( ) ;

			Console.WriteLine( "\n" ) ;

			codeBase.unlock( ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			openDataFiles( ) ;

			setRelation( ) ;

			listRecords( ) ;

			master.free( ) ;

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}
