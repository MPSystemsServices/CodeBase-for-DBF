using System;
using CodeBase;

namespace c4ap
{
	class RELATE3
	{
		static Code4 codeBase = new Code4( 0 ) ;
		static Data4 student = new Data4() ;
		static Data4 enrolment = new Data4() ;
		static Field4 id ; 
		static Field4 fName ; 
		static Field4 lName ;
		static Field4 age ;
		static Field4 cCode ;
		static Relate4 slave = new Relate4() ;
		static Relate4set master = new Relate4set() ;
		static Tag4 idTag = new Tag4() ;
		static Tag4 nameTag = new Tag4() ;

		static void openDataFiles()
		{
			codeBase.init( ) ;

			student.open( ref codeBase,"student" ) ;
			enrolment.open( ref codeBase,"enroll");

			id = new Field4( student,"ID");
			fName = new Field4( student,"F_NAME");
			lName = new Field4( student,"L_NAME");
			age = new Field4( student,"AGE");
			cCode = new Field4( enrolment,"C_CODE_TAG");

			nameTag.init( student,"STU_NAME");
			idTag.init( enrolment,"ENR_ID");

			codeBase.exitTest( ) ;
		}

		static void setRelation()
		{
			master.init( student ) ;
			slave.init( master, enrolment, "ID", idTag);
		}

		static void seek( Data4 dataFile, Tag4 tag, Relate4set relation, string key)
		{
			Tag4 oldTag = new Tag4() ;

			oldTag.initSelected( dataFile ) ;
			dataFile.select( tag ) ;

			dataFile.seek( key ) ;
			relation.doAll( ) ;

			dataFile.select( oldTag ) ;
		}

		static void printRecord()
		{
			Console.WriteLine("{0} {1} {2} {3} {4}", 
				fName.str( ), lName.str( ), id.str( ), 
				age.str( ), cCode.str( ));
		}

		[STAThread]
		static void Main(string[] args)
		{
			openDataFiles( ) ;

			setRelation( ) ;

			seek( student
				, nameTag
				, master, "Tyler           Harvey         ") ;

			printRecord( ) ;

			seek( student
				, nameTag
				, master, "Miller          Albert        ");

			printRecord( ) ;

			codeBase.unlock( ) ;
			master.free( ) ;

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}