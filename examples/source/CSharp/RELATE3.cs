using System;
using CodeBase;

namespace c4ap
{
	class RELATE3
	{
		static Code4 codeBase = new Code4() ;
		static Data4 student = new Data4() ;
		static Data4 enrolment = new Data4() ;
		static Data4 classes = new Data4() ;
		static Field4 studentId ; 
		static Field4 firstName ; 
		static Field4 lastName ;
		static Field4 age ;
		static Field4 classCode ;
		static Field4 classTitle ;
		static Tag4 idTag = new Tag4() ;
		static Tag4 codeTag = new Tag4();
		static Relate4set classRel = new Relate4set() ;
		static Relate4 studentRel = new Relate4() ;
		static Relate4 enrollRel = new Relate4() ;

		static void openDataFiles()
		{
			student.open( ref codeBase, "student" ) ;
			enrolment.open( ref codeBase, "enroll" ) ;
			classes.open( ref codeBase, "classes" ) ;
			studentId = new Field4( student, "ID" ) ;
			firstName = new Field4( student, "F_NAME" ) ;
			lastName = new Field4( student, "L_NAME" ) ;
			age = new Field4( student, "AGE" ) ;
			classCode = new Field4( classes, "CODE" ) ;
			classTitle = new Field4( classes, "TITLE" ) ;

			idTag.init( student, "STU_ID" ) ;
			codeTag.init( enrolment, "ENR_CODE" ) ;

			codeBase.exitTest( ) ;
		}

		static void printStudents( )
		{
			Console.WriteLine("       {0} {1} {2} {3}", 
				firstName.str( ), lastName.str( ), 
				studentId.str( ), age.str( ) ) ;
		}

		static void setRelation( )
		{
			classRel.init( classes ) ;

			enrollRel.init( classRel, enrolment, "CODE", codeTag ) ;

			studentRel.init( enrollRel, student, "STU_ID_TAG", idTag ) ;
		}

		static void printStudentList( string expr, int direction )
		{
			classRel.querySet( expr ) ;
			classRel.sortSet( "student->L_NAME + student->F_NAME" ) ;

			enrollRel.type( Code4.relate4scan ) ;

			int rc, endValue ;
			if( direction > 0 )
			{
				rc = classRel.top( ) ;
				endValue = Code4.r4eof ;
			}
			else
			{
				rc = classRel.bottom( ) ;
				endValue = Code4.r4bof ;
			}
			Console.WriteLine("{0}  {1}", classCode.str( ), classTitle.str( ) ) ;

			for ( ; rc != endValue ; rc = classRel.skip( direction ) )
				printStudents( ) ;

			Console.WriteLine( "\n" ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			openDataFiles( ) ;

			setRelation( ) ;

			printStudentList( "CODE = 'MATH114 '", 1 ) ;

			printStudentList( "CODE = 'CMPT411 '", -1 ) ;

			codeBase.unlock( ) ;
			classRel.free( ) ;

			codeBase.initUndo( ) ;
		}
	}
}
