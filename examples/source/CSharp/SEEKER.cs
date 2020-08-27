using System;
using CodeBase;

namespace c4ap
{
	class SEEKER
	{
		static Code4      codeBase = new Code4() ;
		static Data4      dataFile = new Data4() ;
		static Field4     fName ;
		static Field4	  lName ;
		static Field4	  address ;
		static Field4	  age ;
		static Field4	  birthDate ;
		static Field4	  married ;
		static Field4	  amount ;
		static Field4memo comment ;

		static Tag4 nameTag = new Tag4() ;
		static Tag4 ageTag = new Tag4() ;
		static Tag4 amountTag = new Tag4() ;
		static Tag4 addressTag = new Tag4() ;
		static Tag4 birthdateTag = new Tag4() ;

		static void OpenDataFile( )
		{
			Field4info fieldInfo = new Field4info( ref codeBase ) ;

			fieldInfo.add( "F_NAME", Code4.r4str, 10, 0, 0 ) ;
			fieldInfo.add( "L_NAME", Code4.r4str, 10, 0, 0 ) ;
			fieldInfo.add( "ADDRESS", Code4.r4str, 15, 0, 0 ) ;
			fieldInfo.add( "AGE", Code4.r4num, 2, 0, 0 ) ;
			fieldInfo.add( "BIRTH_DATE", Code4.r4date, 8, 0, 0 ) ;
			fieldInfo.add( "MARRIED", Code4.r4log, 1, 0, 0 ) ;
			fieldInfo.add( "AMOUNT", Code4.r4num, 7, 2, 0 ) ;
			fieldInfo.add( "COMMENT", Code4.r4memo, 10, 0, 0 ) ;

			dataFile.open( ref codeBase, "people.dbf" ) ;

			fName = new Field4( dataFile, "F_NAME") ;
			lName = new Field4( dataFile, "L_NAME") ;
			address = new Field4( dataFile, "ADDRESS") ;
			age = new Field4( dataFile, "AGE") ;
			birthDate = new Field4( dataFile, "BIRTH_DATE") ;
			married = new Field4( dataFile, "MARRIED") ;
			amount = new Field4( dataFile, "AMOUNT") ;
			comment = new Field4memo( dataFile, "COMMENT") ;

			nameTag.init( dataFile, "PPL_NAME" ) ;
			addressTag.init( dataFile, "PPL_ADDR" ) ;
			ageTag.init( dataFile, "PPL_AGE" ) ;
			birthdateTag.init( dataFile, "PPL_BRTH" ) ;
			amountTag.init( dataFile, "PPL_AMNT" ) ;
		}

		static void seekStatus(int rc, ref string status)
		{
			switch(rc)
			{
				case Code4.r4success:
					status = "r4success" ;
					break;

				case Code4.r4eof:
					status = "r4eof" ;
					break;

				case Code4.r4after:
					status = "r4after" ;
					break;

				default:
					status = "other" ;
					break;
			}
		}

		static void printRecord( int rc )
		{
			string bDate ;
			string purchased ;
			string name ;
			string status = "";

			seekStatus( rc, ref status ) ;
			
			purchased = amount.str() ;
			bDate = birthDate.str( ) ;
			name = fName.str( ) ;
			name = name.Trim( ) ;
			name = name + " " + lName.str( ) ;

			Console.WriteLine( "Seek status : {0}", status ) ;
			Console.WriteLine("-------------------------------\nName     : {0}\n" + 
				"Address  : {1}\nAge : {2} Married : {3}\nBirth Date: {4}\n" +
				"Comment: {5}\nPurchased this year:${6}\n", name.Trim(), address.str( ),
				age.str( ), married.str( ), birthDate.str( ), 
				comment.str( ), purchased ) ;
		}

		[STAThread]
		static void Main(string[] args)
		{
			int rc;

			OpenDataFile( ) ;

			dataFile.select( addressTag ) ;
			rc = dataFile.seek( "123 - 76 Ave   ") ;
			printRecord( rc ) ;

			rc = dataFile.seek( "12") ;
			printRecord( rc ) ;

			rc = dataFile.seek( "12             ") ;
			printRecord( rc ) ;

			dataFile.select( birthdateTag) ;
			rc = dataFile.seek( "19581111") ; // October 11, 1958
			printRecord( rc ) ;

			dataFile.select( amountTag) ;
			rc = dataFile.seek( 98.99) ;
			printRecord( rc ) ;

			rc = dataFile.seek( " 98.99") ;
			printRecord( rc ) ;

			//The following code finds all the occurences
			//John Albridge in the tag
			dataFile.select( nameTag) ;
			rc = dataFile.seekNext( "John      Albridge  ") ;

			for ( ; rc == Code4.r4success; rc = dataFile.seekNext("John      Albridge  ") )
			printRecord( rc );

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}