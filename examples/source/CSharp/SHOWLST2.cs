using System;
using CodeBase;

namespace c4ap
{
	class SHOWLST2
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
		static Tag4		  nameTag = new Tag4() ;
		static Tag4		  ageTag = new Tag4() ;
		static Tag4		  amountTag = new Tag4() ;
		static Tag4		  addressTag = new Tag4() ;
		static Tag4		  birthdateTag = new Tag4() ;

		static void OpenDataFile( )
		{
			Tag4info tagInfo = new Tag4info( codeBase ) ;

			tagInfo.add( "PPL_NAME", "F_NAME + L_NAME", ".NOT. DELETED()", 0, 0 ) ;
			tagInfo.add( "PPL_ADDR", "ADDRESS", "", 0, 0 ) ;
			tagInfo.add( "PPL_BRTH", "BIRTH_DATE", "AGE >= 18", 0, 0 ) ;
			tagInfo.add( "PPL_AGE", "AGE", "", 0, 0 ) ;
			tagInfo.add( "PPL_AMNT", "AMOUNT", "", 0, 0 ) ;

			codeBase.accessMode = Code4.OPEN4DENY_RW ;
			codeBase.autoOpen = 0;
			codeBase.safety = 0;

			dataFile.open( ref codeBase, "people.dbf" ) ;

			Index4 index = new Index4() ;
			index.create( ref dataFile, "SHOWLST2", ref tagInfo ) ;

			fName = new Field4( dataFile, "F_NAME") ;
			lName = new Field4( dataFile, "L_NAME") ;
			address = new Field4( dataFile, "ADDRESS") ;
			age = new Field4( dataFile, "AGE") ;
			birthDate = new Field4( dataFile, "BIRTH_DATE") ;
			married = new Field4( dataFile, "MARRIED") ;
			amount = new Field4( dataFile, "AMOUNT") ;
			comment = new Field4memo( dataFile, "COMMENT") ;

			nameTag.init( dataFile, "PPL_NAME") ;
			addressTag.init( dataFile, "PPL_ADDR" ) ;
			ageTag.init( dataFile, "PPL_AGE") ;
			birthdateTag.init( dataFile, "PPL_BRTH" ) ;
			amountTag.init( dataFile, "PPL_AMNT") ;
		}

		static void PrintRecords( )
		{
			string bDate ;
			string purchased ;
			string name ;
			
			for( int rc = dataFile.top( ); rc == Code4.r4success; rc = dataFile.skip( ))
			{
				purchased = amount.str() ;
				bDate = birthDate.str( ) ;
				name = fName.str( ) ;
				name = name.Trim( ) ;
				name = name + " " + lName.str( ) ;

				Console.WriteLine("\t\t-------------------------------\n\t\tName     : {0}\n" + 
					"\t\tAddress  : {1}\n\t\tAge : {2} Married : {3}\n\t\tBirth Date: {4}\n" +
					"\t\tComment: {5}\n\t\tPurchased this year:${6}", name.Trim(), address.str( ),
					age.str( ), married.str( ), birthDate.str( ), 
					comment.str( ), purchased ) ;
			}
		}

		[STAThread]
		static void Main(string[] args)
		{
			OpenDataFile( ) ;

			dataFile.select( nameTag ) ;
			Console.WriteLine( "Order by Name" ) ;
			PrintRecords( ) ;
			Console.ReadLine( ) ;			

			dataFile.select( ageTag ) ;
			Console.WriteLine( "Order by Age" ) ;
			PrintRecords( ) ;
			Console.ReadLine( ) ;

			dataFile.select( amountTag ) ;
			Console.WriteLine( "Order by Amount" ) ;
			PrintRecords( ) ;
			Console.ReadLine( ) ;

			codeBase.closeAll( ) ;
			codeBase.initUndo( ) ;
		}
	}
}