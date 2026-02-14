/* FILE source5.cpp
(c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "oledb5.hpp"
#include "stamp5.hpp"
//CJ -02/08/00- Moved Customer stamp to own file to facilitate other applications
#include "s4stamp.cpp"

#ifdef S4SERVER
   int S4FUNCTION server4securityOption( void )
   {
      return(CustomerStamp.securityOption);
   }



   char *S4FUNCTION server4CustomerFromStamp( void )
   {
      return(CustomerStamp.customerName);
   }

   char *S4FUNCTION server4SerialFromStamp( void )
   {
      return(CustomerStamp.serialNumber);
   }
#endif /* S4SERVER */

#ifdef OLEDB5BUILD

#ifdef S4MAX
   #define S4MAX_OR_SERVER
#else
   #ifdef S4SERVER
      #define S4MAX_OR_SERVER
   #else
      #ifdef S4TRACK_MEMORY
         #define S4MAX_OR_SERVER
      #endif
   #endif
#endif

#ifdef S4MAX_OR_SERVER
   extern long mem4allocated ;
#endif

#ifndef S4SERVER
   LPMALLOC pIMalloc5 = (LPMALLOC) 0 ;
#endif
extern LONG g_cObj ;

#ifdef S4STAND_ALONE
   char *Copyright ;
#endif

Source5all::Source5all()
{
   c4 = 0 ;
   #ifndef S4CLIENT
      tablesSchema = 0 ;
      indexesSchema = 0 ;
      columnsSchema = 0 ;

      long dwswitch = u4switch() ;
      if ( dwswitch & 1L )
         s5fox = TRUE ;
      if ( dwswitch & 2L )
         s5clipper = TRUE ;
      if ( dwswitch & 4L )
      {
         s5mdx = TRUE ;
         s5hasDescending = FALSE ;
      }
   #endif
   #ifndef S4SERVER
      catalog = 0 ;
   #endif
   int doDebugSpecial = 1 ;
   if ( doDebugSpecial == 0 )
      memset( 0, 0, 1 ) ;
}

#ifndef S4SERVER
   // Source5::Source5 --------------------------------------------------
   //
   // Constructor for this class
   //
   Source5::Source5( LPUNKNOWN pUnkOuterIn )
   {
      initialized = FALSE ;
      session = 0 ;

      #ifndef S4CLIENT
         long dwswitch = u4switch() ;
         if ( dwswitch & 1L )
            s5fox = TRUE ;
         if ( dwswitch & 2L )
            s5clipper = TRUE ;
         if ( dwswitch & 4L )
         {
            s5mdx = TRUE ;
            s5hasDescending = FALSE ;
         }
      #endif

      #ifndef S4SERVER
         pUnkOuter = pUnkOuterIn ;
         sourceAgg = new Source5interface( this ) ;
      #endif
      InterlockedIncrement(&g_cObj) ;
   }

   // Source5::~Source5 -------------------------------------------------
   //
   // Destructor for this class
   //
   Source5::~Source5()
   {
      if (session)
         session->Release() ;
      Uninitialize() ;
      if ( sourceAgg )
      {
         delete sourceAgg ;
         sourceAgg = NULL ;
      }
      InterlockedDecrement(&g_cObj) ;
      // AS Jan 9/02 - Improved the ole-debug logging code, now requires an explicit close...
      #ifdef S4OLEDEBUG_PRINT
         log5closeFile();
      #endif
      return ;
   }

   // Source5::operator delete----------------------------------------------
   //
   //  Delete operator for the Source5 object
   //
   void Source5::operator delete( void *p )
   {
      free5(p) ;
      #ifndef S4JOINT_OLEDB_DLL
         mem4reset() ;
      #endif
   }

   // Source5::init --------------------------------------------------------
   //
   // Initialize the Source5 Object
   //
   void Source5::init()
   {
      properties.init() ;
   }



   // Source5::QueryInterface -----------------------------------------------
   // IUnknown method
   //
   // Returns a pointer to a specified interface. Callers use
   // QueryInterface to determine which interfaces the called object
   // supports.
   //
   STDMETHODIMP Source5::QueryInterface( REFIID riid, LPVOID *ppv )
   {
      #ifdef S4OLEDEBUG_PRINT
         log5query( "Source5::QueryInterface()" ) ;
         out5riidInfo( riid ) ;
         log5query( "\r\n" ) ;
      #endif

      // Is the pointer bad?
      if ( ppv == 0 )
         return  E_INVALIDARG ;

      //  Place NULL in *ppv in case of failure
      *ppv = 0 ;

      if ( riid == IID_IDBInitialize )
         *ppv = (LPVOID)(IDBInitialize *)this ;
      else if ( riid == IID_IPersist )
         *ppv = (LPVOID)(IPersist *)this ;
      else if ( riid == IID_IDBProperties )
         *ppv = (LPVOID)(IDBProperties *)this ;
      else if ( riid == IID_IDBInfo  )
      {
         // AS 11/21/00 - conformance tests - don't support this unless initialized
         if (!initialized)
            return  E_UNEXPECTED ;
         *ppv = (LPVOID)(IDBInfo *)this ;
      }
      else if ( riid == IID_ISupportErrorInfo )
         *ppv = (LPVOID) (ISupportErrorInfo *) this ;
      else if ( riid == IID_IDBCreateSession )
      {
         // if it makes it this far, it had better be initialized ore return E_UNEXPECTED
         if (!initialized)
            return  E_UNEXPECTED ;
         *ppv = (LPVOID) (IDBCreateSession *)this ;
      }
      if ( *ppv == 0 )
      {
         if (pUnkOuter)
         {
            if ( riid == IID_IUnknown )
            {
               if ( sourceAgg->notSetInterface == 1 )
                  *ppv = (LPVOID) (Unknown5 *) sourceAgg ;
               else
                  return ( pUnkOuter->QueryInterface( riid, ppv) ) ;
               AddRef() ;
               return S_OK ;
            }
            else
            {
               if ( sourceAgg->notSetInterface != 1 )
                  return ( pUnkOuter->QueryInterface( riid, ppv ) ) ;
            }
         }
         else
         {
            if (riid == IID_IUnknown )
               return ( Unknown5::QueryInterface( riid, ppv ) ) ;
         }
         /* AS for conformance testing, must return bad RIID before initialization failure */
         HRESULT ret = ( Unknown5::QueryInterface( riid, ppv ) ) ;
         // if it makes it this far, it had better be initialized ore return E_UNEXPECTED
         if (!initialized && ret == S_OK )
            return E_UNEXPECTED ;
         return ret ;
      }

      sourceAgg->notSetInterface = 0 ;
      AddRef() ;
      return S_OK ;
   }

   // Source5::AddRef -------------------------------------------------------
   // IUnknown method
   //
   // Increments a persistence count for the object
   //
   STDMETHODIMP_(ULONG) Source5::AddRef()
   {
      if (sourceAgg->notSetInterface == 0 && pUnkOuter)
         pUnkOuter->AddRef() ;

      return Unknown5::AddRef() ;
   }

   STDMETHODIMP_(ULONG) Source5::Release()
   {
      if (sourceAgg->notSetInterface == 0 && pUnkOuter)
         pUnkOuter->Release();

      ref-- ;
      if ( ref == 0 )
      {
         delete this ;
         return 0 ;
      }
      return ref ;
   }

   STDMETHODIMP Source5::CreateSession( IUnknown *pUnkOuterIn, REFIID riid, IUnknown **ppDBSession )
   {
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::CreateSession()" ) ;
         out5riidInfo( riid ) ;
         log5( "\r\n" ) ;
      #endif

      // check in-params and NULL out-params in case of error
      if ( ppDBSession )
         *ppDBSession = 0 ;
      else
         return return5oledbIID( 0, E_INVALIDARG, IID_IDBCreateSession ) ;  // AS 12/14/00 - changed for conformance tests...

      #ifdef S4OLEDEBUG_PRINT
         log5( "\tSource5::CreateSession(): stage 2\r\n" ) ;
      #endif

   /*   if ( pUnkOuterIn )
         return DB_E_NOAGGREGATION ;  */ /*CJ - The session object now supports Aggregation */
      if ( !initialized )
         return return5oledb(0, E_UNEXPECTED ) ;

      #ifdef S4OLEDEBUG_PRINT
         log5( "\tSource5::CreateSession(): stage 3\r\n" ) ;
      #endif

      // this Data Source object can only create 1 DBSession...
      if ( session )
         return return5oledb(0, DB_E_OBJECTCREATIONLIMITREACHED ) ;

      #ifdef S4OLEDEBUG_PRINT
         log5( "\tSource5::CreateSession(): stage 4\r\n" ) ;
      #endif

      try
      {
         // open a DBSession object
         #ifdef S4CLIENT
            session = new Session5( c4 ) ;
         #else
            session = new Session5( 0 ) ;
         #endif
         if ( !session )
            throw Err5memory() ;

         #ifdef S4OLEDEBUG_PRINT
            log5( "\tSource5::CreateSession(): stage 5\r\n" ) ;
         #endif

         session->init( this ) ; // initialize the object
         #ifdef S4STAND_ALONE
            if ( CustomerStamp.securityOption >= 2 )
               code4verifySet( c4, Copyright ) ;
         #endif
         #ifdef S4OLEDEBUG_PRINT
            log5( "\tSource5::CreateSession(): stage 6\r\n" ) ;
         #endif
         if (session->verifySetIUnKnown(riid, pUnkOuterIn) != S_OK )
            throw Err5oledb (DB_E_NOAGGREGATION, c4) ;
         // get requested interface pointer on DBSession
         #ifdef S4OLEDEBUG_PRINT
            log5( "\tSource5::CreateSession(): stage 7\r\n" ) ;
         #endif
         session->sessionAgg->notSetInterface = 1 ;
         HRESULT hr = session->QueryInterface( riid, (void **) ppDBSession ) ;
         session->sessionAgg->notSetInterface = 0 ;
         if ( FAILED( hr ) )
            throw Err5oledb( hr, c4 ) ;

         #ifdef S4OLEDEBUG_PRINT
            log5( "\tSource5::CreateSession(): stage 8\r\n" ) ;
         #endif
         return return5oledb( c4, S_OK ) ;
      }
      catch( Err5oledb& err )
      {
         #ifdef S4OLEDEBUG_PRINT
            log5( "\tSource5::CreateSession(): stage 9\r\n" ) ;
         #endif
         if ( session )
            delete session ;
         session = 0 ;
         *ppDBSession = 0 ;
         if ( c4 != 0 )
            error4set( c4, 0 ) ;
         #ifdef S4OLEDEBUG_PRINT
            log5( "\tSource5::CreateSession(): stage 10\r\n" ) ;
         #endif
         return return5oledb2(c4, err.oledbErr()) ;
      }
   }



   void Source5::setCatalogFromProperties( Bool5 useCatalogPropertyOnly )
   {
      // the stand-alone version uses location property first (location is used as server name
      // in c/s).  If location is not set it then uses the catalog property.  Note that the
      // only way catalog property would be set is if the source object is uninitialized
      // and then re-initialized since you can only set the catalog property after
      // initialization...

      // if useCatalogPropertyOnly is true, then the location property is ignored...
      if ( catalog != 0 )
      {
         free5( catalog ) ;
         catalog = 0 ;
      }

      #ifdef S4STAND_ALONE
         if ( useCatalogPropertyOnly != 1 )
         {
            // uses properties to set the catalog member of the source object, which is used
            // as the default directory...
            char location[LEN4PATH] ;

            getCharPtrFromUnicode5( properties.getLOCATION(), location ) ;

            // first we use the location property.  If it is blank or is not valid use the catalog setting
                        // CJ June 12/01- removed the _waccess as causes problems in win98/ME this check is also done during
                        // the setting of location property
            //if ( ( strcmp( location, "" ) != 0 ) && ( !_waccess( properties.getLOCATION(), 0 ) ) )
            if ( strcmp( location, "" ) != 0  )
                        {
               // there is a location and we can access it...
               int lenLocation = strlen( location ) ;
               if ( location[lenLocation-1] != '\\' )  // backslash missing, add it...
                  location[lenLocation++] = '\\' ;

               catalog = (char *)alloc5( lenLocation + 1 ) ;  // extra byte for NULL
               memcpy( catalog, location, lenLocation ) ;
               catalog[lenLocation] = 0 ;  // null end string

               if ( properties.setCURRCAT( properties.getLOCATION() ) == FALSE )
                  throw Err5oledb( DB_SEC_E_AUTH_FAILED, c4 ) ;
               return ;
            }
         }
      #endif /* S4STAND_ALONE */

      int curCatLenChars = wcslen( properties.getCURRCAT() ) ;
      // AS 09/27/99 -- catalog is a character string, don't need 2 bytes for every character...
      int numBytesToAllocate = curCatLenChars + 1 ;  // extra byte for null
      Bool5 needsToAddBackslash = 0 ;
      // AS 09/27/99 -- was not setting current directory properly... need to subtract 1 from value...
      // if ( properties.getCURRCAT()[curCatLenChars] != L'\\' && properties.getCURRCAT()[curCatLenChars] != 0 )
      if ( properties.getCURRCAT()[curCatLenChars-1] != L'\\' && properties.getCURRCAT()[curCatLenChars-1] != 0 )
      {
         // the catalog should include trailing backslash...
         needsToAddBackslash = 1 ;
         numBytesToAllocate++ ;  // extra byte for backslash
      }

      catalog = (char *)alloc5( numBytesToAllocate ) ;

      getCharPtrFromUnicode5( properties.getCURRCAT(), catalog ) ;

      if ( needsToAddBackslash )
      {
         assert5( catalog[numBytesToAllocate - 2] == 0 ) ;  // should be string terminator now...
         catalog[numBytesToAllocate - 2] = '\\' ;
         catalog[numBytesToAllocate - 1] = 0 ;  // terminate string
      }
   }


   STDMETHODIMP Source5::Initialize()
   {
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::Initialize()\r\n" ) ;
      #endif

      HRESULT hr = S_OK ;
      #ifdef S4CLIENT
         char *portId = 0 ;
         char *userId = 0 ;
         char *password = 0 ;
         char *serverId = 0 ;
      #endif

      if ( initialized )
         return return5oledb( 0, DB_E_ALREADYINITIALIZED) ;
      // connect code?
      initialized = TRUE ;
      properties.initialize() ;

      try
      {
         #ifdef S4CLIENT
            // need to authorize the client on the server...  Note that need a CODE4 for that...
            unsigned int i ;
            portId = (char *)alloc5( 6 ) ;
            strcpy(portId,"23165") ;
            c4 = code4alloc( 1 ) ;
            oledb4SetCodeBaseDefaults( c4 ) ;

            userId = getCharPtrFromUnicode5( properties.getAUTH_USERID() ) ;
            password = getCharPtrFromUnicode5( properties.getPASSWORD() ) ;
            serverId = getCharPtrFromUnicode5( properties.getLOCATION() ) ;
            for ( i = 0 ; i <= strlen( serverId ) ; i++ )
            {
               if ( serverId[i] == ':' )
               {
                  serverId[i]='\0' ;
                  strcpy( portId, &(serverId[i+1]) ) ;
                  break ;
               }
            }
            c4setAcceptTimeOut(c4, properties.getTIMEOUT()) ;


            if ( CustomerStamp.securityOption >= 2 )
            {
               code4verifySet( c4, "n" );  // CS 2002/06/21  force verify string to be allocated
               u4createCopyrightFromStamp( c4getAppVerify( c4 ) ) ;
               char *stampId = getCharPtrFromUnicode5( properties.getStampInfo() ) ;
               if ( strlen(stampId) == 0 )
                  throw Err5oledb( DB_SEC_E_AUTH_FAILED, c4 ) ;
               if ( memcmp( c4getAppVerify( c4 ), stampId, strlen( stampId ) ) )
                  throw Err5oledb( DB_SEC_E_AUTH_FAILED, c4 ) ;
            }
            if ( code4connect( c4, serverId, portId, userId, password, 0 ) != r4success )
               throw Err5oledb( DB_SEC_E_AUTH_FAILED, c4 ) ;

            switch( code4indexFormat( c4 ) )
            {
               case r4cdx:
                  s5fox = TRUE ;
                  break ;
               case r4ntx:
                  s5clipper = TRUE ;
                  break ;
               case r4mdx:
                  s5mdx = TRUE ;
                  s5hasDescending = FALSE ;
                  break ;
            }
         #endif
         #ifdef S4STAND_ALONE
            if ( CustomerStamp.securityOption >= 2 )
            {
               Copyright = (char *)u4alloc( 215 ) ;
               u4createCopyrightFromStamp( Copyright ) ;
               char *stampId = getCharPtrFromUnicode5( properties.getStampInfo() ) ;
               if ( strlen(stampId) == 0 )
                  throw Err5oledb( DB_SEC_E_AUTH_FAILED, c4 ) ;
               if ( memcmp( Copyright, stampId, strlen( stampId ) ) )
                  throw Err5oledb( DB_SEC_E_AUTH_FAILED, c4 ) ;
            }

            setCatalogFromProperties( 0 ) ;

            // AS 05/26/99 log name should take into account current catalog
            char *logEntryName = getCharPtrFromUnicode5( properties.getLogName() ) ;
            u4nameCurrentExtended( logName, sizeof( logName ), logEntryName, catalog ) ;
            free5( logEntryName ) ;
         #else
            setCatalogFromProperties( 0 ) ;
         #endif
      }
      catch( Err5oledb &err )
      {
         //CJ - 06/02/99 next line not needed as the return return5oledb returns the error correctly.
         //hr = err.oledbCode() ;
         Uninitialize() ;
   //      #ifdef S4CLIENT
   //         if ( c4getCopyright( c4 ) != 0 )
   //            free5( c4getCopyright( c4 ) ) ;
   //         code4initUndo( c4 ) ;
   //      #endif
   //      initialized = FALSE ;
         //CJ - eventhough a CODE4 memeber is available it is not valid due to the Uninitialize() ;
         #ifdef S4CLIENT
            if ( userId != 0 )
               free5( userId ) ;
            if ( password != 0 )
               free5( password ) ;
            if ( serverId != 0 )
               free5( serverId ) ;
            if ( portId != 0 )
               free5( portId ) ;
         #endif
         return return5oledb2( 0, err.oledbErr()) ;
      }

      #ifdef S4CLIENT
         if ( userId != 0 )
            free5( userId ) ;
         if ( password != 0 )
            free5( password ) ;
         if ( serverId != 0 )
            free5( serverId ) ;
         if ( portId != 0 )
            free5( portId ) ;
      #endif
      return return5oledb( 0, hr ) ;
   }


   STDMETHODIMP Source5::Uninitialize()
   {
      // Source5::Uninitialize ---------------------------------------------
      // IDBInitialize method
      //
      // Returns the Source5 Object to an uninitialized state
      //
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::Uninitialize()\r\n" ) ;
      #endif

      if (!initialized)
         return return5oledb( 0, S_OK ) ;
      if (session)
         return return5oledb( 0, DB_E_OBJECTOPEN ) ;

      // disconnect code?
      initialized = FALSE ;
      properties.uninitialize() ;
      #ifdef S4CLIENT
         //if ( c4getCopyright( c4 ) != 0 )
         //   free5( c4getCopyright( c4 ) ) ; /* Moved to the code4initUndo */
         code4initUndo( c4 ) ;
      #endif
      #ifdef S4STAND_ALONE
         u4free(Copyright) ;
         // free5( logName ) ;
      #endif
      free5( catalog ) ;
      catalog = 0 ;
      return return5oledb( 0, S_OK ) ;
   }



   HRESULT STDMETHODCALLTYPE Source5::InterfaceSupportsErrorInfo( REFIID riid )
   {
      return( globalInterfaceSupportsErrorInfo(riid) );
   }



   STDMETHODIMP Source5::GetPropertyInfo(
      ULONG             cPropertySets,       //IN  | Number of properties being asked about
      const DBPROPIDSET rgPropertySets[],    //IN  | Array of cPropertySets properties about which to return information
      ULONG*            pcPropertyInfoSets,  //OUT | Number of properties for which information is being returned
      DBPROPINFOSET**   prgPropertyInfoSets, //OUT | Buffer containing default values returned
      WCHAR**           ppDescBuffer         //OUT | Buffer containing property descriptions
      )
   {
      // Source5::GetPropertyInfo  -----------------------------------------
      // IDBProperties method
      //
      // Returns information about properties supported by the provider
      //
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::GetPropertyInfo()\r\n" ) ;
      #endif

      try
      {
         HRESULT hr = properties.getPropertyInfo( cPropertySets, rgPropertySets, pcPropertyInfoSets, prgPropertyInfoSets, ppDescBuffer ) ;
         return return5oledb( 0 , hr ) ;
      }
      catch(Err5oledb &err)
      {
         return return5oledb2( 0, err.oledbErr() ) ;
      }
   }



   STDMETHODIMP Source5::GetProperties(
      ULONG             cPropertySets,    //IN  | count of restriction guids
      const DBPROPIDSET rgPropertySets[], //IN  | restriction guids
      ULONG*            pcProperties,     //OUT | count of properties returned
      DBPROPSET**       prgProperties     //OUT | property information returned
   )
   {
      // Source5::GetProperties ----------------------------------------------------
      // IDBProperties method
      //
      // Returns current settings of properties in the data source property groups
      //
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::GetProperties()\r\n" ) ;
      #endif

      try
      {
         HRESULT hr = properties.getProperties( cPropertySets, rgPropertySets, pcProperties, prgProperties ) ;
         #ifdef S4OLEDEBUG_PRINT
            if ( hr == 0 )
               log5( "Source5::GetProperties() succeeded\r\n" ) ;
            else
               log5( "Source5::GetProperties() failed\r\n" ) ;
         #endif
         return return5oledb( 0 , hr ) ;
      }
      catch(Err5oledb &err)
      {
         return return5oledb2( 0, err.oledbErr() ) ;
      }
   }


   // Source5::SetProperties  --------------------------------------------
   // IDBProperties method
   //
   // Set properties in the data source property groups
   //
   STDMETHODIMP Source5::SetProperties(
      ULONG     cProperties,   //IN  | count of restriction guids
      DBPROPSET rgProperties[] //IN  | restriction guids
   )
   {
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::SetProperties()\r\n" ) ;
      #endif

      HRESULT hr = properties.setProperties( cProperties, rgProperties ) ;

      if ( initialized && ( hr == DB_S_ERRORSOCCURRED || hr == S_OK ) )
      {
         // the only property that can be set here is the catalog property, so assume that
         // if hr is S_OK OR DB_S_ERROSOCCURRED that the catalog property was set and assign to it.
         // may want to a add an assertion here to ensure that this is indeed the case.
         // - S_ERRORSOCCURRED occurred means still ok... a property was set, must be
         // the catalog property...
         setCatalogFromProperties( 1 ) ;  // only use catalog property to re-set
      }

      if (hr == DB_E_ERRORSOCCURRED)
      {
         ERRORINFO info ;

         info.hrError = DB_E_ERRORSOCCURRED ;
         info.dwMinor = ERR_CODEBASE_PROPERTIES;
         #ifndef S4SERVER
            info.clsid = CodeBaseClsid ;
         #endif
         info.iid = IID_IDBProperties ;
         return return5oledb2( 0, info ) ;
      }else
         return return5oledb( 0, hr ) ;
   }



   STDMETHODIMP Source5::GetClassID( CLSID *pClsID )
   {
      // Source5::GetClassID ---------------------------------------------
      // IPersist method
      //
      // Returns the CLSID for this OLE DB provider
      // CLSID - OUT | A pointer to memory in which to return the class ID
      //
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::GetClassID()\r\n" ) ;
      #endif

      if ( !pClsID )
         return  E_FAIL ;
      memcpy( pClsID, &CodeBaseClsid, sizeof( CLSID ) ) ;
      return S_OK ;
   }



   STDMETHODIMP Source5::GetLiteralInfo( ULONG cLiterals, const DBLITERAL rgLiterals[],
      ULONG *pcLiteralInfo, DBLITERALINFO **prgLiteralInfo, OLECHAR **ppStringsBuffer )
   {
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::GetLiteralInfo()\r\n" ) ;
      #endif

      HRESULT hr = S_OK ;

      if ( pcLiteralInfo == 0 )
         hr = E_INVALIDARG ;
      else
         *pcLiteralInfo = 0 ;

      if ( prgLiteralInfo == 0 )
         hr = E_INVALIDARG ;
      else
         *prgLiteralInfo = 0 ;

      if ( ppStringsBuffer == 0 )
         hr = E_INVALIDARG ;
      else
         *ppStringsBuffer = 0 ;

      if ( cLiterals != 0 && rgLiterals == 0 )
         hr = E_INVALIDARG ;

      if ( initialized == 0 )
         hr = E_UNEXPECTED ;

      if ( hr != S_OK )
         return hr ;

      WSTR5 *strings = L"\\" ;
      int allUnsupported = 1, someUnsupported = 0 ;

      try
      {
         (*ppStringsBuffer) = (OLECHAR *)alloc5( (wcslen( strings ) + 1) * 2 ) ;
         wcscpy( *ppStringsBuffer, strings ) ;

         if ( cLiterals != 0 )
            *pcLiteralInfo = cLiterals ;
         else
            *pcLiteralInfo = 5 ;  // only support 4 literals

         (*prgLiteralInfo) = (DBLITERALINFO *)alloc5null( (*pcLiteralInfo) * sizeof( DBLITERALINFO ) ) ;

         for ( unsigned long loop = 0 ; loop < (*pcLiteralInfo) ; loop++ )
         {
            if ( cLiterals == 0 ) // just go through all
            {
               allUnsupported = 0 ;
               switch( loop )
               {
                  case 0:
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = *ppStringsBuffer ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = DBLITERAL_CATALOG_SEPARATOR ;
                     (*prgLiteralInfo)[loop].cchMaxLen = 1 ;
                     break ;
                  case 1:
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = DBLITERAL_TABLE_NAME ;
                     (*prgLiteralInfo)[loop].cchMaxLen = LEN5TABLE_NAME ;
                     break ;
                  case 2:
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = DBLITERAL_INDEX_NAME ;
                     (*prgLiteralInfo)[loop].cchMaxLen = LEN4TAG_ALIAS ;
                     break ;
                  case 3:
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = DBLITERAL_COLUMN_NAME ;
                     (*prgLiteralInfo)[loop].cchMaxLen = 10 ;
                     break ;
                  case 4:
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = DBLITERAL_CATALOG_NAME ;
                     (*prgLiteralInfo)[loop].cchMaxLen = LEN5TABLE_NAME ;
                     break ;
                  default:  // should not occur
                     throw Err5internal(c4) ;
               }
            }
            else  // check the inputs...
            {
               /* AS note that providing 'invalid' characters as OLE-DB requires
                  is not particularly easy or useful (since using OLE-CHAR,
                  so you would need to consider all foreign languages, etc.)
                  I have decided to just implement by allowing any characters,
                  and wait for a more useful version of the specification to
                  be presented by microsoft. */

               switch( rgLiterals[loop] )
               {
                  case DBLITERAL_CATALOG_SEPARATOR:
                     allUnsupported = 0 ;
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = *ppStringsBuffer ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = rgLiterals[loop] ;
                     (*prgLiteralInfo)[loop].cchMaxLen = 1 ;
                     break ;
                  case DBLITERAL_TABLE_NAME:
                     allUnsupported = 0 ;
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = rgLiterals[loop] ;
                     (*prgLiteralInfo)[loop].cchMaxLen = LEN5TABLE_NAME ;
                     break ;
                  case DBLITERAL_CATALOG_NAME:
                     allUnsupported = 0 ;
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = rgLiterals[loop] ;
                     (*prgLiteralInfo)[loop].cchMaxLen = LEN5TABLE_NAME ;
                     break ;
                  case DBLITERAL_INDEX_NAME:
                     allUnsupported = 0 ;
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = rgLiterals[loop] ;
                     (*prgLiteralInfo)[loop].cchMaxLen = LEN4TAG_ALIAS ;
                     break ;
                  case DBLITERAL_COLUMN_NAME:
                     allUnsupported = 0 ;
                     (*prgLiteralInfo)[loop].fSupported = TRUE ;
                     (*prgLiteralInfo)[loop].pwszLiteralValue = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidChars = 0 ;
                     (*prgLiteralInfo)[loop].pwszInvalidStartingChars = 0 ;
                     (*prgLiteralInfo)[loop].lt = rgLiterals[loop] ;
                     (*prgLiteralInfo)[loop].cchMaxLen = 10 ;
                     break ;
                  default:
                     (*prgLiteralInfo)[loop].fSupported = FALSE ;
                     someUnsupported = 1 ;
                     break ;
               }
            }
         }

         if ( someUnsupported )
         {
            if ( allUnsupported )
            {
               free5( *ppStringsBuffer ) ;
               *ppStringsBuffer = 0 ;
               return DB_E_ERRORSOCCURRED ;
            }
            else
               return DB_S_ERRORSOCCURRED ;
         }
      }
      catch( Err5oledb& )
      {
         *pcLiteralInfo = 0 ;
         if ( *prgLiteralInfo )
         {
            free5( *prgLiteralInfo ) ;
            *prgLiteralInfo = 0 ;
         }
         if ( *ppStringsBuffer )
         {
            free5( *ppStringsBuffer ) ;
            (*ppStringsBuffer) = 0 ;
         }
      }

      return S_OK ;
   }



   STDMETHODIMP Source5::GetKeywords( LPOLESTR *ppwszKeywords )
   {
      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5::GetKeywords()\r\n" ) ;
      #endif

      if ( ppwszKeywords == 0 )
         return E_INVALIDARG ;

      *ppwszKeywords = 0 ;  // also, we have no keywords?

      if ( initialized == FALSE )
         return E_UNEXPECTED ;

      return S_OK ;
   }


   HRESULT Source5::verifySetIUnKnown(REFIID riid, IUnknown *punkOuter)
   {
      IUnknown *test ;
      HRESULT hr ;

      if ( punkOuter == 0 )
         return return5noErr( c4, S_OK ) ;
      if ( riid != IID_IUnknown )
         return return5noErr( c4, E_FAIL ) ;
      hr = punkOuter->QueryInterface( IID_IUnknown, (void **)&test) ;
      if ( hr != S_OK  )
         return return5noErr( c4, E_FAIL ) ;
      test->Release() ;
      return return5noErr( c4, S_OK ) ;
   }

#endif /* !S4SERVER */



#ifdef S4SERVER
   char *Source5all::systemPath()
   {
      // can't be inline because required structures for c4systemPath not defined yet
      return c4systemPath( c4 ) ;
   }



   void Source5all::initUndo()
   {
      code4enterExclusive( c4, c4getCatalogClient( c4 ), 0 ) ;   // AS 09/30/98 -- don't wait, else may end up waiting for ever
      c4->currentClient = c4getCatalogClient( c4 ) ;
      if ( tablesSchema != 0 )
      {
         delete tablesSchema ;
         tablesSchema = 0 ;
      }
      if ( indexesSchema != 0 )
      {
         delete indexesSchema ;
         indexesSchema = 0 ;
      }
      if ( columnsSchema != 0 )
      {
         delete columnsSchema ;
         columnsSchema = 0 ;
      }
      code4exitExclusive( c4, c4getCatalogClient( c4 ) ) ;
   }



#endif /* S4SERVER */

#ifndef S4SERVER
   STDMETHODIMP_(ULONG) Source5interface::AddRef()
   {
      ULONG count ;

      notSetInterface = 1 ;

      count = pSource->AddRef() ;

      notSetInterface = 0 ;

      return count ;
   }



   STDMETHODIMP Source5interface::QueryInterface(REFIID riid, LPVOID *ppv)
   {
      HRESULT hr;
      notSetInterface = 1 ;

      #ifdef S4OLEDEBUG_PRINT
         log5( "Source5interface::QueryInterface()" ) ;
         out5riidInfo( riid ) ;
         log5( "\r\n" ) ;
      #endif
      hr = pSource->QueryInterface(riid, ppv);

      notSetInterface = 0 ;
      return return5noErr( pSource->c4, hr );
   }



   STDMETHODIMP_(ULONG) Source5interface::Release()
   {
      ULONG count ;

      notSetInterface = 1 ;

      count = pSource->Release() ;

      notSetInterface = 0 ;

      return count ;
   }
#endif /* !S4SERVER */

#ifdef S4MAX_OR_SERVER
   #undef S4MAX_OR_SERVER
#endif

#endif /* OLEDB5BUILD */
