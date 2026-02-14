// Session5.hpp

#ifdef OLEDB5BUILD

class StatusData5 ;
class Table5cache ;
#ifndef S4SERVER
   class Table5 ;
#endif
class Source5all ;

enum Regen5
{
   regen5no = 0,
   regen5opt,
   regen5reqd,
} ;

enum Schema5type
{
   schema5typeTables,
   schema5typeIndexes,
   schema5typeColumns,
   schema5typeProvs,
   schema5typeInvalid,
} ;

class S5CLASS Dbid5 : public Link5
{
public:
   DBID id ;
} ;

class Session5;
class Schema5tables ;
class Schema5indexes ;
class Schema5columns ;

#ifndef S4SERVER
class S5CLASS Session5interface : private IUnknown
{
public:

   Session5interface() { pSession=NULL; notSetInterface=0;}
   Session5interface( Session5 *session ) {pSession=session; notSetInterface=0; }
   DECLARE5_IUnknown

   Session5 *pSession ;
   short notSetInterface ;
};
#endif



#ifdef S4SERVER
   class Table5schemaServerLocal ;
#endif



// Corresponds to an OLE DB session object
#ifdef S4SERVER
   class S5CLASS Session5
#else
   #ifdef S4TESTING
          // For testing, we need to access the IOpenRowset functions directly, so make public
      class S5CLASS Session5 : public IOpenRowset,
   #else
      // in normal case, don't allow direct access to IOpenRowset
      class S5CLASS Session5 : private IOpenRowset,
   #endif
   #ifdef S4COMMAND
      private IDBCreateCommand,
   #endif
   private ISessionProperties, private IDBSchemaRowset, private ITransactionLocal, private ITableDefinition,
   private IGetDataSource, private IIndexDefinition, private ITransactionOptions, private Unknown5,
   private ISupportErrorInfo
#endif
{
public:
   void * operator new( size_t s ) { void *ptr = alloc5(s) ; memset( ptr, 0, s); return ptr ; }
   void operator delete( void *p ) { free5(p) ; }
   Session5( CODE4 *c4 ) ;  // for client version, c4 cannot be NULL
   ~Session5(void) ;

   #ifndef S4SERVER
      Session5properties properties;

      DECLARE5_IUnknown
      DECLARE5_IGetDataSource
      DECLARE5_IOpenRowset
      DECLARE5_ISessionProperties
      DECLARE5_IDBSchemaRowset
      DECLARE5_ITransactionLocal
      DECLARE5_ITransactionOptions
      DECLARE5_ITableDefinition
      DECLARE5_IIndexDefinition
      DECLARE5_ISupportErrorInfo
      #ifdef S4COMMAND
         DECLARE5_IDBCreateCommand
      #endif
   #endif

   void init( Source5all *pSource5 ) ;

   int isUnicode ;

   CODE4 *c4 ;
   #ifdef S4SERVER
      Source5all *source ;
   #else
      Source5 *source ;
   #endif

   #ifndef S4SERVER
      MEM4 *memRowIndexNoKey ;  // memory for Row5index (when no key present)
          // memRows requires a zero allocator
      Mem5zero *memRows ;    // memory for Row5 - zero out
   #endif
// KS   int fullRows ; // Flag just in case Accessor's are created after the fact.
                  // True means send full rows.  Set under single user in particular.
// KS   int fullKeys ; // Always send full keys.  Do not set automatically in single user.
//   ISOLATIONLEVEL isolationLevelDefault ;  // used when no transaction is in progress

   List5 openTables ;    // list of opened Table5's
   List5 ids ;

   int startTransaction( int transactionIsolationLevel ) ;
   void abortTransaction( void ) ;
   void commitTransaction( void ) ;

   #ifndef S4SERVER
      List5 cachedTables ;  // list of cached closed Table5cache's
      int numCachedTables ; // number of caches before files actually get closed

      void cacheTable( Table5cache * ) ;  // caches close tables
      void closeExtraCachedTables() ; // when transactions end
      Table5 *findCachedTable( const char *, int doRemove ) ;
      Table5 *findOpenTable( const char * ) ;
      Table5 *openSessionTable( LPCWSTR, int ) ;
      HRESULT verifySetIUnKnown(REFIID riid, IUnknown *punkOuter);
      IUnknown *pUnkOuter;
      Session5interface *sessionAgg ;

      createIndexTagExpressionAddColumn( char **expression, WSTR5 *colName, Table5 *table, Bool5 needsPlusSign, DBINDEX_COL_ORDERENUM order, Bool5 needsAscend ) ;
      DBID *createIndexId( const DBID *pTableID, const char *tagName, DBID *pIndexID ) ;
   #endif

   inline ISOLEVEL isolationLevel() { return ( inTransaction == 0 ) ? NO5TRANSACTION : isoLevel ; }

   void isolationLevel( ISOLEVEL isolationLevel ) ;  // KS Can be changed in the middle of a transaction for refetch additional field purposes.

   // AS added function - checks the REGEN field of the config file for setting
   // options are: regen5no, regen5opt, and regen5reqd.
   // actually should be stored somewhere here, but read from config file on open
   inline Regen5 regenColumns() { return regen ; }
   inline void regenColumns( Regen5 newRegen ) { regen = newRegen ; }  // sets regen option

   Regen5 regen ;

   #ifdef S4SERVER
      Table5schemaServerLocal *tablesSchema ;
      Table5schemaServerLocal *indexesSchema ;
      Table5schemaServerLocal *columnsSchema ;
   #endif

   char *userId ; //CJ-03/12/98-Added as two function need the same value, retrieved only once per session
   void setInTransaction( char val ) ;
private:
   int nameConform( const char *nameToConform, char *outputName, int outputNameLen, const char *catalog, char *extensionToUse = 0 ) ;
   #ifndef S4SERVER
      char *getTagName( Table5 *table, DBID *pTableID, DBID *pIndexID ) ;
   #endif

   #ifdef S4STAND_ALONE
      void versionCheck() ;
   #endif
   // for client, isoLevel is stored on the server
   ISOLEVEL isoLevel ; // Value is ISOLATIONLEVEL_UNSPECIFIED when not in a transaction
   char inTransaction ; // true/false
   #ifndef S4SERVER
      void unlockAll() ;
   #endif
} ;

/*
class TransactionOptions5 : public ITransactionOptions
{
public:
   XACTOPT options ;
} ;
*/

// S4CLIENT, S4STAND_ALONE, S4SERVER
// Forward declarations ------------------------------------------------------

//class Imp5IGetDataSource;
//class Imp5IOpenRowset;
//class Imp5ISessionProperties;
//class Imp5IDBSchemaRowset;
//class Imp5ITransactionLocal;
//typedef Imp5IGetDataSource*      PIMP5IGETDATASOURCE;
//typedef Imp5IOpenRowset*         PIMP5IOPENROWSET;
//typedef Imp5ISessionProperties*  PIMP5ISESSIONPROPERTIES;
//typedef Imp5IDBSchemaRowset*     PIMP5IDBSCHEMAROWSET;
//typedef Imp5ITransactionLocal*   PIMP5ITRANSACTIONLOCAL;
/*
class Session5base
{
public:
   // contains elements common to CODE4 for client and stand-alone, and common to a given
   // client for server

   int isolationLevel ; // -1 no Transaction in progress
   int isolationLevelDefault ;  // used when no transaction is in progress

   List5 openData5s ;    // list of opened Table5's
//   int isolationLevel() ;
//   void isolationLevel( int isolationLevel ) ;  // set

   int startTransaction( int transactionIsolationLevel ) ;
   void abortTransaction( void ) ;
   void commitTransaction( void ) ;
} ;

class Session5client : public Session5base
{
   // for the server, builds on the Session5base by adding required
} ;

class Code5Db
{
public:
   // contains elements common to a CODE4 for client, stand-alone, and server
   // also notes and sets up default CODE4 settings based on properties

   CODE4 *c4 ;

   MEM4 *memIndex ;  // used by Index5client, Index5server and Index5stand
   MEM4 *memTable ;   // used by Table5stand, Table5server and Table5client

   void init()   // initialize the default CODE4 settings based on the properties
   {
      // ??use DBPROP_INIT_MODE to set:
      //  Code4.readOnly
      //  Code4.accessMode
   }
} ;

class Session5lowServer : Code5Db
{
public:
   Ptr5array *dataArray ;   // table5server
   Ptr5array *indexArray ;   // index5server

   ~Session5lowServer() ; //??  { uninitialize and free up the arrays ; }
} ;

class Session5perClient : Session5base
{
   // used by S4SERVER, for each client
} ;
*/

// typedef Session5 *PSESSION5;

/*
?? class Imp5IGetDataSource : public IGetDataSource
{
   private:
      DEFINE_DEFAULT_IUNKNOWN_MEMBER_DATA(Session5)

   public:
      DEFINE_DEFAULT_IUNKNOWN_CTOR_DTOR(Session5, Imp5IGetDataSource);
      DEFINE_DEFAULT_IUNKNOWN_ADDREF_RELEASE

};
*/
/*
?? class Imp5IOpenRowset : public IOpenRowset
{
   private:
      DEFINE_DEFAULT_IUNKNOWN_MEMBER_DATA(Session5)

   public:
      DEFINE_DEFAULT_IUNKNOWN_CTOR_DTOR(Session5, Imp5IOpenRowset);
      DEFINE_DEFAULT_IUNKNOWN_ADDREF_RELEASE

      //IOpenRowset method
      //OpenRowset
      STDMETHODIMP OpenRowset( IUnknown*, DBID*, DBID*, REFIID, ULONG, DBPROPSET[], IUnknown** );
};

//?? class Imp5ISessionProperties : public ISessionProperties
{
   private:
      DEFINE_DEFAULT_IUNKNOWN_MEMBER_DATA(Session5)

   public:
      DEFINE_DEFAULT_IUNKNOWN_CTOR_DTOR(Session5, Imp5ISessionProperties);
      DEFINE_DEFAULT_IUNKNOWN_ADDREF_RELEASE

      //ISessionProperties methods
      //GetProperties method
      STDMETHODIMP GetProperties(ULONG cPropertySets, const DBPROPIDSET rgPropertySets[], ULONG* pcProperties, DBPROPSET** prgProperties);
      //SetProperties method
      STDMETHODIMP SetProperties(ULONG cProperties, DBPROPSET rgProperties[]);
};

//?? class Imp5IDBSchemaRowset : public IDBSchemaRowset
{
   private:
      DEFINE_DEFAULT_IUNKNOWN_MEMBER_DATA(Session5)

   public:
      DEFINE_DEFAULT_IUNKNOWN_CTOR_DTOR(Session5, Imp5IDBSchemaRowset);
      DEFINE_DEFAULT_IUNKNOWN_ADDREF_RELEASE

      //IDBSchemaRowset methods
      //GetRowset method
      STDMETHODIMP GetRowset(IUnknown *punkOuter, REFGUID rguidSchema, ULONG cRestrictions, const VARIANT rgRestrictions[], REFIID riid, ULONG cPropertySets, DBPROPSET rgPropertySets[], IUnknown **ppRowset);
      //GetSchemas method
      STDMETHODIMP GetSchemas(ULONG *pcSchemas, GUID **prgSchemas, ULONG** prgRestrictionSupport);
};

//?? class Imp5ITransactionLocal : public ITransactionLocal
{
private:
   DEFINE_DEFAULT_IUNKNOWN_MEMBER_DATA(Session5)

public:
   DEFINE_DEFAULT_IUNKNOWN_CTOR_DTOR(Session5, Imp5ITransactionLocal);
   DEFINE_DEFAULT_IUNKNOWN_ADDREF_RELEASE

   //ITransactionLocal methods
   //Abort method
   STDMETHODIMP Abort(BOID *pboidReason, BOOL fRetaining, BOOL fAsync);
   //Commit method
   STDMETHODIMP Commit(BOOL fRetaining, DWORD grfTC, DWORD grfRM);
   //GetTransactionInfo method
   STDMETHODIMP GetTransactionInfo(XACTTRANSINFO *pInfo);
   //GetOptionsObject method
   STDMETHODIMP GetOptionsObject(ITransactionOptions **ppOptions);
   //StartTransaction method
   STDMETHODIMP StartTransaction(ISOLEVEL isoLevel, ULONG isoFlags, ITransactionOptions *pOtherOptions, ULONG *pulTransactionLevel);
} ;

*/

// used by source and session to set default values after CODE4 initialization
void oledb4SetCodeBaseDefaults( CODE4 *c4 ) ;

#endif /* OLEDB5BUILD */
