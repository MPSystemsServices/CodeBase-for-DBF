// a5.hpp Accessor Classes

#if defined( OLEDB5BUILD ) && !defined( S4SERVER )
   //#define pAccessor(p) (Accessor5 *)(p) Must always assume it is an Accessor5all
   class Accessor5all ;
   class Accessor5index ;
   class Accessor5handler ;
   class Index5 ;

   #define pure5dbType( a ) ( a & ~DBTYPE_BYREF & ~DBTYPE_VECTOR & ~DBTYPE_ARRAY )

   inline Accessor5all *pAccessor5all(HACCESSOR a) { return (Accessor5all *)(a) ; }
   inline HACCESSOR HACCESSOR5( Accessor5all *p ) { return HACCESSOR(p) ; }
   inline Accessor5all *pAccessor5all(void *a) { return (Accessor5all *)(a) ; } // For List5 functions

   // Extra info needed by SetData
   class Accessor5stepMultiple : public Convert5step
   {
   public:
      int iBinding ; // Used for the setData to specify which binding it is for.
   } ;



   class Accessor5all : public Link5
   {
   public:
      void * operator new( size_t s ) { return alloc5null(s) ; }
      void operator delete( void *p ) { free5(p) ; }
      Accessor5all()
      {
         ref = 1 ;
         checkMe = 1 ;
         accessorHandler = 0 ;
         fieldset = 0 ;
         statusIndexes = 0 ;
         statusIndexesFieldOrdinal = 0 ;
         nBindings = 0 ;
         bindings = 0 ;
         nMultiple = 0 ;
         stepsSetMultiple = 0 ;
         deferredNecessary = 0 ;
         interfaceNecessary = 0 ;
      }
      ~Accessor5all() ;

      // Functions that correspond to IAccessor functions
      // Corresponds mostly to IAccessor::CreateAccessor
      HRESULT initLow( Accessor5handler *accHandler, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] ) ;
      virtual HRESULT init( Rowset5all *rowsetParm, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] ) = 0 ;

      // Corresponds mostly to IAccessor::AddRefAccessor
      inline HRESULT addRefAccessor( ULONG  *pcRefCount) { ref++ ; ( pcRefCount == 0 ) ? 0 : ( *pcRefCount = ref ) ; return S_OK ; }

      HRESULT getBindings( DBACCESSORFLAGS *pdwAccessorFlags, ULONG *pcBindings, DBBINDING **prgBindings ) ;

      // Frees up accessor memory when count gets to zero.
      virtual HRESULT STDMETHODCALLTYPE releaseAccessor( ULONG __RPC_FAR *pcRefCount) ;

      virtual Accessor5index *getIndexAccessor( Session5 *session, Index5 *index ) = 0 ;

      // Ensures that the Accessor is initialized, and that the accessor field set matches the current rowset field set.
      void ensureAccessorIsCurrent( int isNullable, int *iRowset ) ;

      inline HRESULT dataRead( Row5all *row, char *pConsumerBuffer ) ;

      HRESULT dataSet( Row5all *row, char *pConsumerBuffer, int maxBinding = -1 ) ;

   //   virtual unsigned int getColumnInfoIndex( unsigned int bindingOrdinal ) = 0 ;  // gets the index into the column info based on the binding ordinal - differs for integrated indexes...

      ULONG ref ;
      Fieldset5all *fieldset ;
      DBACCESSORFLAGS accessorFlags ;

      // For status optimizations in transferData()
      ULONG nStatus ;
      ULONG *statusIndexes ; // An array of indexes into 'bindings' for which status value is relevant.
      ULONG *statusIndexesFieldOrdinal ; // One entry per field.  It is an index by field ordinal into 'bindings'.

   //   Rowset5all *rowset ;
      Accessor5handler *accessorHandler ;

      unsigned nBindings ;
      DBBINDING *bindings ;
      Convert5consumer stepsRead ; // To quickly allow transferData to quickly fill in value parts.
                             // There is one step entry for each binding.
      Convert5consumer stepsSet ; // For setting as opposed to reading.
      int nMultiple ; // # of stepsSetMultiple
      Accessor5stepMultiple *stepsSetMultiple ; // This is for steps converting from the row buffer to the row buffer
                                        // in the case where there are multiple buffer entries for the same field.
      Bool5 deferredNecessary ; // TRUE if the Accessor5 requires deferred data,
                               // FALSE if the Accessor5 does not
      Bool5 interfaceNecessary ; // TRUE if the Accessor5 requires interface data,
                               // FALSE if the Accessor5 does not

      void *getFindValue( void __RPC_FAR *pFindValue, int *len, char **freePtr ) ;
      BOOL isStringType() ;
      void fieldToString( const void *fieldBuffer, char **pConsumer, char **freePtr ) ;
      char checkMe ;
      char nullFieldsetOK ;
   } ;



   class Accessor5rowset : public Accessor5all
   {
      virtual Accessor5index *getIndexAccessor( Session5 *session, Index5 *index ) { return 0 ; }  // for non-index, non-integrated rowsets there is none
      inline HRESULT initDo( Accessor5handler *accHandler, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] ) ;
      inline HRESULT init( Rowset5all *rowsetParm, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] ) ;
   //   unsigned int getColumnInfoIndex( unsigned int bindingOrdinal ) { return bindingOrdinal ; }  // for rowset, the column info entry matches the ordinal
   } ;



   class Accessor5index : public Accessor5all
   {
   public:
      Accessor5index( Bool5 fromIntParm ) { checkMe = 0 ; isFromIntegratedAccessorHandler = fromIntParm ; }
      virtual Accessor5index *getIndexAccessor( Session5 *session, Index5 *index ) { return this ; }
      inline HRESULT initDo( Accessor5handler *accHandler, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] ) ;
      inline HRESULT init( Rowset5all *rowsetParm, DBACCESSORFLAGS dwAccessorFlags, ULONG cBindings, const DBBINDING rgBindings[], ULONG cbRowSize, DBBINDSTATUS rgStatus[] ) ;
   //   unsigned int getColumnInfoIndex( unsigned int bindingOrdinal ) { return bindingOrdinal ; }  // for index, the column info entry matches the ordinal
   private:
      Bool5 isFromIntegratedAccessorHandler ;
   } ;



   class Accessor5integrated : public Accessor5rowset
   {
   public:
      // an integrated index rowset needs 2 accessors, 1 onto the rowset and 1 onto the index
      // the indexAccessor member here points to the accessor belonging to the index group if available.
      Accessor5integrated() { indexAccessor = 0 ; indexAccessorHandler = 0 ; }
      ~Accessor5integrated() ;
      void createIndexAccessor( Session5 *session, Index5 *index ) ;
      virtual HRESULT STDMETHODCALLTYPE releaseAccessor( ULONG __RPC_FAR *pcRefCount) ;
      Accessor5index *getIndexAccessor( Session5 *session, Index5 *index )
      {
         if ( indexAccessor == 0 )
            createIndexAccessor( session, index ) ;
         return indexAccessor ;
      }
   private:
      Accessor5index *indexAccessor ;
      Accessor5handler *indexAccessorHandler ;
   } ;

   inline Accessor5index *pAccessor5index(HACCESSOR a, Session5 *session, Index5 *index) { return ((Accessor5all *)(a))->getIndexAccessor( session, index ) ; }
#endif /* #if defined( OLEDB5BUILD ) && !defined( S4SERVER ) */
