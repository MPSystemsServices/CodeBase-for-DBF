
template <class T> class Array5index
//class Array5index
{
public:
   void * operator new( size_t s ) { void *ptr = u4alloc(s) ; return ptr ; }
   void * operator new[]( size_t s ) { void *ptr = u4alloc(s) ; return ptr ; }
   void operator delete( void *p ) { u4free(p) ; }
   void operator delete[]( void *p ) { u4free(p) ; }

   short index ;
   T *item ;
} ;

// number of elements to increase size by for increasing array size
#define ADDTOARRAY5 100

// an efficient array - doesn't require moving memory because it uses
//   an array of indexes in addition to the array of elements
template<class T> class Array5
{
public:
   Array5() { numActiveElements = 0 ; maxElements = 0 ; itemArray = 0 ; handleArray = 0 ; lastHandle = 0 ; mem4init() ; }
   ~Array5() { free() ; mem4reset() ; }
   int top()  // returns index of 1st element
   {
      if ( numActiveElements == 0 )
         return -1 ;
      return itemArray[1].index ;
   }
   int next( int handleIndex )
   {
      /* note that this routine does not work correctly if items are
         removed or added between calls of top() or next() */
      short itemIndex = handleArray[handleIndex]->index + 1 ;
      if ( itemIndex > numActiveElements )
         return -1 ;
      assert5( itemArray[itemIndex].item != 0 ) ;
      return itemArray[itemIndex].index ;
   }
   void free()
   {
      #ifdef E4DEBUG
         check() ;
      #endif

      if ( itemArray )
      {
         delete [] itemArray ;
         // free5( itemArray ) ;
         itemArray = 0 ;
      }
      if ( handleArray )
      {
         u4free( handleArray ) ;
         handleArray = 0 ;
      }
      maxElements = numActiveElements = 0 ;
   }
   short add( T *item )   // returns a handle to the item used for retrieval
   {
      if ( numActiveElements >= maxElements - 1 )  // need to increase array size (-1 due to 0 entry not used)
      {
         short newSize = maxElements + ADDTOARRAY5 ;
         Array5index<T> *newItemArray = new Array5index<T>[newSize] ; // alloc5( newSize * sizeof( Array5index<T>) ) ;
         Array5index<T> **newHandleArray = (Array5index<T> **)u4alloc( newSize * sizeof( Array5index<T> * ) ) ;

         memcpy( newItemArray, itemArray, maxElements * sizeof( Array5index<T> ) ) ;
         memcpy( newHandleArray, handleArray, maxElements * sizeof( Array5index<T> * ) ) ;

         if ( handleArray )
            u4free( handleArray ) ;
         handleArray = newHandleArray ;
         if ( itemArray )
            delete [] itemArray ;
         itemArray = newItemArray ;

         int loop ;

//         assert5( numActiveElements == maxElements - 1 ) ;  // the old array set was just full

         for ( loop = 1 ; loop <= numActiveElements ; loop++ )
            handleArray[itemArray[loop].index] = &itemArray[loop] ;

         for ( loop = maxElements ; loop < newSize ; loop++ )
         {
            handleArray[loop] = &itemArray[loop] ;
            itemArray[loop].index = loop ;
            itemArray[loop].item = 0 ;   // precautionary
         }

         maxElements = newSize ;
      }

      #ifdef E4DEBUG
         check() ;
      #endif

      lastHandle++ ;
      numActiveElements++ ;
      short myHandle = itemArray[lastHandle].index ;

      itemArray[lastHandle].item = item ;

      #ifdef E4DEBUG
         check() ;
      #endif

      return myHandle ;
   }
   #ifdef E4DEBUG
   void check()
   {
      for ( short i = 0 ; i < maxElements ; i++ )
      {
         assert5( handleArray[i] != 0 ) ;
         assert5( handleArray[itemArray[i].index] = &itemArray[i] ) ;
         assert5( handleArray[i]->index == i ) ;
      }
   }
   #endif
   T *get( short index )
   {
      #ifdef E4DEBUG
         check() ;
      #endif

      assert5( index > maxElements || index <= 0 || handleArray[index] != 0 ) ;  // either should be valid and a valid handle or index is out of range
      return ( (index <= 0 || index > maxElements) ? 0 : handleArray[index]->item ) ;
   }
   T *remove( short handleIndex )
   {
      #ifdef E4DEBUG
         check() ;
      #endif

      if ( handleIndex <= 0 || handleIndex > maxElements )  /* invalid */
         return 0 ;

      T *retItem = handleArray[handleIndex]->item ;

      short saveHandleIndex = itemArray[lastHandle].index ;

      handleArray[handleIndex]->index = itemArray[lastHandle].index ;
      handleArray[handleIndex]->item = itemArray[lastHandle].item ;
      itemArray[lastHandle].index = handleIndex ;
      handleArray[saveHandleIndex] = handleArray[handleIndex] ;
      handleArray[handleIndex] = &itemArray[lastHandle] ;
      itemArray[lastHandle].item = 0 ;  // precautionary

      lastHandle-- ;
      numActiveElements-- ;
      #ifdef E4DEBUG
         check() ;
      #endif
      return retItem ;
   }
   T *pop()
   {
      return ( (numActiveElements == 0) ? 0 : remove( itemArray[numActiveElements].index ) ) ;
   }
   void freeAll()
   {
      // removes and deletes every item on the array
      #ifdef E4DEBUG
         check() ;
      #endif

      for( ;; )
      {
         T *item = pop() ;
         if ( item == 0 )
            break ;
         delete item ;
      }
      free() ;
   }
   short find( T *val )
   {
      #ifdef E4DEBUG
         check() ;
      #endif

      short i ;
      for ( i = 1 ; i <= numActiveElements ; i++ )
         if ( itemArray[i].item == val )
            return itemArray[i].index ;  // return the handle index which is constant
      return -1 ;
   }
   char isEmpty() { return(  numActiveElements == 0 ) ; }
private:
   short numActiveElements ;
   short maxElements ;

   // the item array is 'compressed' in that all blank entries are at the
   // end.  In order for allowing the compression of these values while
   // still allowing old 'index' values to remain valid, the index values
   // the users have are indexes into the handleArray which is not in
   // compressed format.  Empty itemArray entries contain corresponding
   // 'blank' entries into the handleArray which are used for them.

   Array5index<T> *itemArray ;   // array of handles to the item pointers

   Array5index<T> **handleArray ;
   short lastHandle ;  // entry 0 of the arrays is not used.
} ;

#ifdef S4SERVER
class Source5all ;
class Table5serverBase ;

class Array5tables
{
public:
   Array5tables( Session5 *sessn ) { session = sessn ; }
   short add( Table5serverBase *item ) { return tables.add( item ) ; }
   void freeAll() ;
   Table5serverBase *get( short index ) ;
   Table5serverBase *remove( short index ) ;
   Session5 *session ;
   inline short top() { return tables.top() ; }
   inline short next( short index ) { return tables.next( index ) ; }
private:
   Array5<Table5serverBase> tables ;
} ;
#endif
