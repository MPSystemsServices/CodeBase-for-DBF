#include "oledb5.hpp"

#ifdef OLEDB5BUILD

/*
template<class T>Array5<T>::~Array5(void)
{
   if ( itemArray )
      free5( itemArray ) ;
   if ( handleArray )
      free5( handleArray ) ;
}

template<class T> T *Array5<T>::get( short index )
{
   return handleArray[index]->item ;
}

template<class T> void Array5<T>::freeAll()
{
   // removes and deletes every item on the array
   for( ;; )
   {
      T *item = pop() ;
      if ( item == NULL )
         break ;
      delete item ;
   }
}

template<class T> T *Array5<T>::remove( short index )
{
   T *retItem = itemArray[index].item ;
   short saveIndex = itemArray[lastHandle].index ;
   handleArray[itemArray[lastHandle].index] = &itemArray[index] ;
   itemArray[index].index = index ;
   itemArray[index].item = itemArray[lastHandle].item ;
   itemArray[lastHandle].index = saveIndex ;
   itemArray[lastHandle].item = NULL ;  // precautionary
   lastHandle-- ;
   numActiveElements-- ;
   return retItem ;
}
*/

/*
template<class T> short Array5<T>::add( T *item )
{
   if ( numActiveElements == maxElements )  // need to increase array size
   {
      short newSize = maxElements + ADDTOARRAY5 ;
      Array5index *newItemArray = (Array5index *)alloc5( newSize * sizeof( Array5index) ) ;
      Array5index **newHandleArray = (Array5index **)alloc5( newSize * sizeof( Array5index * ) ) ;

      memcpy( newItemArray, itemArray, maxElements * sizeof( Array5index ) ) ;
      memcpy( newHandleArray, handleArray, maxElements * sizeof( Array5index * ) ) ;

      free5( itemArray ) ;
      itemArray = newItemArray ;
      free5( handleArray ) ;
      handleArray = newHandleArray ;

      for ( loop = maxElements, loop < newSize ; loop++ )
      {
         handleArray[loop] = &itemArray[loop] ;
         itemArray[loop].index = loop ;
         itemArray[loop].item = NULL ;   // precautionary
      }

      maxElements = newSize ;
   }

   lastHandle++ ;
   numActiveElements++ ;
   short myHandle = itemArray[lastHandle].index ;

   itemArray[lastHandle].item = T ;

   return myHandle ;
}
*/

#ifdef S4SERVER
void Array5tables::freeAll()
{
   for( ;; )
   {
      Table5serverBase *item = tables.pop() ;
      if ( item == NULL )
         break ;
      delete item->table ;
      delete item ;
   }
   tables.freeAll() ;
}

Table5serverBase *Array5tables::remove( short index )
{
   switch( index )
   {
      // cannot remove the schema types
      case SCHEMA5ID_TABLES:
      case SCHEMA5ID_INDEXES:
      case SCHEMA5ID_COLUMNS:
         return NULL ;
      default:
         return tables.remove( index ) ;
   }
}

Table5serverBase *Array5tables::get( short index )
{
   switch( index )
   {
      case SCHEMA5ID_TABLES:
         return session->tablesSchema ;
      case SCHEMA5ID_INDEXES:
         return session->indexesSchema ;
      case SCHEMA5ID_COLUMNS:
         return session->columnsSchema ;
      default:
         return tables.get( index ) ;
   }
}
#endif /* S4SERVER */

#endif /* OLEDB5BUILD */
