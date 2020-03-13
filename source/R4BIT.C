/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* r4bit.c   (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"

#ifndef S4OFF_REPORT
#ifdef S4WINDOWS

#define DIB_HEADER_MARKER   ((WORD) ('M' << 8) | 'B')
#define BYTES_PER_READ  32767


#define WIDTHBYTES(bits)      (((bits) + 31) / 32 * 4)
#define IS_WIN30_DIB(lpbi)  ((*(LPDWORD) (lpbi)) == sizeof (BITMAPINFOHEADER))


//DWORD PASCAL kwrite ( int, VOID FAR *, DWORD );

WORD S4FUNCTION bmp4DIBNumColors(LPBITMAPINFOHEADER lpbi)
{
   WORD wBitCount;


      /* If this is a Windows style DIB, the number of colors in the  */
      /*  color table can be less than the number of bits per pixel   */
      /*  allows for (i.e. lpbi->biClrUsed can be set to some value). */
      /*  If this is the case, return the appropriate value.          */

   if (IS_WIN30_DIB (lpbi))
      {
      DWORD dwClrUsed;

      dwClrUsed = ((LPBITMAPINFOHEADER) lpbi)->biClrUsed;

      if (dwClrUsed)
         return (WORD) dwClrUsed;
      }


      /* Calculate the number of colors in the color table based on */
      /*  the number of bits per pixel for the DIB.                 */

   if (IS_WIN30_DIB (lpbi))
      wBitCount = ((LPBITMAPINFOHEADER) lpbi)->biBitCount;
   else
      wBitCount = ((LPBITMAPCOREHEADER) lpbi)->bcBitCount;

   switch (wBitCount)
      {
      case 1:
         return 2;

      case 4:
         return 16;

      case 8:
         return 256;

      default:
         return 0;
      }
}


WORD S4FUNCTION bmp4PaletteSize(LPBITMAPINFOHEADER lpbi)
{
   if (IS_WIN30_DIB (lpbi))
      return (bmp4DIBNumColors(lpbi) * sizeof(RGBQUAD));
   else
      return (bmp4DIBNumColors(lpbi) * sizeof(RGBTRIPLE));
}

LPVOID S4FUNCTION bmp4FindDIBBits(LPBITMAPINFO lpbi)
{
   return (((char *)lpbi) + lpbi->bmiHeader.biSize + bmp4PaletteSize(&(lpbi->bmiHeader)));
}



//HANDLE S4FUNCTION bmp4GetDIB ( LPSTR fname, CODE4 *codeBase )
PBITMAPINFO S4FUNCTION bmp4GetDIB( LPSTR fname, CODE4 *codeBase )
{
   //int      hFile, errOpen;
   int oldErrOpen, oldReadOnly;
   //HANDLE hFile;
   //OFSTRUCT ofs;
   PBITMAPINFO pDIB = 0;  // CS 2001/06/13
   char fname2[14];
   FILE4 file;
   int rc;

   SetCursor(LoadCursor((HINSTANCE)NULL, IDC_WAIT));

   oldErrOpen = codeBase->errOpen;
   codeBase->errOpen = 0;

   oldReadOnly = codeBase->readOnly;
   codeBase->readOnly = 1;

   u4namePiece( fname2, 14, fname, 0, 1 );  // strip path from fname and put in fname2
   rc = file4open( &file, codeBase, fname2, 1 );
   if (rc != r4success)
      rc = file4open( &file, codeBase, fname, 1 );

   if (rc == r4success )
   {
      pDIB = bmp4ReadDIBFile(&file);
      file4close(&file);
   }
   else
      error4set( codeBase, 0 );

   SetCursor(LoadCursor((HINSTANCE)NULL, IDC_ARROW));
   return pDIB;
}

/*************************************************************************

  Function:  MyRead (int, LPSTR, DWORD)

   Purpose:  Routine to read files greater than 64K in size.

   Returns:  TRUE if successful.
             FALSE if an error occurs.

  Comments:

   History:   Date     Reason

             6/1/91    Created

*************************************************************************/

/*BOOL MyRead (int hFile, LPSTR lpBuffer, DWORD dwSize)
{
   #ifndef S4WIN32
      char huge *lpInBuf = (char huge *)lpBuffer;
   #else
      char *lpInBuf = (char *)lpBuffer;
   #endif
   int nBytes;
   FILE4SEQ_READ fseq;

   file4seqReadInit(&fseq, hFile, 0

   while (dwSize)
   {
      nBytes = (int) (dwSize > (DWORD) BYTES_PER_READ ? BYTES_PER_READ : LOWORD (dwSize));

      if (_lread (hFile, (LPSTR) lpInBuf, nBytes) != (WORD) nBytes)
         return FALSE;

      dwSize  -= nBytes;
      lpInBuf += nBytes;
   }

   return TRUE;
}*/


/*************************************************************************

  Function:  ReadDIBFile (int)

   Purpose:  Reads in the specified DIB file into a global chunk of
             memory.

   Returns:  A handle to a dib (hDIB) if successful.
             NULL if an error occurs.

  Comments:  BITMAPFILEHEADER is stripped off of the DIB.  Everything
             from the end of the BITMAPFILEHEADER structure on is
             returned in the global memory handle.

   History:   Date      Author      Reason

             6/1/91    Created
             6/27/91   Removed PM bitmap conversion routines.
             6/31/91   Removed logic which overallocated memory
                       (to account for bad display drivers).
            11/08/91   Again removed logic which overallocated
                       memory (it had creeped back in!)

*************************************************************************/

//HANDLE S4FUNCTION bmp4ReadDIBFile (int hFile)
PBITMAPINFO S4FUNCTION bmp4ReadDIBFile(FILE4 *hFile)  // CS 2000/06/25
{
   BITMAPFILEHEADER bmfHeader;
   DWORD dwBitsSize;
   //HANDLE hDIB;
   PBITMAPINFO pDIB;
   unsigned int lenRead;

   /* get length of DIB in bytes for use when reading */
   //#ifdef S4WIN32
     //dwBitsSize = file4longGetLo( u4filelength((HANDLE)hFile) ) ;
   //#else
     //dwBitsSize = u4filelength (hFile);
   //#endif
   dwBitsSize = file4len(hFile);
   /* Go read the DIB file header and check if it's valid. */

   //_lread (hFile, (LPSTR) &bmfHeader, sizeof (bmfHeader))
   lenRead = file4read(hFile, 0, &bmfHeader, sizeof(bmfHeader));
   if ((lenRead != sizeof (bmfHeader)) || bmfHeader.bfType != DIB_HEADER_MARKER)
      return 0;

   /* Allocate memory for DIB */

   /*hDIB = GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, dwBitsSize - sizeof(BITMAPFILEHEADER));
   if (hDIB == 0)
      return (HANDLE)INVALID_HANDLE_VALUE;

   pDIB = (LPSTR)GlobalLock(hDIB);*/

   pDIB = (PBITMAPINFO)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, dwBitsSize - sizeof(BITMAPFILEHEADER));
   if (pDIB == 0)
      return 0;

   /* Go read the bits. */

   lenRead = file4read(hFile, sizeof(bmfHeader), pDIB, dwBitsSize - sizeof(bmfHeader) );
   // LY Dec 1/04 : corrected length comparison to dwBitsSize - sizeof(bmfHeader)
   if ( lenRead != (dwBitsSize - sizeof(bmfHeader)) )
      return 0;

   /*if (!MyRead(hFile, pDIB, dwBitsSize - sizeof(BITMAPFILEHEADER)))
   {
      GlobalUnlock(hDIB);
      GlobalFree(hDIB);
      return INVALID_HANDLE_VALUE;
   }*/

   //GlobalUnlock(hDIB);
   return pDIB;
}

/****************************************************************************

 FUNCTION   : lwrite(int fh, VOID FAR *pv, DWORD ul)

 PURPOSE    : Writes data in steps of 32k till all the data is written.

 RETURNS    : 0 - If write did not proceed correctly.
                 number of bytes written otherwise.

 ****************************************************************************/
/*DWORD PASCAL kwrite(int fh, VOID FAR *pv, DWORD ul)
{
   DWORD     ulT = ul;
   #ifndef S4WIN32
      BYTE huge *hp = (unsigned char huge *)pv;
   #else
      BYTE *hp = (BYTE*)pv;
   #endif
   while (ul > BYTES_PER_READ)
   {
      if (_lwrite(fh, (LPSTR)hp, (WORD)BYTES_PER_READ) != BYTES_PER_READ)
                   return 0;

      ul -= BYTES_PER_READ;
      hp += BYTES_PER_READ;
   }

   if (_lwrite(fh, (LPSTR)hp, (WORD)ul) != (WORD)ul)
      return 0;

   return ulT;
}*/


/****************************************************************************

 FUNCTION   : WriteDIB(LPSTR szFile,HANDLE hdib)

 PURPOSE    : Write a global handle in CF_DIB format to a file.

 RETURNS    : TRUE  - if successful.
              FALSE - otherwise

 ****************************************************************************/
//BOOL S4FUNCTION bmp4WriteDIB (CODE4 *c4, LPSTR szFile, HANDLE hdib)
BOOL S4FUNCTION bmp4WriteDIB (CODE4 *c4, LPSTR szFile, LPVOID pDIB)
{
   BITMAPFILEHEADER hdr;
   LPBITMAPINFOHEADER lpbi = (LPBITMAPINFOHEADER)pDIB;
   //int                 fh;
   //OFSTRUCT            of;
   FILE4 fh;
   char oldSafety;
   int rc;

   if (!pDIB)
      return FALSE;

   /* fh = OpenFile (szFile, &of, OF_CREATE|OF_READWRITE);
    if (fh == -1)
        return FALSE;*/

   oldSafety = c4->safety;
   c4->safety = 0;
   rc = file4create(&fh, c4, szFile, 1);
   c4->safety = oldSafety;
   if (rc != r4success)
      return false;

    //lpbi = (LPBITMAPINFOHEADER)GlobalLock (hdib);

    /* Fill in the fields of the file header */
    hdr.bfType          = DIB_HEADER_MARKER;
    hdr.bfSize          = sizeof(BITMAPFILEHEADER) + HeapSize(GetProcessHeap(), 0, pDIB);
    hdr.bfReserved1     = 0;
    hdr.bfReserved2     = 0;
    hdr.bfOffBits       = (DWORD)sizeof(BITMAPFILEHEADER) + lpbi->biSize +
                          bmp4PaletteSize(lpbi);

    /* Write the file header */
    /*if (!_lwrite (fh, (LPSTR)&hdr, sizeof (BITMAPFILEHEADER)))
      {
      GlobalUnlock (hdib);
      _lclose (fh);
      return FALSE;
      }*/
   if (file4write(&fh, 0, &hdr, sizeof(BITMAPFILEHEADER)) != r4success)
   {
      error4set(c4, 0);
      file4close(&fh);
      return FALSE;
   }

   /* Write the DIB header and the bits */
    /*if (!kwrite (fh, (LPSTR)lpbi, GlobalSize (hdib)))
      {
      GlobalUnlock (hdib);
      _lclose (fh);
      return FALSE;
      }*/
   if (file4write(&fh, sizeof(BITMAPFILEHEADER), lpbi, HeapSize(GetProcessHeap(), 0, pDIB)) != r4success)
   {
      error4set(c4, 0);
      file4close(&fh);
      return FALSE;
   }

    /*GlobalUnlock (hdib);
    _lclose (fh);*/
   file4close(&fh);
   return TRUE;
}
#endif
#endif   /* S4OFF_REPORT */
