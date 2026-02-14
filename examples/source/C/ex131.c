/*ex131.c*/
#include <d4all.h>

int reindexDone;

short __stdcall cback(double p)
{
   printf("%f\n",p);
   if (p >= 1.0)
      reindexDone = 1;
   return 0;
}

int main(void)
{
   CODE4 cb;
   DATA4 *data;
   unsigned short actionCode;

   code4init(&cb);
   data = d4open(&cb,"student");

   reindexDone = 0;
   d4reindexWithProgress(data,cback,200);
   while (!reindexDone)
   {
      Sleep(0);
   }

   Sleep(210);  /* Make sure example does not exit before final callback. */
   return code4initUndo(&cb);
}