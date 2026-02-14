//ex190.cpp
#include <d4all.hpp>

int reindexDone;

short __stdcall cback(double p)
{
   printf("%f\n",p);
   return 0;
}

int main(void)
{
   Code4 cb;
   Data4 data;
   unsigned short actionCode;

   data.open(cb,"student");

   reindexDone = 0;
   data.reindexWithProgress(cback,200);
   while (!reindexDone)
   {
      Sleep(0);
   }

   Sleep(210);  // Make sure example does not exit before final callback.
   return cb.initUndo();
}
