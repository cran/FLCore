#include "FLCoreClasses_6.hpp"

#ifdef WIN32
   #define SEXPDLLExport __declspec(dllexport) SEXP __cdecl    
#else
   #define SEXPDLLExport SEXP    
#endif

extern "C" SEXPDLLExport TestFLBiol(SEXP xBiol)
   {
   SEXP ReturnObject = R_NilValue;
  
   FLBiol Biol(xBiol);

   for (int i=Biol.m.minquant(); i<=Biol.m.maxquant(); i++)
      Biol.m(i,Biol.m.minyr()) *=2.0;

   return Biol.Return();
   }

extern "C" SEXPDLLExport TestFLQuant(SEXP xQuant)
   {
   SEXP ReturnObject = R_NilValue;
  
   FLQuant Quant(xQuant);

   for (int i=Quant.minquant(); i<=Quant.maxquant(); i++)
      Quant(i,Quant.minyr()) *=2.0;

   return Quant.Return();
   }

extern "C" SEXPDLLExport TestFLQuant2(SEXP xQuant)
   {
   SEXP ReturnObject = R_NilValue;
  
   FLQuant2


 Quant(xQuant);

   return Quant.Return();
   }


extern "C" SEXPDLLExport TestFLStock(SEXP xStock)
   {
   SEXP ReturnObject = R_NilValue;
  
   FLStock stock(xStock);

   for (int i=stock.m.minquant(); i<=stock.m.maxquant(); i++)
      stock.m(i,stock.m.minyr()) *=2.0;

   return stock.Return();
   }

extern "C" SEXPDLLExport TestFLCatch(SEXP xCatch)
   {
   SEXP ReturnObject = R_NilValue;
  
   FLCatch Catch(xCatch);

   for (int i=Catch.catch_n.minquant(); i<=Catch.catch_n.maxquant(); i++)
      Catch.catch_n(i,Catch.catch_n.minyr()) *=2.0;

   return Catch.Return();
   }

extern "C" SEXPDLLExport TestFLFleet(SEXP xFleet)
   {
   SEXP ReturnObject = R_NilValue;
  
   FLFleet Fleet(xFleet);

   return Fleet.Return();
   }
