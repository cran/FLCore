#include <math.h>
#include <float.h>
#include <string.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "FLCoreClasses.hpp"

int             outofbounds_int;
bool            outofbounds_bool;
double          outofbounds_double;
FLRConst_Target outofbounds_target;
FLRConst_SRR    outofbounds_FLRConst_SRR;


void InputAgeRange(SEXP obj, int *MinAge, int *MaxAge)
   {
   SEXP names = GET_NAMES(obj);
	  
   double *a  = NUMERIC_POINTER(obj);

   int n = length(obj);

   for (int i=0; i<n; i++)
      {
      char *s = CHAR(VECTOR_ELT(names, i));

      if (      strcmp(s, "min")==0)
         *MinAge     = (int)((a)[i]);
      else  if (strcmp(s, "max")==0)
         *MaxAge     = (int)((a)[i]);
      }

  UNPROTECT(1);
  }

void InputRange(SEXP obj, int *MinAge, int *MaxAge, int *PlusGroup, int *MinYear, int *MaxYear)
   {
   SEXP names = GET_NAMES(obj);
	  
   double *a  = NUMERIC_POINTER(obj);

   int n = length(obj);

   for (int i=0; i<n; i++)
      {
      char *s = CHAR(VECTOR_ELT(names, i));

      if (      strcmp(s, "min")==0)
         *MinAge     = (int)((a)[i]);
      else  if (strcmp(s, "max")==0)
         *MaxAge     = (int)((a)[i]);
      else  if (strcmp(s, "plusgroup")==0)
         *PlusGroup  = (int)((a)[i]);
      else  if (strcmp(s, "minyear")==0)
         *MinYear    = (int)((a)[i]);
      else  if (strcmp(s, "maxyear")==0)
         *MaxYear    = (int)((a)[i]);
      }

  UNPROTECT(1);
  }

void InputRange(SEXP obj, int *MinAge, int *MaxAge, int *PlusGroup, int *MinYear, int *MaxYear, int *MinFBarAge, int *MaxFBarAge)
   {
   SEXP names = GET_NAMES(obj);
	  
   double *a  = NUMERIC_POINTER(obj);

   int n = length(obj);

   for (int i=0; i<n; i++)
      {
      char *s = CHAR(VECTOR_ELT(names, i));

      if (      strcmp(s, "min")==0)
         *MinAge     = (int)((a)[i]);
      else  if (strcmp(s, "max")==0)
         *MaxAge     = (int)((a)[i]);
      else  if (strcmp(s, "plusgroup")==0)
         *PlusGroup  = (int)((a)[i]);
      else  if (strcmp(s, "minyear")==0)
         *MinYear    = (int)((a)[i]);
      else  if (strcmp(s, "maxyear")==0)
         *MaxYear    = (int)((a)[i]);
      else  if (strcmp(s, "fbar.min")==0)
         *MinFBarAge = (int)((a)[i]);
      else  if (strcmp(s, "fbar.max")==0)
         *MaxFBarAge = (int)((a)[i]);
      }

  UNPROTECT(1);
  }

SEXP CreateRange(int min, int max, int plusgroup, int minyr, int maxyr)
   {
    SEXP v, 
         d1,  
         dim,
         dimnames;    

    //Create new S4 object    
    v = PROTECT(NEW_NUMERIC(5)); 
  
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 1));       
    INTEGER(dim)[0] = 5;
        
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 1));
    
    PROTECT(d1 = allocVector(STRSXP, 5));
    
    SET_STRING_ELT(d1, 0, mkChar("min"));
    SET_STRING_ELT(d1, 1, mkChar("max"));
    SET_STRING_ELT(d1, 2, mkChar("plusgroup"));
    SET_STRING_ELT(d1, 3, mkChar("minyr"));
    SET_STRING_ELT(d1, 4, mkChar("maxyr"));
    
    setAttrib(v, install("names"), d1);
     
    //Set data
    REAL(v)[0] = min;
    REAL(v)[1] = max;
    REAL(v)[2] = plusgroup;
    REAL(v)[3] = minyr;
    REAL(v)[4] = maxyr;
           
    
    UNPROTECT(4);
    
    return v;
    }


void InputRange(SEXP obj, int *MinAge, int *MaxAge, int *PlusGroup, int *MinYear, int *MaxYear, double *Start, double *End)
   {
   SEXP names = GET_NAMES(obj);
	  
   double *a  = NUMERIC_POINTER(obj);

   int n = length(obj);

   for (int i=0; i<length(names); i++)
      {
      char *s = CHAR(VECTOR_ELT(names, i));

      if (      strcmp(s, "min")==0)
         *MinAge     = (int)((a)[i]);
      else  if (strcmp(s, "max")==0)
         *MaxAge     = (int)((a)[i]);
      else  if (strcmp(s, "plusgroup")==0)
         *PlusGroup  = (int)((a)[i]);
      else  if (strcmp(s, "minyear")==0)
         *MinYear    = (int)((a)[i]);
      else  if (strcmp(s, "maxyear")==0)
         *MaxYear    = (int)((a)[i]);
      else  if (strcmp(s, "start")==0)
         *Start      = (a)[i];
      else  if (strcmp(s, "end")==0)
         *End        = (a)[i];
      }
   
      if (n<length(names))
         {
         if (length(names) >= 1) 
            *MinAge     = (int)((a)[0]);
         if (length(names) >= 2) 
            *MaxAge     = (int)((a)[1]);
         if (length(names) >= 3) 
            *PlusGroup  = (int)((a)[2]);
         if (length(names) >= 4) 
            *MinYear    = (int)((a)[3]);
         if (length(names) >= 5) 
            *MaxYear    = (int)((a)[4]);
         if (length(names) >= 6) 
            *MaxYear    = (int)(a)[5];
         if (length(names) >= 7) 
            *MaxYear    = (int)(a)[6];
         }

  UNPROTECT(1);
  }

int NElemList(SEXP x)
   {
   //Check that it is a list
   if (!IS_LIST(x) || TYPEOF(x) != VECSXP) 
      return 0;
   else
      return length(x);
  }

bool isFLQuant(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLQuant")==0;
   }

bool isFLBiol(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLBiol")==0;
   }

bool isFLFleet(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLFleet")==0;
   }

bool isFLcatches(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLcatches")==0;
   }

bool isFLStock(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLStock")==0;
   }

bool isFLIndex(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndex")==0;
   }

bool isFLIndexCom(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndexCom")==0;
   }

bool isFLIndexSurvey(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndexSurvey")==0;
   }

bool isFLIndexAcoustic(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndexAcoustic")==0;
   }

bool isFLIndices(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLIndices")==0;
   }

bool isFLFleets(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLFleets")==0;
   }

bool isFLCatch(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLCatch")==0;
   }

bool isFLStocks(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLStocks")==0;
   }

bool isFLXSA(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLXSA")==0;
   }

bool isFLBRP(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLBRP")==0;
   }

bool isFLSR(SEXP t)
   {
   char *s = CHAR(STRING_ELT(GET_CLASS(t), 0));

   return strcmp(s, "FLSR")==0;
   }

FLQuant::FLQuant(void)      
    {
    InitFlag() = false;
    }

double& FLQuant::operator()(int _age,int _yr,int _unit, int _season, int _area, int _iter) { 
//   assert(_age<minquant || _age>maxquant || _yr<minyr || _yr>maxyr || _unit<1 ||  _unit>nunits || _season<1 || _season>nseasons || _area<1 || _area>nareas);
   return (data)[_age][_yr][_unit][_season][_area][_iter]; 
   } 

FLQuant::FLQuant(SEXP x)  
    {
    InitFlag() = false;

    if (isFLQuant(x))
       Init(x);
    }

FLQuant::FLQuant(int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)
   {
   InitFlag() = false;

   Init(_minquant,_maxquant,_minyr,_maxyr,_nunits,_nseasons,_nareas,_niters,val);   
   }

void FLQuant::Init(SEXP x, double val)      
   {
   Init(x);

   int iAge, iYear, iUnit, iSeason, iArea, iIter;
   for(iIter = 1; iIter <= niters(); iIter++) 
	   for (iArea = 1; iArea <= nareas(); iArea++)
     	   for (iSeason = 1; iSeason <= nseasons(); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(); iUnit++)
      		   for (iYear = minyr(); iYear <= maxyr(); iYear++)
		   		   for (iAge = minquant(); iAge <= maxquant(); iAge++)
			       			   data[iAge][iYear][iUnit][iSeason][iArea][iIter] = val;       
    }

void FLQuant::Init(SEXP x)      
    {
    if (InitFlag()) unalloc();

    SEXP Quant    = PROTECT(duplicate(GET_SLOT(x, install(".Data")))),
         dims     = GET_DIM(Quant),
         dimnames = GET_DIMNAMES(Quant);

    double *Q     = NUMERIC_POINTER(AS_NUMERIC(Quant));

    int dim[6], n = length(dims);

    dim[0] = INTEGER(dims)[0];
    dim[1] = INTEGER(dims)[1];
    dim[2] = INTEGER(dims)[2];
    dim[3] = INTEGER(dims)[3];
    dim[4] = INTEGER(dims)[4];
    dim[5] = n>=6 ? INTEGER(dims)[5] : 1; 
      
    if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
        ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
      {
      UNPROTECT(1);

      return;
      }

    minquant() = 0;
    minyr()    = 0;
    maxquant() = (int)dim[0] -1;
    maxyr()    = (int)dim[1] -1;
    nunits()   = (int)dim[2];
    nseasons() = (int)dim[3];
    nareas()   = (int)dim[4]; 
    niters()   = (int)dim[5];
	   
      
    if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int  t = 0;
         char *c;
         
         if (n >= 1 && INTEGER(dims)[0] >= 1) 
            {
            c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 

            minquant() += t;
            maxquant() += t;
  	         }
		   
         if (n >= 2 && INTEGER(dims)[1] >= 1) 
            {
            t = 0;
            c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 
            
            minyr()   += t;
            maxyr()   += t;
 	      	}
		   }


   alloc();

   int iAge, iYear, iUnit, iSeason, iArea, iIter, i=0; 
   for(iIter = 1; iIter <= niters(); iIter++)
	   for (iArea = 1; iArea <= nareas(); iArea++)
	    	for (iSeason = 1; iSeason <= nseasons(); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(); iUnit++)
		         for (iYear = minyr(); iYear <= maxyr(); iYear++)
			   		   for (iAge = minquant(); iAge <= maxquant(); iAge++)
			       			   data[iAge][iYear][iUnit][iSeason][iArea][iIter] = (Q)[i++];       
                   
    UNPROTECT(1);
    }

void FLQuant::Init(int _minquant, int _maxquant, int _minyr, int _maxyr, int _nunits, int _nseasons, int _nareas, int _niters, double val)      
    {
    if (InitFlag()) unalloc();

  	 int dim[6];

    dim[0] = _maxquant-_minquant+1;
    dim[1] = _maxyr-_minyr+1;
    dim[2] = _nunits;
    dim[3] = _nseasons;
    dim[4] = _nareas;
    dim[5] = _niters;

    if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
        ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || ((int)dim[4]) < 1 || ((int)dim[5]) < 1)
      return;

   minquant() = _minquant;
   maxquant() = _maxquant;
   minyr()    = _minyr;
   maxyr()    = _maxyr;
   nunits()   = _nunits;
   nareas()   = _nareas;
   nseasons() = _nseasons;
   niters()   = _niters;

   alloc();

   int iAge, iYear, iUnit, iSeason, iArea, iIter; 
   for(iIter = 1; iIter <= niters(); iIter++)
	   for (iArea = 1; iArea <= nareas(); iArea++)
	    	   for (iSeason = 1; iSeason <= nseasons(); iSeason++)
		 	   for (iUnit = 1; iUnit <= nunits(); iUnit++)
		      		   for (iYear = minyr(); iYear <= maxyr(); iYear++)
			   		   for (iAge = minquant(); iAge <= maxquant(); iAge++)
			       			   data[iAge][iYear][iUnit][iSeason][iArea][iIter] = val;       
    }

//Only return 5 dim array 
SEXP FLQuant::Return(void)      
    {
    SEXP Quant, v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    int i, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    

    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, FLQUANT_NDIMS));       
    INTEGER(dim)[0] = maxquant()-minquant() +1;
    INTEGER(dim)[1] = maxyr()   -minyr()    +1;
    INTEGER(dim)[2] = nunits(); 
    INTEGER(dim)[3] = nseasons(); 
    INTEGER(dim)[4] = nareas(); 

#ifdef FLCORE_2
     INTEGER(dim)[5] = niters();
#endif
        
    //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, FLQUANT_NDIMS));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant()-minquant() +1));
    for (iAge=minquant(), i=0; iAge<=maxquant(); iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr()-minyr()+1));
    for (iYear=minyr(), i=0; iYear<=maxyr(); iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits()==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits()));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits()));
       for (iUnit=1, i=0; iUnit<=nunits(); iUnit++, i++)
          INTEGER(d3)[i] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons()==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons()));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons()));
       for (iSeason=1, i=0; iSeason<=nseasons(); iSeason++, i++)
          INTEGER(d4)[i] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas()==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas()));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas()));
       for (iArea=1, i=0; iArea<=nareas(); iArea++, i++)
          INTEGER(d5)[i] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

#ifdef FLCORE_2
    PROTECT(d6 = allocVector(INTSXP, niters()));
    for (iIter=1, i=0; iIter<=niters(); iIter++, i++)
        INTEGER(d6)[i] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
#endif
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
#ifdef FLCORE_2
    SET_STRING_ELT(names, 5, mkChar("iter")); 
#endif
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    i=0;
    for(iIter = 1; iIter <= niters(); iIter++)
	    for (iArea = 1; iArea <= nareas(); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(); iUnit++)
	    		    for (iYear = minyr(); iYear <= maxyr(); iYear++)
			 		    for (iAge = minquant(); iAge <= maxquant(); iAge++)
			      			    REAL(v)[i++] = data [iAge][iYear][iUnit][iSeason][iArea][iIter]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

#ifdef FLCORE_2
    UNPROTECT(11);
#else
    UNPROTECT(10);
#endif
    
    return Quant;
    }

void FLQuant::unalloc(void)      
   {
   if (!InitFlag()) return;

   for (int iAge  = minquant(); iAge <= maxquant(); iAge++)
      {
      for (int iYear  = minyr(); iYear  <= maxyr();   iYear++)
         {
         for (int iUnit  = 1; iUnit    <= nunits();  iUnit++)
            {
            for (int iSeason  = 1; iSeason <= nseasons(); iSeason++) 
         		{
		         for (int iArea = 1; iArea <= nareas(); iArea++) 
                   free((char * ) (data[iAge][iYear][iUnit][iSeason][iArea]+1));
     
            	free((char * ) (data[iAge][iYear][iUnit][iSeason]+1));
            	}
	         free((char * ) (data[iAge][iYear][iUnit]+1));
	         }
         free((char * ) (data[iAge][iYear]+1));
         }
      free((char * ) (data[iAge]+minyr()));
      }
   free((char * ) (data+minquant()));
   
   InitFlag()=false;
   }                               

void FLQuant::alloc(void)      
   {
   if (InitFlag()) unalloc();

   int i = 0,j = 0, k = 0, l = 0, m = 0;
   data=(double******) malloc((maxquant()-minquant()+1)*sizeof(double*****)) - minquant();
   for(i=minquant();i<=maxquant();i++) 
      {
      (data)[i]  =(double*****) malloc((maxyr()-minyr()+1)*sizeof(double****)) - minyr();
      for (j=minyr(); j<=maxyr(); j++)
         {
         (data)[i][j]  =(double****) malloc((nunits())*sizeof(double***)) - 1;
         for (k = 1; k<=nunits(); k++)
            {
            (data)[i][j][k]  =(double***) malloc((nseasons())*sizeof(double**)) - 1;
            for (l=1; l<=nseasons(); l++)
	             {
                (data)[i][j][k][l]  =(double**) malloc((nareas())*sizeof(double*)) - 1;
	             for (m=1; m<=nareas(); m++)
      		       (data)[i][j][k][l][m] = (double*) malloc((niters())*sizeof(double)) - 1; 
	            }   
            }
         }
      }                                                           
   
   InitFlag()=true;
   }

FLQuant::~FLQuant(void)      
   {
   unalloc();
   }                               

int& FLQuant::minquant()
   {
   return flq_minquant;
   }

int& FLQuant::maxquant()
   {
   return flq_maxquant;
   }

int& FLQuant::plusgrp()
   {
   return flq_plusgrp;
   }

int& FLQuant::minyr()
   {
   return flq_minyr;
   }

int& FLQuant::maxyr()
   {
   return flq_maxyr;
   }

int& FLQuant::nunits()
   {
   return flq_nunits;
   }

int& FLQuant::nseasons()
   {
   return flq_nseasons;
   }

int& FLQuant::nareas()
   {
   return flq_nareas;
   }
 
int& FLQuant::niters()
   {
   return flq_niters;
   }

bool& FLQuant::InitFlag()
   {
   return flq_InitFlag;
   }

int FLQuant::mindim(int i)
   {
   switch (i) {
      case 1:
         return flq_minquant;
      case 2:
         return flq_minyr;    
      case 3:
         return 1;
      case 4:
         return 1;
      case 5:
         return 1;
      }

   return 0;
   }

int FLQuant::maxdim(int i)
   {
   switch (i) {
      case 1:
         return flq_maxquant;
      case 2:
         return flq_maxyr;
      case 3:
         return flq_nunits;
      case 4:
         return flq_nseasons;
      case 5:
         return flq_nareas;
      }

   return 0;
   }

FLStock::FLStock(void)      
    {
    InitFlag = false;
    }

FLStock::FLStock(SEXP x)      
    {
    InitFlag = false;

    if (isFLStock(x) && !InitFlag)
       Init(x);
    }
   
void FLStock::Init(SEXP x)
   {
   minquant = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp  = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr    = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr    = (int)REAL(GET_SLOT(x, install("range")))[4];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   catch_.Init(      GET_SLOT(x, install("catch"))); 
   catch_n.Init(     GET_SLOT(x, install("catch.n"))); 
   catch_wt.Init(    GET_SLOT(x, install("catch.wt"))); 
   discards.Init(    GET_SLOT(x, install("discards"))); 
   discards_n.Init(  GET_SLOT(x, install("discards.n"))); 
   discards_wt.Init( GET_SLOT(x, install("discards.wt"))); 
   landings.Init(    GET_SLOT(x, install("landings"))); 
   landings_n.Init(  GET_SLOT(x, install("landings.n"))); 
   landings_wt.Init( GET_SLOT(x, install("landings.wt"))); 
   stock.Init(       GET_SLOT(x, install("stock"))); 
   stock_n.Init(     GET_SLOT(x, install("stock.n"))); 
   stock_wt.Init(    GET_SLOT(x, install("stock.wt"))); 
   m.Init(           GET_SLOT(x, install("m"))); 
   mat.Init(         GET_SLOT(x, install("mat"))); 
   harvest.Init(     GET_SLOT(x, install("harvest"))); 
   harvest_spwn.Init(GET_SLOT(x, install("harvest.spwn"))); 
   m_spwn.Init(      GET_SLOT(x, install("m.spwn"))); 

   
   //need to check seasons, areas & units
   }

FLStock::~FLStock(void)      
   {
   ; //unalloc();
   }                               

SEXP FLStock::Return(void)
   {
   SEXP Stock, Range;

   PROTECT(Stock  = NEW_OBJECT(MAKE_CLASS("FLStock")));
   Range          = PROTECT(NEW_NUMERIC(5)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
       
   SET_SLOT(Stock, install("catch"),       catch_.Return());
   SET_SLOT(Stock, install("catch.n"),     catch_n.Return());
   SET_SLOT(Stock, install("catch.wt"),    catch_wt.Return());
   SET_SLOT(Stock, install("discards"),    discards.Return());
   SET_SLOT(Stock, install("discards.n"),  discards_n.Return());
   SET_SLOT(Stock, install("discards.wt"), discards_wt.Return());
   SET_SLOT(Stock, install("landings"),    landings.Return());
   SET_SLOT(Stock, install("landings.n"),  landings_n.Return());
   SET_SLOT(Stock, install("landings.wt"), landings_wt.Return());
   SET_SLOT(Stock, install("stock"),       stock.Return());
   SET_SLOT(Stock, install("stock.n"),     stock_n.Return());
   SET_SLOT(Stock, install("stock.wt"),    stock_wt.Return());
   SET_SLOT(Stock, install("mat"),         mat.Return());
   SET_SLOT(Stock, install("harvest"),     harvest.Return()); 
   SET_SLOT(Stock, install("harvest.spwn"),harvest_spwn.Return());
   SET_SLOT(Stock, install("m"),           m.Return()); 
   SET_SLOT(Stock, install("m.spwn"),      m_spwn.Return());
   SET_SLOT(Stock, install("range"),       Range);
      
   UNPROTECT(2);

   return Stock;
   }
   
FLIndex::FLIndex(void)      
    {
    InitFlag = false;
    }

FLIndex::FLIndex(SEXP x)      
    {
    InitFlag = false;

    if (isFLIndex(x) && !InitFlag)
       Init(x);
    }
   
void FLIndex::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   start      = (int)REAL(GET_SLOT(x, install("range")))[5];
   end        = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   index.Init(    0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);
   index_var.Init(0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);
   index.Init(    GET_SLOT(x, install("index"))); 
   index_var.Init(GET_SLOT(x, install("index"))); 
   
   //need to check seasons, areas, units & iterations
   }

FLIndex::~FLIndex(void)      
   {
   //unalloc();
   }                               

SEXP FLIndex::Return(void)
   {
   SEXP Index, Range;

   PROTECT(Index  = NEW_OBJECT(MAKE_CLASS("FLIndex")));
   Range          = PROTECT(NEW_NUMERIC(7)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = start;
   REAL(Range)[6] = end;
   
   SET_SLOT(Index, install("range"),      Range);
   SET_SLOT(Index, install("index"),      index.Return());
   SET_SLOT(Index, install("index.var"),  index_var.Return());
      
   UNPROTECT(2);

   return Index;
   }

FLIndexCom::FLIndexCom(void)      
    {
    InitFlag = false;
    }

FLIndexCom::FLIndexCom(SEXP x)      
    {
    InitFlag = false;

    if (isFLIndexCom(x) && !InitFlag)
       Init(x);
    }
   
void FLIndexCom::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   start      = (int)REAL(GET_SLOT(x, install("range")))[5];
   end        = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   catch_.Init(     0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);     
   catch_wt.Init(   0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);       
   effort.Init(     0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);  
   sel_pattern.Init(0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);      
   index_q.Init(    0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);            
   index.Init(      0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);    
   index_var.Init(  0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0); 

   catch_.Init(     GET_SLOT(x, install("index"))); 
   catch_wt.Init(   GET_SLOT(x, install("index"))); 
   effort.Init(     GET_SLOT(x, install("index"))); 
   sel_pattern.Init(GET_SLOT(x, install("index"))); 
   index_q.Init(    GET_SLOT(x, install("index"))); 
   index.Init(      GET_SLOT(x, install("index"))); 
   index_var.Init(  GET_SLOT(x, install("index"))); 
   
   //need to check seasons, areas, units & iterations
   }

FLIndexCom::~FLIndexCom(void)      
   {
   //unalloc();
   }                               

SEXP FLIndexCom::Return(void)
   {
   SEXP Index, Range;

   PROTECT(Index  = NEW_OBJECT(MAKE_CLASS("FLIndexCom")));
   Range          = PROTECT(NEW_NUMERIC(7)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = start;
   REAL(Range)[6] = end;
   
   SET_SLOT(Index, install("range"),      Range);
   
   SET_SLOT(Index, install("catch"),      catch_.Return());     
   SET_SLOT(Index, install("catch.wt"),   catch_wt.Return());       
   SET_SLOT(Index, install("effort"),     effort.Return());  
   SET_SLOT(Index, install("sel.pattern"),sel_pattern.Return());      
   SET_SLOT(Index, install("index.q"),    index_q.Return());            
   SET_SLOT(Index, install("index"),      index.Return());    
   SET_SLOT(Index, install("index.var"),  index_var.Return()); 
   
   UNPROTECT(2);

   return Index;
   }

FLIndexSurvey::FLIndexSurvey(void)      
    {
    InitFlag = false;
    }

FLIndexSurvey::FLIndexSurvey(SEXP x)      
    {
    InitFlag = false;

    if (isFLIndexSurvey(x) && !InitFlag)
       Init(x);
    }
   
void FLIndexSurvey::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   start      = (int)REAL(GET_SLOT(x, install("range")))[5];
   end        = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   catch_n.Init(    0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);     
   catch_wt.Init(   0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);       
   effort.Init(     0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);  
   index_q.Init(    0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);            
   index.Init(      0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);    
   index_var.Init(  0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0); 

   catch_n.Init(    GET_SLOT(x, install("index"))); 
   catch_wt.Init(   GET_SLOT(x, install("index"))); 
   effort.Init(     GET_SLOT(x, install("index"))); 
   index_q.Init(    GET_SLOT(x, install("index"))); 
   index.Init(      GET_SLOT(x, install("index"))); 
   index_var.Init(  GET_SLOT(x, install("index"))); 
   
   //need to check seasons, areas, units & iterations
   }

FLIndexSurvey::~FLIndexSurvey(void)      
   {
   //unalloc();
   }                               

SEXP FLIndexSurvey::Return(void)
   {
   SEXP Index, Range;

   PROTECT(Index  = NEW_OBJECT(MAKE_CLASS("FLIndexSurvey")));
   Range          = PROTECT(NEW_NUMERIC(7)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = start;
   REAL(Range)[6] = end;
   
   SET_SLOT(Index, install("range"),      Range);
   SET_SLOT(Index, install("catch.n"),    catch_n.Return());     
   SET_SLOT(Index, install("catch.wt"),   catch_wt.Return());       
   SET_SLOT(Index, install("effort"),     effort.Return());  
   SET_SLOT(Index, install("index.q"),    index_q.Return());            
   SET_SLOT(Index, install("index"),      index.Return());    
   SET_SLOT(Index, install("index.var"),  index_var.Return()); 
      
   UNPROTECT(2);

   return Index;
   }

FLIndexAcoustic::FLIndexAcoustic(void)      
    {
    InitFlag = false;
    }

FLIndexAcoustic::FLIndexAcoustic(SEXP x)      
    {
    InitFlag = false;

    if (isFLIndexAcoustic(x) && !InitFlag)
       Init(x);
    }
   
void FLIndexAcoustic::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   start      = (int)REAL(GET_SLOT(x, install("range")))[5];
   end        = (int)REAL(GET_SLOT(x, install("range")))[6];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   index_q.Init(    0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);            
   index.Init(      0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0);    
   index_var.Init(  0, 0, index.minyr(),index.maxyr(),index.nunits(),index.nseasons(),index.nareas(), index.niters(), 1.0); 

   index_q.Init(    GET_SLOT(x, install("index"))); 
   index.Init(      GET_SLOT(x, install("index"))); 
   index_var.Init(  GET_SLOT(x, install("index"))); 
   
   //need to check seasons, areas, units & iterations
   }

FLIndexAcoustic::~FLIndexAcoustic(void)      
   {
   //unalloc();
   }                               

SEXP FLIndexAcoustic::Return(void)
   {
   SEXP Index, Range;

   PROTECT(Index  = NEW_OBJECT(MAKE_CLASS("FLIndexAcoustic")));
   Range          = PROTECT(NEW_NUMERIC(7)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   REAL(Range)[5] = start;
   REAL(Range)[6] = end;
   
   SET_SLOT(Index, install("range"),      Range);
   SET_SLOT(Index, install("index.q"),    index_q.Return());            
   SET_SLOT(Index, install("index"),      index.Return());    
   SET_SLOT(Index, install("index.var"),  index_var.Return()); 
         
   UNPROTECT(2);

   return Index;
   }


FLBiol::FLBiol(void)      
    {
    InitFlag = false;
    }

FLBiol::FLBiol(SEXP x)      
    {
    InitFlag = false;

    if (isFLBiol(x) && !InitFlag)
       Init(x);
    }
   
void FLBiol::Init(SEXP x)
   {
   minquant   = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant   = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp    = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr      = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr      = (int)REAL(GET_SLOT(x, install("range")))[4];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   niters   = 1;

   n.Init(   GET_SLOT(x, install("n"))); 
   m.Init(   GET_SLOT(x, install("m"))); 
   wt.Init(  GET_SLOT(x, install("wt"))); 
   fec.Init( GET_SLOT(x, install("fec"))); 
   spwn.Init(GET_SLOT(x, install("spwn"))); 
   
   //need to check seasons, areas & units
   }

FLBiol::~FLBiol(void)      
   {
   ; //unalloc();
   }                               

SEXP FLBiol::Return(void)
   {
   SEXP Biol, Range;

   PROTECT(Biol  = NEW_OBJECT(MAKE_CLASS("FLBiol")));
   Range          = PROTECT(NEW_NUMERIC(5)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
       
   SET_SLOT(Biol, install("n"),       n.Return());
   SET_SLOT(Biol, install("m"),       m.Return());
   SET_SLOT(Biol, install("wt"),      wt.Return());
   SET_SLOT(Biol, install("fec"),     fec.Return());
   SET_SLOT(Biol, install("spwn"),    spwn.Return());

   SET_SLOT(Biol, install("range"),   Range);
      
   UNPROTECT(2);

   return Biol;
   }

FLSR::FLSR(void)      
    {
    InitFlag = false;
    }

FLSR::FLSR(SEXP x)      
    {
    InitFlag = false;

    if (isFLSR(x) && !InitFlag)
       Init(x);
    }
   
void FLSR::Init(SEXP x)
   {
   
   name  = CHAR(STRING_ELT(GET_SLOT(x,install("name")),0));
   desc  = CHAR(STRING_ELT(GET_SLOT(x,install("desc")),0));
   model = CHAR(STRING_ELT(GET_SLOT(x,install("model")),0));

   ssb.Init(      GET_SLOT(x, install("ssb"))); 
   rec.Init(    	  GET_SLOT(x, install("rec"))); 
   rechat.Init(     GET_SLOT(x, install("rechat"))); 
   residuals.Init(GET_SLOT(x, install("residuals"))); 
   
   params = REAL(GET_SLOT(x,install("params")));
   se     = REAL(GET_SLOT(x,install("se")));
   covar  = REAL(GET_SLOT(x,install("covar")));
   var    = REAL(GET_SLOT(x,install("var")))[0];
   vara   = REAL(GET_SLOT(x,install("vara")))[0];
   aic    = REAL(GET_SLOT(x,install("aic")))[0]; 
   
   }

FLSR::~FLSR(void)
    {
    // unalloc();
    }

SEXP FLSR::Return(int nparams)
   {

   SEXP SR;
   int i,j;
   PROTECT(SR  = NEW_OBJECT(MAKE_CLASS("FLSR")));
   SEXP rpar,rse,rcovar,rvar,rvara,raic;

   PROTECT(rpar   = allocMatrix(REALSXP, (int)nparams, (int)3));
   PROTECT(rse    = allocVector(REALSXP, (int)3));
   PROTECT(rcovar = allocMatrix(REALSXP, (int)3, int(3)));
   PROTECT(rvar   = allocVector(REALSXP,(int)1));
   PROTECT(rvara  = allocVector(REALSXP,(int)1)); 
   PROTECT(raic   = allocVector(REALSXP,(int)1)); 
   
   for(j=0;j<int(3);j++) {
	   for(i=0;i<nparams;i++) REAL(rpar)[i+j*nparams] = params[i+j*nparams];
   }
   for(i=0;i<(int)3;i++) REAL(rse)[i] = se[i];
   for(i=0;i<(int)3;i++) {
	   for(j=0;j<(int)3;j++) REAL(rcovar)[i+(int)3*j] = covar[i+(int)3*j];
   }
   REAL(rvar)[0]  = var;
   REAL(rvara)[0] = vara;
   REAL(raic)[0]  = aic;
   
   SET_SLOT(SR, install("ssb"),       ssb.Return());
   SET_SLOT(SR, install("rec"),         rec.Return());
   SET_SLOT(SR, install("rechat"),      rechat.Return());
   SET_SLOT(SR, install("residuals"), residuals.Return());
   SET_SLOT(SR, install("params"),    rpar); 
   SET_SLOT(SR, install("se"),        rse);
   SET_SLOT(SR, install("covar"),     rcovar);
   SET_SLOT(SR, install("var"),       rvar);
   SET_SLOT(SR, install("vara"),      rvara);
   SET_SLOT(SR, install("aic"),       raic); 
  
   UNPROTECT(7);

   return SR; 
   }
   
FLVector::FLVector(void)      
    {
    InitFlag = false;
    }

double& FLVector::operator()(int _i) 
   { 

   if (_i < mindim || _i > maxdim) 
      return outofbounds_double =0.0;
   
   return (data)[_i]; 
   } 

FLVector::FLVector(SEXP x)  
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }

void FLVector::Init(SEXP x)      
   {
   SEXP v;
   int n;

   if (!isVector(x) || !isNumeric(x)) 
      return;

   PROTECT(v = AS_NUMERIC(x));
   n = LENGTH(v);

   double *d = NUMERIC_POINTER(v); 

   SEXP names = GET_NAMES(v);
   if (LENGTH(names) == n) //index by name
      {
      //get indices
      mindim = atoi(CHAR(VECTOR_ELT(names, 0))); 
      maxdim = mindim + n - 1;

      //check indices
      for (int i=1; i<n; i++)
         if ((mindim+i) != atoi(CHAR(VECTOR_ELT(names, i))))
            return;
      }
   else
     {
     mindim = 1;
     maxdim = n;
     }
    
   alloc();
 
   for (int i=mindim; i<=maxdim; i++)
      data[i] = (d)[i - mindim];       
      
   UNPROTECT(1);

   return;
   }

void FLVector::Init(int _min, int _max, double val)      
   {
   if (_min > _max) return;

   mindim = _min;
   maxdim = _max;
    
   alloc();
 
   for (int i=mindim; i<=maxdim; i++)
      data[i] = val;       
      
   return;
   }

SEXP FLVector::Return(void)      
    {
    int i, j;
    
    SEXP v, 
         d1,  
         dim,
         dimnames;    

    //Create new S4 object    
    v = PROTECT(NEW_NUMERIC(maxdim-mindim+1)); 
  
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 1));       
    INTEGER(dim)[0] = maxdim-mindim+1;
        
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 1));
    
    PROTECT(d1 = allocVector(INTSXP, maxdim-mindim+1));
    for (j=mindim, i=0; j<=maxdim; j++, i++)
        INTEGER(d1)[i] = j; 
    SET_VECTOR_ELT(dimnames, 1, d1);
     
    //Set data
    for (j=mindim, i=0; j<=maxdim; j++, i++)
       REAL(v)[i] =data [j];
           
    
    UNPROTECT(5);
    
    return v;
    }

void FLVector::unalloc(void)      
   {
   if (!InitFlag) return;

   free((char * ) (data+mindim));
   
   InitFlag=false;
   }                               

void FLVector::alloc(void)      
   {
   if (InitFlag) unalloc();

   data=(double*) malloc((maxdim-mindim+1)*sizeof(double)) - mindim;
   }

FLVector::~FLVector(void)      
   {
   unalloc();
   }                               

FLBool::FLBool(void)      
    {
    InitFlag = false;
    }

bool& FLBool::operator()(int _i) 
   { 

   if (_i < mindim || _i > maxdim) 
      outofbounds_bool=false;

   return (data)[_i]; 
   } 

FLBool::FLBool(SEXP x)  
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }

void FLBool::Init(SEXP x)      
   {
   SEXP v;
   int n;

   if (!isLogical(x)) 
      return;

   PROTECT(v = AS_LOGICAL(x));
   n = LENGTH(v);

   int *d = LOGICAL_POINTER(v); 

   SEXP names = GET_NAMES(v);
   if (LENGTH(names) == n) //index by name
      {
      //get indices
      mindim = atoi(CHAR(VECTOR_ELT(names, 0))); 
      maxdim = mindim + n - 1;

      //check indices
      for (int i=1; i<n; i++)
         if ((mindim+i) != atoi(CHAR(VECTOR_ELT(names, i))))
            return;
      }
   else
     {
     mindim = 1;
     maxdim = n;
     }
    
   alloc();
 
   for (int i=mindim; i<=maxdim; i++)
      data[i] = (d)[i - mindim] <= 0 ? false : true;       
      
   UNPROTECT(1);

   return;
   }

SEXP FLBool::Return(void)      
    {
    int i, j;
    
    SEXP v, 
         d1,  
         dim,
         dimnames;    

    //Create new S4 object    
    v = PROTECT(NEW_NUMERIC(maxdim-mindim+1)); 
  
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 1));       
    INTEGER(dim)[0] = maxdim-mindim+1;
        
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 1));
    
    PROTECT(d1 = allocVector(INTSXP, maxdim-mindim+1));
    for (j=mindim, i=0; j<=maxdim; j++, i++)
        INTEGER(d1)[i] = j; 
    SET_VECTOR_ELT(dimnames, 1, d1);
     
    //Set data
    for (j=mindim, i=0; j<=maxdim; j++, i++)
       REAL(v)[i] =data [j];
           
    
    UNPROTECT(5);
    
    return v;
    }

void FLBool::unalloc(void)      
   {
   if (!InitFlag) return;

   free((char * ) (data+mindim));
   
   InitFlag=false;
   }                               

void FLBool::alloc(void)      
   {
   if (InitFlag) unalloc();

   data=(bool*) malloc((maxdim-mindim+1)*sizeof(bool)) - mindim;
   }

FLBool::~FLBool(void)      
   {
   unalloc();
   }                               


FL2D::FL2D(void)      
    {
    InitFlag = false;
    }

double& FL2D::operator()(int _i, int _j) 
   { 
   if (_i < min1 || _i > max1 || _j < min2 || _j > max2) 
      return outofbounds_double=0.0;

   return (data)[_i][_j]; 
   } 

int FL2D::mindim(int i) 
   { 
   switch (i) {
      case 1:
         return min1;
      case 2:
         return min2;
      }

   return outofbounds_int=0;
   }

int FL2D::maxdim(int i) 
   { 
   switch (i) {
      case 1:
         return max1;
      case 2:
         return max2;
      }

   return outofbounds_int=0;
   }

FL2D::FL2D(SEXP x)  
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }

void FL2D::Init(SEXP x)      
    {
    if (isMatrix(x)  && isNumeric(x))
       {
       SEXP dims     = GET_DIM(x),
            dimnames = GET_DIMNAMES(x);

       int dim[2], n = length(dims);

       if (n !=2) return;

       double *a     = NUMERIC_POINTER(x);

       dim[0] = INTEGER(dims)[0];
       dim[1] = INTEGER(dims)[1];
    
       min1  = 0;
       min2  = 0;
       max1  = (int)dim[0] -1;
       max2  = (int)dim[1] -1;

       if (dimnames != R_NilValue) 
         if (TYPEOF(dimnames) == VECSXP) 
            {
            int  t = 0;
            char *c;
             	   
            if (n >= 1 && INTEGER(dims)[0] >= 1) 
               {
               c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0));

               //check that name is not a text string
               for (int i=0; i<=(signed)strlen(c); i++)
                  if (isalpha(c[i])) t=1;

               if (t !=1)
	               t = atoi(c); 

               min1 += t;
               max1 += t;
  	            }
		      if (n >= 2 && INTEGER(dims)[1] >= 1) 
               {
	            c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0));

               //check that name is not a text string
               for (int i=0; i<=(signed)strlen(c); i++)
                  if (isalpha(c[i])) t=1;

               if (t !=1)
	               t = atoi(c); 
            
               min2 += t;
               max2 += t;
    	         }
		      }
		 
       alloc();

       int i1, i2,
             i,  j;

       for (i1 = min1, i = 0; i1 <= max1; i1++, i++)
          for (i2 = min2, j = 0; i2 <= max2; i2++, j++)
             data[i1][i2] = (a)[i + j*(max1-min1+1)];       
       }
    else if (isVector(x)  && isNumeric(x))
       {
       SEXP v;

       PROTECT(v = AS_NUMERIC(x));
       int n = LENGTH(v);

       double *d = NUMERIC_POINTER(v); 

       SEXP names = GET_NAMES(v);
       if (LENGTH(names) == n) //index by name
          {
          int  t = 0;
          char *c;
             	   
          c = CHAR(VECTOR_ELT(names, 0));

          //check that name is not a text string
          for (int i=0; i<=(signed)strlen(c); i++)
             if (isalpha(c[i])) t=1;

          if (t !=1)
             t = atoi(c); 

          min1 = t; 
 
          if (t==0)      
            min1 = atoi(CHAR(VECTOR_ELT(names, 0))); 
          else 
            min1 = t;

          max1 = min1 + n - 1;
          min2 = max2 = 1;
 
          //check indices
          //for (int i=1; i<n; i++)
          //   if ((min1+i) != atoi(CHAR(VECTOR_ELT(names, i))))
          //      return;
          }
       else
          {
          min1 = 1;
          max1 = n;
          min2 = max2 = 1;
          }
    
       alloc();
 
       int i1, i;
       for (i1 = min1, i = 0; i1 <= max1; i1++, i++)
          data[i1][1] = (d)[i];       

       UNPROTECT(1);
       }
    else
       return;
        
    return;
    }

SEXP FL2D::Return(void)      
    {
    int i, j, i_, j_;
    
    SEXP v, 
         d1, d2,  
         dim,   dimnames;    

    //Create new S4 object    
    PROTECT(v = NEW_OBJECT(MAKE_CLASS("array")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 2));       
    INTEGER(dim)[0] = max1 -min1 +1;
    INTEGER(dim)[1] = max2-min2+1;
        
    //Allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 2));
    
    PROTECT(d1 = allocVector(INTSXP, max1-min1 +1));
    for (i_=min1, i=0; i_<=max1; i_++, i++)
        INTEGER(d1)[i] = i_; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, max2-min2+1));
    for (j_=min2, i=0; j_<=max2; j_++, i++)
        INTEGER(d2)[i] = j_; 
    SET_VECTOR_ELT(dimnames, 1, d2);
    
    setAttrib(v, install("dimnames"), dimnames);
     
    //Set data
    for (i_=min1, i=0; i_<=max1; i_++, i++)
       for (j_=min2, j=0; j_<=max2; j_++, j++)
          REAL(v)[i + j*(max1-min1+1)] =data[i_][j_];
           
    UNPROTECT(6);
    
    return v;
    }

void FL2D::unalloc(void)      
   {
   if (!InitFlag) return;

   for (int i=min1; i<=max1; i++)
      free((char * ) (data[i] + min2));

   free((char * ) (data+min1));
  
   InitFlag=false;
   }                               

void FL2D::alloc(void)      
   {
   if (InitFlag) unalloc();

   int i = 0;
   data=(double**) malloc((max1-min1+1)*sizeof(double*)) - min1;
   for(i=min1;i<=max1;i++) 
      (data)[i]  =(double*) malloc((max2-min2+1)*sizeof(double)) - min2;
      
   }                                                           
   
FL2D::~FL2D(void)      
   {
   unalloc();
   }                               

FLQuant2::FLQuant2(void)   
   {
   flq_n7 = 0;
   }

FLQuant2::FLQuant2(int _n7)   
   {
   Init(_n7);
   }

FLQuant2::FLQuant2(SEXP x)
   {
   Init(x);
   }

FLQuant2::~FLQuant2(void)
   {
   unalloc();
   }

void FLQuant2::Init(int _n7)   
   {
   if (flq_n7>=1)
      unalloc();

   flq_n7 = _n7;

   flq_minquant=(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_maxquant=(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_minyr   =(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_maxyr   =(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_nunits  =(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_nseasons=(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_nareas  =(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_niters  =(int*)          malloc((flq_n7)*sizeof(int))           - 1;
   flq_InitFlag=(bool* )        malloc((flq_n7)*sizeof(bool))          - 1;
   data        =(double*******) malloc((flq_n7)*sizeof(double******))  - 1;

   for (int i=1; i<=flq_n7; i++)
      flq_InitFlag[i] = false;
   }

void FLQuant2::Init(SEXP x)
   {
   Init(NElemList(x));

   for (int i=1; i<=n7(); i++)
      Init(i, VECTOR_ELT(x, i-1));
   }

void FLQuant2::Init(int i7, SEXP x)
   {
   if (i7 > flq_n7 || i7 > flq_n7) return;
   
   if (flq_InitFlag[i7]) unalloc(i7);

   flq_InitFlag[i7] = false;

   SEXP Quant    = PROTECT(duplicate(GET_SLOT(x, install(".Data")))),
        dims     = GET_DIM(Quant),
        dimnames = GET_DIMNAMES(Quant);

   double *Q     = NUMERIC_POINTER(AS_NUMERIC(Quant));

  	int dim[6], n = length(dims);
    
   if (n>=1) dim[0] = INTEGER(dims)[0]; else dim[0] = 1;
   if (n>=2) dim[1] = INTEGER(dims)[1]; else dim[1] = 1;
   if (n>=3) dim[2] = INTEGER(dims)[2]; else dim[2] = 1;
   if (n>=4) dim[3] = INTEGER(dims)[3]; else dim[3] = 1;
   if (n>=5) dim[4] = INTEGER(dims)[4]; else dim[4] = 1;
   if (n>=6) dim[5] = INTEGER(dims)[5]; else dim[5] = 1;

   if (((int)dim[0]) <  1 || ((int)dim[1]) < 1 || 
       ((int)dim[2]) <  1 || ((int)dim[3]) < 1 || 
	    ((int)dim[4]) <  1 || ((int)dim[5]) < 1   )
     UNPROTECT(1);

   flq_minquant[i7] = 0;
   flq_maxquant[i7] = (int)dim[0] -1;
   flq_minyr[i7]    = 0;
   flq_maxyr[i7]    = (int)dim[1] -1;
   flq_nunits[i7]   = (int)dim[2];
   flq_nareas[i7]   = (int)dim[3];
   flq_nseasons[i7] = (int)dim[4];
   flq_niters[i7]   = (int)dim[5];

   if (dimnames != R_NilValue) 
      if (TYPEOF(dimnames) == VECSXP) 
         {
         int  t = 0;
         char *c;
         
         if (n >= 1 && INTEGER(dims)[0] >= 1) 
            {
            c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 0), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 

            flq_minquant[i7] += t;
            flq_maxquant[i7] += t;
  	         }
		   
         if (n >= 2 && INTEGER(dims)[1] >= 1) 
            {
            t = 0;
            c = CHAR(VECTOR_ELT(VECTOR_ELT(dimnames, 1), 0));

            //check that name is not a text string
            for (int i=0; i<=(signed)strlen(c); i++)
               if (isalpha(c[i])) t=1;

            if (t !=1)
	            t = atoi(c); 
            
            flq_minyr[i7]   += t;
            flq_maxyr[i7]   += t;
 	      	}
		   }
	   
   alloc(i7);
 
   int iIter, iAge, iYear, iUnit, iSeason, iArea, i=0; 
   for (iIter = 1; iIter <= flq_niters[i7]; iIter++)
	   for (iArea = 1; iArea <= flq_nareas[i7]; iArea++)
		  for (iSeason = 1; iSeason <= flq_nseasons[i7]; iSeason++)
			 for (iUnit = 1; iUnit <= flq_nunits[i7]; iUnit++)
				for (iYear = flq_minyr[i7]; iYear <= flq_maxyr[i7]; iYear++)
				   for (iAge = flq_minquant[i7]; iAge <= flq_maxquant[i7]; iAge++)
					  data[i7][iAge][iYear][iUnit][iSeason][iArea][iIter] = (Q)[i++];       
                  
   UNPROTECT(1);
   }

double& FLQuant2::operator()(int i, int _age,int _yr,int _unit, int _season, int _area, int _iter) { 
   if (i<1 || i>flq_n7 || _age<flq_minquant[i] || _age>flq_maxquant[i] || _yr<flq_minyr[i] || _yr>flq_maxyr[i] || _unit<1 ||  _unit>flq_nunits[i] || _season<1 || _season>flq_nseasons[i] || _area<1 || _area>flq_nareas[i] || _iter<1 || _iter>flq_niters[i])
      return outofbounds_double = 0.0;
   else   
      return (data)[i][_age][_yr][_unit][_season][_area][_iter]; 
   } 

void FLQuant2::alloc(int i7)
   {
   if (i7 < 1 || i7 > flq_n7) return;

   int i = 0, j = 0, k = 0, l = 0, m = 0;
   data[i7] =(double******) malloc((flq_maxquant[i7]-flq_minquant[i7]+1)*sizeof(double*****)) - flq_minquant[i7];
   for(i=flq_minquant[i7];i<=flq_maxquant[i7];i++) 
      {
      (data[i7])[i]  =(double*****) malloc((flq_maxyr[i7]-flq_minyr[i7]+1)*sizeof(double****)) - flq_minyr[i7];
      for (j=flq_minyr[i7]; j<=flq_maxyr[i7]; j++)
         {
         (data[i7])[i][j]  =(double****) malloc((flq_nunits[i7])*sizeof(double***)) - 1;
         for (k = 1; k<=flq_nunits[i7]; k++)
            {
            (data[i7])[i][j][k]  =(double***) malloc((flq_nseasons[i7])*sizeof(double**)) - 1;
            for (l=1; l<=flq_nseasons[i7]; l++)
	             {
                (data[i7])[i][j][k][l]  =(double**) malloc((flq_nareas[i7])*sizeof(double*)) - 1;
	             for (m=1; m<=flq_nareas[i7]; m++)
      		       (data[i7])[i][j][k][l][m] = (double*) malloc((flq_niters[i7])*sizeof(double)) - 1; 
	            }   
            }
         }
      }                                                           
   
   flq_InitFlag[i7]=true;
   }

void FLQuant2::unalloc(int i7)
   {
   if (i7 > flq_n7 || i7 <= 0 || flq_n7 <= 0) return;
     
   if (flq_InitFlag[i7])
      for (int iAge = flq_minquant[i7]; iAge <= flq_maxquant[i7]; iAge++)
         {
         for (int iYear = flq_minyr[i7]; iYear <= flq_maxyr[i7]; iYear++)
            {
            for (int iUnit = 1; iUnit <= flq_nunits[i7]; iUnit++)
                {
                for (int iSeason = 1; iSeason <= flq_nseasons[i7]; iSeason++)
                    {
                    for (int iArea = 1; iArea <= flq_nareas[i7]; iArea++)
                        free((char *) (data[i7][iAge][iYear][iUnit][iSeason][iArea]+1));
  
                   free((char *) (data[i7][iAge][iYear][iUnit][iSeason]+1));
                   }  
                free((char *) (data[i7][iAge][iYear][iUnit]+1));
                }
             free((char *) (data[i7][iAge][iYear]+1));
             }
         free((char *) (data[i7][iAge]+flq_minyr[i7]));
         }
      free((char *) (data[i7]+flq_minquant[i7]));
   
   flq_InitFlag[i7] = false;
   }
  
void FLQuant2::unalloc(void)
   {
   if (flq_n7 < 1) return;
   
   for (int i  = 1; i <= flq_n7; i++)
      unalloc(i);

   free((char *) (flq_minquant+1));
   free((char *) (flq_maxquant+1));
   free((char *) (flq_minyr   +1));
   free((char *) (flq_maxyr   +1));
   free((char *) (flq_nunits  +1));
   free((char *) (flq_nseasons+1));
   free((char *) (flq_nareas  +1));
   free((char *) (flq_InitFlag+1));
   
   flq_n7 = 0;
   }

//Only return 5 dim array 
SEXP FLQuant2::Return(int _i)      
    {
    SEXP Quant  = R_NilValue, 
         v, 
         d1, d2, d3, d4, d5, d6, 
         dim, dimnames, names;    

    if (_i > flq_n7 || _i < 1) return Quant;

    int i, iAge, iYear, iUnit, iArea, iSeason, iIter;

    //Create new S4 object    
    PROTECT(Quant = NEW_OBJECT(MAKE_CLASS("FLQuant")));

    //Create array for slot    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, FLQUANT_NDIMS));       
    INTEGER(dim)[0] = maxquant(_i)-minquant(_i) +1;
    INTEGER(dim)[1] = maxyr(_i)   -minyr(_i)    +1;
    INTEGER(dim)[2] = nunits(_i); 
    INTEGER(dim)[3] = nseasons(_i); 
    INTEGER(dim)[4] = nareas(_i); 
#ifdef FLCORE_2
    INTEGER(dim)[5] = niters();
#endif
        
    //allocate memory
    PROTECT(v = Rf_allocArray(REALSXP, dim)); 
    
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, FLQUANT_NDIMS));
    
    PROTECT(d1 = allocVector(INTSXP, maxquant(_i)-minquant(_i) +1));
    for (iAge=minquant(_i), i=0; iAge<=maxquant(_i); iAge++, i++)
        INTEGER(d1)[i] = iAge; 
    SET_VECTOR_ELT(dimnames, 0, d1);
    
    PROTECT(d2 = allocVector(INTSXP, maxyr(_i)-minyr(_i)+1));
    for (iYear=minyr(_i), i=0; iYear<=maxyr(_i); iYear++, i++)
        INTEGER(d2)[i] = iYear; 
    SET_VECTOR_ELT(dimnames, 1, d2);
     
    if (nunits(_i)==1)
       {
       PROTECT(d3 = allocVector(STRSXP, nunits(_i)));
       SET_STRING_ELT(d3, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d3 = allocVector(INTSXP, nunits(_i)));
       for (iUnit=1, i=0; iUnit<=nunits(_i); iUnit++, i++)
          INTEGER(d3)[i] = iUnit; 
       }
    SET_VECTOR_ELT(dimnames, 2, d3);
       
    if (nseasons(_i)==1)
       {
       PROTECT(d4 = allocVector(STRSXP, nseasons(_i)));
       SET_STRING_ELT(d4, 0, mkChar("all"));
       }
    else
       {
       PROTECT(d4 = allocVector(INTSXP, nseasons(_i)));
       for (iSeason=1, i=0; iSeason<=nseasons(_i); iSeason++, i++)
          INTEGER(d4)[i] = iSeason; 
       }
    SET_VECTOR_ELT(dimnames, 3, d4);
    

    if (nareas(_i)==1)
       {
       PROTECT(d5 = allocVector(STRSXP, nareas(_i)));
       SET_STRING_ELT(d5, 0, mkChar("unique"));
       }
    else
       {
       PROTECT(d5 = allocVector(INTSXP, nareas(_i)));
       for (iArea=1, i=0; iArea<=nareas(_i); iArea++, i++)
          INTEGER(d5)[i] = iArea; 
       }
    SET_VECTOR_ELT(dimnames, 4, d5);

#ifdef FLCORE_2
    PROTECT(d6 = allocVector(INTSXP, niters(_i)));
    for (iIter=1, i=0; iIter<=niters(_i); iIter++, i++)
        INTEGER(d6)[i] = iIter; 
    SET_VECTOR_ELT(dimnames, 5, d6);
#endif
    
    //Create names for dimensions
    PROTECT(names = allocVector(STRSXP, FLQUANT_NDIMS));
    SET_STRING_ELT(names, 0, mkChar("age"));
    SET_STRING_ELT(names, 1, mkChar("year"));
    SET_STRING_ELT(names, 2, mkChar("unit"));
    SET_STRING_ELT(names, 3, mkChar("season"));
    SET_STRING_ELT(names, 4, mkChar("area"));
#ifdef FLCORE_2
    SET_STRING_ELT(names, 5, mkChar("iter")); 
#endif
    setAttrib(dimnames, R_NamesSymbol, names);
    setAttrib(v, R_DimNamesSymbol, dimnames);
   
    //Set data
    i=0;
    for(iIter = 1; iIter <= niters(_i); iIter++)
	    for (iArea = 1; iArea <= nareas(_i); iArea++)
	  	    for (iSeason = 1; iSeason <= nseasons(_i); iSeason++)
     		    for (iUnit = 1; iUnit <= nunits(_i); iUnit++)
	    		    for (iYear = minyr(_i); iYear <= maxyr(_i); iYear++)
			 		    for (iAge = minquant(_i); iAge <= maxquant(_i); iAge++)
			      			    REAL(v)[i++] = data[_i][iAge][iYear][iUnit][iSeason][iArea][iIter]; 
                   
    //Set slot
    Quant = R_do_slot_assign(Quant, install(".Data"), v);

    UNPROTECT(11);
    
    return Quant;
    }

SEXP FLQuant2::Return(void)
   {
   SEXP ReturnObject;

   PROTECT(ReturnObject = allocVector(VECSXP,flq_n7));
   for (int i=0; i<flq_n7; i++)
      SET_VECTOR_ELT(ReturnObject, i,  Return(i+1));

   return ReturnObject;
   }

int& FLQuant2::minquant(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_minquant[i];
   }

int& FLQuant2::maxquant(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_maxquant[i];
   }

int& FLQuant2::plusgrp(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_plusgrp[i];
   }

int& FLQuant2::minyr(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_minyr[i];
   }

int& FLQuant2::maxyr(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_maxyr[i];
   }

int& FLQuant2::nunits(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_nunits[i];
   }

int& FLQuant2::nseasons(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_nseasons[i];
   }

int& FLQuant2::nareas(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_nareas[i];
   }

int& FLQuant2::niters(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_int=0;
   else
      return flq_niters[i];
   }

int& FLQuant2::n7()
   {
   return flq_n7;
   }

bool& FLQuant2::InitFlag(int i)
   {
   if (i<1 || i>flq_n7)
      return outofbounds_bool=false;
   else
      return flq_InitFlag[i];
   }

FLSRRVector::FLSRRVector(void)      
   {
   InitFlag = false;
   }

FLRConst_SRR& FLSRRVector::operator()(int _i) 
   { 

   if (_i < mindim || _i > maxdim) 
      return outofbounds_FLRConst_SRR = FLRConst_Mean;
      
   return (data)[_i]; 
   } 

FLSRRVector::FLSRRVector(SEXP x)  
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }

void FLSRRVector::Init(SEXP v)      
   {
   int n;

   if (!isVector(v) || !isString(v)) 
      return;

   n = LENGTH(v);

   SEXP names = GET_NAMES(v);
   if (LENGTH(names) == n) //index by name
      {
      //get indices
      mindim = atoi(CHAR(VECTOR_ELT(names, 0))); 
      maxdim = mindim + n - 1;

      //check indices
      for (int i=1; i<n; i++)
         if ((mindim+i) != atoi(CHAR(VECTOR_ELT(names, i))))
            return;
      }
   else
     {
     mindim = 1;
     maxdim = n;
     }
    
   alloc();

   for (int i=mindim; i<=maxdim; i++)
      {
      char *t = _strlwr(CHAR(STRING_ELT(v,     i-mindim)));
 
      if       (t[0]=='m')
         data[i] = FLRConst_Mean;
      else  if (t[0]=='b')
         data[i] = FLRConst_BevHolt;
      else  if (t[0]=='r')
         data[i] = FLRConst_Ricker;
      else  if (t[0]=='s' || t[0]=='q')
         data[i] = FLRConst_SegReg;
      else
         data[i] = FLRConst_Mean;   
      }

    return;
    }

SEXP FLSRRVector::Return(void)      
    {
    int i, i_;
    
    SEXP v, 
         d1,  
         dim,
         dimnames;    

    //Create new S4 object    
    PROTECT(v       = allocVector(STRSXP, maxdim-mindim+1)); 
    
    //Set dimensions of array
    PROTECT(dim     = allocVector(INTSXP, 1));       
    INTEGER(dim)[0] = maxdim-mindim+1;
        
    //Create dimension names
    PROTECT(dimnames = allocVector(VECSXP, 1));
    
    PROTECT(d1 = allocVector(INTSXP, maxdim-mindim+1));
    for (i_=mindim, i=0; i_<=maxdim; i_++, i++)
        INTEGER(d1)[i] = i_; 
    SET_VECTOR_ELT(dimnames, 1, d1);
     
    //Set data
    for (i_=mindim, i=0; i_<=maxdim; i_++, i++)
          {
          if       (data[i_]==FLRConst_Mean)
             SET_VECTOR_ELT(v,i, mkChar("mean"));
          else  if (data[i_]==FLRConst_BevHolt)
             SET_VECTOR_ELT(v,i, mkChar("bevholt"));
          else  if (data[i_]==FLRConst_Ricker)
             SET_VECTOR_ELT(v,i, mkChar("ricker"));
          else  if (data[i_]==FLRConst_SegReg)
             SET_VECTOR_ELT(v,i, mkChar("qhstk"));
          else
             SET_VECTOR_ELT(v,i, mkChar("mean"));
          }

    UNPROTECT(4);
    
    return v;
    }

void FLSRRVector::unalloc(void)      
   {
   if (!InitFlag) return;

   free((char * ) (data+mindim));
   
   InitFlag=false;
   }                               

void FLSRRVector::alloc(void)      
   {
   if (InitFlag) unalloc();

   data=(FLRConst_SRR*) malloc((maxdim-mindim+1)*sizeof(FLRConst_SRR)) - mindim;
   }

FLSRRVector::~FLSRRVector(void)      
   {
   unalloc();
   }                               


FLCatch::FLCatch(void)      
    {
    InitFlag = false;
    }

FLCatch::FLCatch(SEXP x)      
    {
    InitFlag = false;

    if (!InitFlag)
       Init(x);
    }
   
FLCatch::~FLCatch(void)      
   {
   ;   
   }                               

void FLCatch::Init(SEXP x)
   {

   minquant  = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant  = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp   = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr     = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr     = (int)REAL(GET_SLOT(x, install("range")))[4];

   q.Init(           GET_SLOT(x, install("q")));
   catch_.Init(      GET_SLOT(x, install("catch")));
   catch_n.Init(     GET_SLOT(x, install("catch.n")));
   catch_wt.Init(    GET_SLOT(x, install("catch.wt")));
   catch_sel.Init(   GET_SLOT(x, install("catch.sel")));
   discards.Init(    GET_SLOT(x, install("discards")));
   discards_n.Init(  GET_SLOT(x, install("discards.n")));
   discards_wt.Init( GET_SLOT(x, install("discards.wt")));
   discards_sel.Init(GET_SLOT(x, install("discards.sel")));
   landings.Init(    GET_SLOT(x, install("landings")));
   landings_n.Init(  GET_SLOT(x, install("landings.n")));
   landings_wt.Init( GET_SLOT(x, install("landings.wt")));
   landings_sel.Init(GET_SLOT(x, install("landings.sel")));
   price.Init(       GET_SLOT(x, install("price")));

   InitFlag = true;
   }

SEXP FLCatch::Return(void)
   {
   SEXP Catch, Range;


   PROTECT(Catch  = NEW_OBJECT(MAKE_CLASS("FLCatch")));
   Range          = PROTECT(NEW_NUMERIC(5)); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   
   SET_SLOT(Catch, install("catch"),       catch_.Return());
   SET_SLOT(Catch, install("q"),           q.Return());            
	SET_SLOT(Catch, install("catch.n"),     catch_n.Return());      
	SET_SLOT(Catch, install("catch.wt"),    catch_wt.Return());     
   SET_SLOT(Catch, install("catch.sel"),   catch_sel.Return());    
	SET_SLOT(Catch, install("discards"),    discards.Return());     
	SET_SLOT(Catch, install("discards.n"),  discards_n.Return());   
	SET_SLOT(Catch, install("discards.wt"), discards_wt.Return());  
	SET_SLOT(Catch, install("discards.sel"),discards_sel.Return()); 
	SET_SLOT(Catch, install("landings"),    landings.Return());     
	SET_SLOT(Catch, install("landings.n"),  landings_n.Return());   
	SET_SLOT(Catch, install("landings.wt"), landings_wt.Return());  
	SET_SLOT(Catch, install("landings.sel"),landings_sel.Return());  
   SET_SLOT(Catch, install("price"),       price.Return());         

      
   UNPROTECT(2);

   return Catch;
   }

   
FLFleet::FLFleet(void)
   {
   InitFlag = false;
      
   nunits   = 
   nseasons = 
   nareas   = 
   nspp     = 0;
   }
      
FLFleet::FLFleet(SEXP x)
   {
   InitFlag = false;

   nunits   = 
   nseasons = 
   nareas   = 
   nspp     = 0;

   Init(x);
   }

FLFleet::~FLFleet(void)     
   {
   ;
   }

void  FLFleet::Init(SEXP x)
   {
   if (nunits >= 1 || nseasons >= 1 || nareas >= 1 || nspp >= 1) unalloc();

   minquant = (int)REAL(GET_SLOT(x, install("range")))[0];
   maxquant = (int)REAL(GET_SLOT(x, install("range")))[1];
   plusgrp  = (int)REAL(GET_SLOT(x, install("range")))[2];
   minyr    = (int)REAL(GET_SLOT(x, install("range")))[3];
   maxyr    = (int)REAL(GET_SLOT(x, install("range")))[4];
   
   nunits   = 1;
   nseasons = 1;
   nareas   = 1;
   
   SEXP catches = PROTECT(GET_SLOT(x, install("catches")));
  
   nspp = NElemList(catches);

   if (nspp < 1) return;
   
   effort.Init( GET_SLOT(x, install("effort"))); 
   capacity.Init( GET_SLOT(x, install("capacity")));   
   crewshare.Init( GET_SLOT(x, install("crewshare")));
   fcost.Init(  GET_SLOT(x, install("fcost"))); 
   vcost.Init(  GET_SLOT(x, install("vcost"))); 

   catch_.Init(      nspp);
   catch_n.Init(     nspp);
   catch_wt.Init(    nspp);
   catch_sel.Init(   nspp);
   landings.Init(    nspp);
   landings_n.Init(  nspp);
   landings_wt.Init( nspp);
   landings_sel.Init(nspp);
   discards.Init(    nspp);
   discards_n.Init(  nspp);
   discards_wt.Init( nspp);
   discards_sel.Init(nspp);
   q.Init(           nspp);
   price.Init(       nspp);

   for (int i=1; i<=nspp; i++)
      {
      SEXP t = PROTECT(VECTOR_ELT(catches, i-1));

      catch_.Init(      i, GET_SLOT(t, install("catch"))); 
      catch_n.Init(     i, GET_SLOT(t, install("catch.n"))); 
      catch_wt.Init(    i, GET_SLOT(t, install("catch.wt"))); 
      catch_sel.Init(   i, GET_SLOT(t, install("catch.sel"))); 
      landings.Init(    i, GET_SLOT(t, install("landings"))); 
      landings_n.Init(  i, GET_SLOT(t, install("landings.n"))); 
      landings_wt.Init( i, GET_SLOT(t, install("landings.wt"))); 
      landings_sel.Init(i, GET_SLOT(t, install("landings.sel"))); 
      discards.Init(    i, GET_SLOT(t, install("discards"))); 
      discards_n.Init(  i, GET_SLOT(t, install("discards.n"))); 
      discards_wt.Init( i, GET_SLOT(t, install("discards.wt"))); 
      discards_sel.Init(i, GET_SLOT(t, install("discards.sel"))); 
      q.Init(           i, GET_SLOT(t, install("q"))); 
      price.Init(       i, GET_SLOT(t, install("price"))); 
      }
    
   UNPROTECT(1);
   }

void  FLFleet::alloc(void)      
      {
      ;
      }

void  FLFleet::unalloc(void)      
      {
      ;
      }

SEXP  FLFleet::Return(void)     
   {
   SEXP fleet, catches, Range, t;

   PROTECT(fleet  = NEW_OBJECT(MAKE_CLASS("FLFleet")));
   PROTECT(Range  = PROTECT(NEW_NUMERIC(5))); 
   
   REAL(Range)[0] = minquant;
   REAL(Range)[1] = maxquant;
   REAL(Range)[2] = plusgrp;
   REAL(Range)[3] = minyr;
   REAL(Range)[4] = maxyr;
   
   SET_SLOT(fleet, install("range"),      Range);         
   SET_SLOT(fleet, install("effort"),     effort.Return());         
   SET_SLOT(fleet, install("capacity"),   capacity.Return());         
   SET_SLOT(fleet, install("crewshare"),  crewshare.Return());         
   SET_SLOT(fleet, install("fcost"),      fcost.Return());         
   SET_SLOT(fleet, install("vcost"),      vcost.Return());         
   
   PROTECT(catches = allocVector(VECSXP, nspp));        

   for (int i=1; i<=nspp; i++)
      {
      PROTECT(t = NEW_OBJECT(MAKE_CLASS("FLCatch")));
      
      SET_SLOT(t, install("catch"),        catch_.Return(i));
      SET_SLOT(t, install("catch.n"),      catch_n.Return(i));
      SET_SLOT(t, install("catch.wt"),     catch_wt.Return(i));
      SET_SLOT(t, install("catch.sel"),    catch_sel.Return(i));
      SET_SLOT(t, install("landings"),     landings.Return(i));
      SET_SLOT(t, install("landings.n"),   landings_n.Return(i));
      SET_SLOT(t, install("landings.wt"),  landings_wt.Return(i));
      SET_SLOT(t, install("landings.sel"), landings_sel.Return(i));
      SET_SLOT(t, install("discards"),     discards.Return(i));
      SET_SLOT(t, install("discards.n"),   discards_n.Return(i));
      SET_SLOT(t, install("discards.wt"),  discards_wt.Return(i));
      SET_SLOT(t, install("discards.sel"), discards_sel.Return(i));
      SET_SLOT(t, install("q"),            q.Return(i));
      SET_SLOT(t, install("price"),        price.Return(i));
      
      SET_VECTOR_ELT(catches, i-1, t); 
      }
    
   SET_SLOT(fleet, install("catches"),  catches);         

   return fleet;
   }

SEXP ReturnDouble(double x) 
    {
    SEXP v = PROTECT(NEW_NUMERIC(1)); 
  
    REAL(v)[0] = x;
           
    UNPROTECT(1);
    
    return v;    
    }

FLIndices::FLIndices(void)      
    {
    InitFlag() = false;

    nIndex()   = 0;
    }

FLIndices::FLIndices(SEXP x)      
   {
   InitFlag() = false;

   nIndex() = 0;
   
   if (isFLIndices(x))
      Init(x);
   }                               

void  FLIndices::Init(SEXP x)
   {
   if (!isFLIndices(x)) return;

   nIndex() = NElemList(x);

   if (nIndex() < 1) return;
   
   catch_.Init(     nIndex()); 
   catch_wt.Init(   nIndex()); 
   effort.Init(     nIndex()); 
   sel_pattern.Init(nIndex()); 
   index_q.Init(    nIndex()); 
   index.Init(      nIndex()); 
   index_var.Init(  nIndex()); 

  
   minquant.Init(1, nIndex(), 0.0);
   maxquant.Init(1, nIndex(), 0.0);
   plusgrp.Init( 1, nIndex(), 0.0);
   minyr.Init(   1, nIndex(), 0.0);
   maxyr.Init(   1, nIndex(), 0.0);
   start.Init(   1, nIndex(), 0.0);
   end.Init(     1, nIndex(), 0.0);
   
   for (int i=1; i<=nIndex(); i++)
      {
      SEXP t     = PROTECT(VECTOR_ELT(x, i-1));
      SEXP range = PROTECT(GET_SLOT(t, install("range")));
      
      int     _minquant, _maxquant, _plusgrp,  _minyr, _maxyr;
      double  _start, _end;

      InputRange(range, &_minquant, &_maxquant, &_plusgrp, &_minyr, &_maxyr, &_start, &_end);

      minquant(i) = _minquant;
      maxquant(i) = _maxquant;
      plusgrp(i)  = _plusgrp;
      minyr(i)    = _minyr;
      maxyr(i)    = _maxyr;
      start(i)    = _start;
      end(i)      = _end;
   
      catch_.Init(     i, GET_SLOT(t, install("catch")));      
      catch_wt.Init(   i, GET_SLOT(t, install("catch.wt"))); 
      effort.Init(     i, GET_SLOT(t, install("effort"))); 
      sel_pattern.Init(i, GET_SLOT(t, install("sel.pattern"))); 
      index_q.Init(    i, GET_SLOT(t, install("index.q"))); 
      index.Init(      i, GET_SLOT(t, install("index"))); 
      index_var.Init(  i, GET_SLOT(t, install("index.var"))); 
      }
    
   UNPROTECT(1);
   }

SEXP  FLIndices::Return(void)     
   {
   SEXP ReturnValue = R_NilValue, t, range, e = R_NilValue;

   PROTECT(ReturnValue = allocVector(VECSXP, nIndex()));        

   for (int i=1; i<=nIndex(); i++)
      {
      PROTECT(t = NEW_OBJECT(MAKE_CLASS("FLIndex")));
      
      range          = PROTECT(NEW_NUMERIC(7)); 
   
      REAL(range)[0] = minquant(i);
      REAL(range)[1] = maxquant(i);
      REAL(range)[2] = plusgrp(i);
      REAL(range)[3] = minyr(i);
      REAL(range)[4] = maxyr(i);
      REAL(range)[5] = start(i);
      REAL(range)[6] = end(i);
   
      SET_SLOT(t, install("range"),    range);
      SET_SLOT(t, install("catch"),       catch_.Return(i));
      SET_SLOT(t, install("catch.wt."),   catch_wt.Return(i));
      SET_SLOT(t, install("effort."),     effort.Return(i));
      SET_SLOT(t, install("sel_pattern"), sel_pattern.Return(i));
      SET_SLOT(t, install("index_q"),     index_q.Return(i));
      SET_SLOT(t, install("index"),       index.Return(i));
      SET_SLOT(t, install("index.var"),   index_var.Return(i));

      SET_VECTOR_ELT(ReturnValue, i-1, t); 
      
      UNPROTECT(1);
      }
   
   UNPROTECT(1);
 
   return ReturnValue;
   }

bool& FLIndices::InitFlag()
   {
   return member_InitFlag;
   }

int& FLIndices::nIndex()
   {
   return member_nIndex;
   }

FLIndices::~FLIndices(void)      
   {
   ; //unalloc();
   }                               
