#ifndef _INC_FLCoreClasses
#define _INC_FLCoreClasses

#include <math.h>
#include <float.h>
#include <Rdefines.h>
#include <Rinternals.h>

//#define FLCORE_2

#ifdef FLCORE_2
   #define FLQUANT_NDIMS 6
#else
   #define FLQUANT_NDIMS 5
#endif

#define const_nan 0.0


typedef enum tagFLRConst_Target 
	{
   FLRConst_SSB        = 1,
   FLRConst_Biomass    = 2,
   FLRConst_Catch      = 3,
   FLRConst_Landings   = 4,
   FLRConst_Effort     = 5,
   FLRConst_F          = 6,
   FLRConst_None       = 7
	} FLRConst_Target;

typedef enum tagFLRConst_SRR 
	{
   FLRConst_Mean       = 1,
   FLRConst_BevHolt    = 2,
   FLRConst_Ricker     = 3,
   FLRConst_SegReg     = 4
	} FLRConst_SRR;

typedef enum tagFLRConst_Index 
	{
   FLRConst_Base         = 1,
   FLRConst_IndexCom     = 2,
   FLRConst_IndexSurvey  = 3,
   FLRConst_IndexAcoustic= 4
	} FLRConst_Index;

extern int             outofbounds_int;
extern bool            outofbounds_bool;
extern double          outofbounds_double;
extern FLRConst_Target outofbounds_target;
extern FLRConst_SRR    outofbounds_FLRConst_SRR;
       
int NElemList(SEXP);

bool isFLQuant(SEXP);
bool isFLBiol(SEXP);
bool isFLFleet(SEXP);
bool isFLCatch(SEXP);
bool isFLStock(SEXP);
bool isFLFleets(SEXP);
bool isFLcatch(SEXP);
bool isFLStocks(SEXP);
bool isFLCatchs(SEXP);
bool isFLSR(SEXP);
void InputRange(SEXP, int *, int *, int *, int *, int *);
void InputRange(SEXP, int *, int *, int *, int *, int *, int *, int *);
void InputRange(SEXP, int *, int *, int *, int *, int *, double *, double *);
void InputAgeRange(SEXP, int *, int *);

SEXP ReturnDouble(double); 

class FLVector
{
public:        

   FLVector(void);      
   FLVector(SEXP);
  ~FLVector(void);      

   void Init(SEXP);
   void Init(int, int, double val=0.0);      
      
   
   SEXP Return(void);      

   double& operator () (int _i);

   int mindim, maxdim;

protected: 
   bool InitFlag;

   double *data;   

   void alloc(void);      
   void unalloc(void);         
   };                  


class FLBool
{
public:        
   FLBool(void);      
   FLBool(SEXP);
  ~FLBool(void);      

   void Init(SEXP);      
   
   SEXP Return(void);      

   bool& operator () (int _i);

   int mindim, maxdim;

protected: 
   bool InitFlag;

   bool *data;   
   
   void alloc(void);      
   void unalloc(void);      
   };                  

class FL2D
{
public:        
   FL2D(void);      
   FL2D(SEXP);
  ~FL2D(void);      

   void Init(SEXP);      
   
   SEXP Return(void);      

   int mindim(int);
   int maxdim(int);

   double& operator () (int _i, int _j);

protected: 
   bool InitFlag;

   int min1, max1, min2, max2;

   double **data;   
   
   void alloc(void);      
   void unalloc(void);      
   };                  


class FL2DRagged
{
public:        
   FL2DRagged(void);      
  ~FL2DRagged(void);      

   void Init(int);
   void Init(int, SEXP);

   SEXP Return(int);      

   int mindim(int);
   int maxdim(int);

   double& operator () (int _i, int _j);

protected: 
   bool *InitFlag;

   int  n;
   int *_mindim, *_maxdim;
   
   double **data;   
 
   void alloc(int);         
   void unalloc(int);         
   void unalloc(void);         
   };                  

class FLQuant
{
public:        
   FLQuant(void);      
   FLQuant(SEXP);
   FLQuant(int, int, int, int, int, int, int, int, double);
  ~FLQuant(void);      

   void Init(SEXP);      
   void Init(SEXP, double);      
   void Init(int, int, int, int, int, int, int, int, double val=0.0);     

   SEXP Return(void);      
   
   double& operator () (int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1);

   bool &InitFlag();

   int &minquant(), &maxquant(), &plusgrp(),
       &minyr(),    &maxyr(),
       &nunits(),
       &nseasons(),
       &nareas(),
       &niters();

   int mindim(int);
   int maxdim(int);

protected: 
   bool flq_InitFlag;

   int flq_minquant, flq_maxquant, flq_plusgrp,
       flq_minyr,    flq_maxyr,
       flq_nunits,
       flq_nseasons,
       flq_nareas,
       flq_niters;
   
   double ******data;   

   void alloc(void);      
   void unalloc(void);         
   };                  

class FLQuant2 
{
public:        

   FLQuant2(void);
   FLQuant2(SEXP);      
   FLQuant2(int);
   FLQuant2(int, SEXP);      
  ~FLQuant2(void);      
   
   void Init(int);      
   void Init(SEXP);      
   void Init(int, SEXP);      

   SEXP Return(int);      
   SEXP Return(void);      

   double& operator () (int i, int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1);
 
   bool &InitFlag(int);

   int &minquant(int i=1), &maxquant(int i=1), &plusgrp(int i=1),
       &minyr(int i=1),    &maxyr(int i=1),
       &nunits(int i=1),
       &nseasons(int i=1),
       &nareas(int i=1),
       &niters(int i=1),
       &n7();

protected: 
   bool *flq_InitFlag;

   int *flq_minquant, *flq_maxquant, *flq_plusgrp,
       *flq_minyr,    *flq_maxyr,
       *flq_nunits,
       *flq_nseasons,
       *flq_nareas,
	    *flq_niters,
       flq_n7;

   double *******data;   

//   void Init(double*****_d,int i, int _minquant,int _maxquant,int _minyr,int _maxyr,int _nunits=1,int _nseasons=1,int _nareas=1);      
   
   void alloc(int);      
   void unalloc(void);      
   void unalloc(int);      
   };                  

class FLQuant3 
{
public:        

   FLQuant3(void);
   FLQuant3(int, int);
   FLQuant3(int, int, SEXP);      
  ~FLQuant3(void);      

   void Init(int, int);      
   void Init(int, int, SEXP);      
      
   double& operator () (int i, int j, int _age, int _yr, int _unit=1, int _season=1, int _area=1, int _iter=1);

   SEXP Return(int, int);      
   SEXP Return(void);      

   bool &InitFlag(int);

   int &minquant(int i=1, int j=1), &maxquant(int i=1, int j=1), &plusgrp(int i=1, int j=1),
       &minyr(   int i=1, int j=1), &maxyr(   int i=1, int j=1),
       &nunits(  int i=1, int j=1),
       &nseasons(int i=1, int j=1),
       &nareas(  int i=1, int j=1),
       &niters(  int i=1, int j=1),
       &n7(), &n8();

protected: 
   bool *flq_InitFlag;

   int **flq_minquant, **flq_maxquant, **flq_plusgrp,
       **flq_minyr,    **flq_maxyr,
       **flq_nunits,
       **flq_nseasons,
       **flq_nareas,
	    **flq_niters,
       flq_n7, flq_n8;

   double ********data;   
   
   void alloc(void);      
   void alloc(int,int);      
   void unalloc(void);      
   void unalloc(int,int);         
   };                  


class FLStock 
{
public:        

   FLStock(void);      
   FLStock(SEXP);      
  ~FLStock(void);      

   void Init(SEXP);      
   SEXP Return(void);      

  FLQuant catch_, 
          catch_n, 
          catch_wt, 
          discards, 
          discards_n, 
          discards_wt, 
          landings, 
          landings_n, 
          landings_wt, 
          stock, 
          stock_n, 
          stock_wt, 
          m, 
          mat, 
          harvest, 
          harvest_spwn, 
          m_spwn; 
  
protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

   void unalloc(void);      
   };                  


class FLIndex 
{
public:        

   FLIndex(void);      
   FLIndex(SEXP);      
  ~FLIndex(void);      

   void Init(SEXP);      

   SEXP Return(void);      

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

protected: 
   bool InitFlag;

   double start, end;

   FLQuant index_var,
           index;   

   void alloc(void);      
   };                  

class FLIndexCom 
{
public:        

   FLIndexCom(void);      
   FLIndexCom(SEXP);      
  ~FLIndexCom(void);      

   void Init(SEXP);      

   SEXP Return(void);      

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

protected: 
   bool InitFlag;

   double start, end;

   FLQuant catch_,     
           catch_wt,       
           effort,  
           sel_pattern,      
           index_q,            
           index,    
           index_var;   

   void alloc(void);      
   };                  
             
class FLIndexSurvey 
{
public:        

   FLIndexSurvey(void);      
   FLIndexSurvey(SEXP);      
  ~FLIndexSurvey(void);      

   void Init(SEXP);      

   SEXP Return(void);      

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

protected: 
   bool InitFlag;

   double start, end;

   FLQuant catch_n,     
           catch_wt,       
           effort,      
           index_q,             
           index,    
           index_var;   

   void alloc(void);      
   };                  

class FLIndexAcoustic 
{
public:        

   FLIndexAcoustic(void);      
   FLIndexAcoustic(SEXP);      
  ~FLIndexAcoustic(void);      

   void Init(SEXP);      

   SEXP Return(void);      

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

protected: 
   bool InitFlag;

   double start, end;

   FLQuant index_var,
           index_q,
           index;   

   void alloc(void);      
   };                  

class FLBiol 
{
public:        

   FLBiol(void);      
   FLBiol(SEXP);      
  ~FLBiol(void);      

   FLQuant n,
           m,
           wt,
           fec,
           spwn;   

   void Init(SEXP);      
   SEXP Return(void);      

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters;

   void alloc(void);      
   };
                  
class FLSR
{
public:
   
   FLSR(void);
   FLSR(SEXP);
  ~FLSR(void);

   char *name,
	*desc,
	*model;
   
   FLQuant ssb,
	   rec,
	   rechat,
	   residuals;
   
   double *params,
	  *se,
	  *covar,
	  var,
	  vara,
	  aic;

   void Init(SEXP);
   //change to SEXP Return(int nparams) when updated for 
   //latest params definition in FLSR
   SEXP Return(int nparams);

protected:
   bool InitFlag;

   void alloc(void); 
   };
	
/*   END OF THE CLASS DECLARATIONS */


class FLSRRVector
{
public:        

   FLSRRVector(void);      
   FLSRRVector(SEXP);
  ~FLSRRVector(void);      

   void Init(SEXP);      
   
   SEXP Return(void);      

   FLRConst_SRR& operator () (int _i);

protected: 
   bool InitFlag;
         
   int mindim, maxdim;

   FLRConst_SRR *data;   

   void alloc(void);      
   void unalloc(void);         
   };                  

class FLCatch 
{
public:        

   FLCatch(void);      
   FLCatch(SEXP);      
  ~FLCatch(void);      

   void Init(SEXP);      
   SEXP Return(void);      

   FLQuant catch_, 
           catch_n, 
           catch_wt, 
           catch_sel, 
           landings, 
           landings_n, 
           landings_wt, 
           landings_sel, 
           discards, 
           discards_n, 
           discards_wt, 
           discards_sel, 
           q, 
           price;

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas;
   };     

class FLFleet 
{
public:        

   FLFleet(void);      
   FLFleet(SEXP);      
  ~FLFleet(void);      

   void Init(SEXP);      

   SEXP Return(void);      

   FLQuant effort, 
           capacity,
           crewshare, 
           vcost, 
           fcost; 

   FLQuant2 catch_, 
            catch_n, 
            catch_wt, 
            catch_sel, 
            landings, 
            landings_n, 
            landings_wt, 
            landings_sel, 
            discards, 
            discards_n, 
            discards_wt, 
            discards_sel, 
            q, 
            price;

protected: 
   bool InitFlag;

   int minquant, maxquant, plusgrp,
       minyr,  maxyr,
       nunits,
       nseasons,
       nareas,
       niters,
       nspp;

   void alloc(void);      
   void unalloc(void);      
};     

class FLIndices 
{
public:        

   FLIndices(void);      
   FLIndices(SEXP);      
  ~FLIndices(void);      

   void Init(SEXP);      

   bool &InitFlag();
   int  &nIndex();

   FLVector minquant, maxquant, plusgrp,
            minyr,    maxyr,    
            start,    end,
            nunits,
            nseasons,
            nareas;

  FLQuant2 catch_,     
           catch_wt,       
           effort,  
           sel_pattern,      
           index_q,            
           index,    
           index_var;   

   SEXP Return(void);      

protected: 
   bool member_InitFlag;
   int  member_nIndex;

   void alloc(void);      
   };                  

 #endif /* _INC_FLCoreClasses */


