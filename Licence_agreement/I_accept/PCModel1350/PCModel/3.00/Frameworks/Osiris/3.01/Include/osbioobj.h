/*------------------------------------------------------------------------*/
/*  Osbioobj.h                                                            */
/*  Copyright (c) 1993, 1999                                              */
/*  W.M. Mooij                                                            */
/*  Netherlands Institute of Ecology                                      */
/*  Centre for Limnology                                                  */
/*  Rijksstraatweg 6                                                      */
/*  3631 AC  Nieuwersluis                                                 */
/*  The Netherlands                                                       */
/*  tel: +31 294 239300                                                   */
/*  fax: +31 294 232224                                                   */
/*  e-mail: mooij@cl.nioo.knaw.nl                                         */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __OSBIOOBJ_H
#define __OSBIOOBJ_H

#include "ossimobj.h"

// Constant definitions

#define __OSIRIS__   0x301
#define ANA_LNG_READY    1
#define ANA_DBL_READY    1
#define ANA_STR_SYS_INP  1
#define ANA_STR_GCN_INP  2
#define ANA_STR_GRS_INP  3
#define ANA_STR_POP_INP  4
#define ANA_STR_HAB_INP  5
#define ANA_STR_LCN_INP  6
#define ANA_STR_LRS_INP  7
#define ANA_STR_IND_INP  8
#define ANA_STR_ANA_LOG  9
#define ANA_STR_SYS_LOG 10
#define ANA_STR_GCN_LOG 11
#define ANA_STR_GRS_LOG 12
#define ANA_STR_POP_LOG 13
#define ANA_STR_HAB_LOG 14
#define ANA_STR_LCN_LOG 15
#define ANA_STR_LRS_LOG 16
#define ANA_STR_IND_LOG 17
#define ANA_STR_ANA_OUT 18
#define ANA_STR_SYS_OUT 19
#define ANA_STR_GCN_OUT 20
#define ANA_STR_GRS_OUT 21
#define ANA_STR_POP_OUT 22
#define ANA_STR_HAB_OUT 23
#define ANA_STR_LCN_OUT 24
#define ANA_STR_LRS_OUT 25
#define ANA_STR_IND_OUT 26
#define ANA_STR_ANA_DMP 27
#define ANA_STR_SYS_DMP 28
#define ANA_STR_GCN_DMP 29
#define ANA_STR_GRS_DMP 30
#define ANA_STR_POP_DMP 31
#define ANA_STR_HAB_DMP 32
#define ANA_STR_LCN_DMP 33
#define ANA_STR_LRS_DMP 34
#define ANA_STR_IND_DMP 35
#define HAB_LNG_SYS_ID   1
#define GCN_LNG_SYS_ID   1
#define GRS_LNG_SYS_ID   1
#define POP_LNG_SYS_ID   1
#define LCN_LNG_GCN_ID   1
#define LCN_LNG_HAB_ID   2
#define LCN_LNG_SYS_ID   3
#define LRS_LNG_GRS_ID   1
#define LRS_LNG_HAB_ID   2
#define LRS_LNG_SYS_ID   3
#define IND_LNG_POP_ID   1
#define IND_LNG_HAB_ID   2
#define IND_LNG_SYS_ID   3

// Class declarations

class Analyser;
class AnalyserTask;
class System;
class SystemTask;
class SysObject;
class GlobalCondition;
class GlobalConditionTask;
class GlobalResource;
class GlobalResourceTask;
class Population;
class PopulationTask;
class Habitat;
class HabitatTask;
class HabObject;
class LocalCondition;
class LocalConditionLink;
//class LocalConditionStlt;
class LocalConditionTask;
class LocalResource;
class LocalResourceLink;
//class LocalResourceStlt;
class LocalResourceTask;
class Individual;
class IndividualLink;
//class IndividualStlt;
class IndividualTask;

// Class definitions

class Analyser : public SimObject
{ public :
    // Member functions
    inline static  Analyser        &getAna();
    inline         double           getAnaDblReady() const;
    inline         long             getAnaLngReady() const;
    inline static  ofstream        &getDmpFile();
    inline         long             getGblIndNumber(long p) const;
    inline         GlobalCondition &getGcn(long c) const;
    inline         long             getGcnNumber() const;
    inline         GlobalResource  &getGrs(long r) const;
    inline         long             getGrsNumber() const;
    inline         Habitat         &getHab(long h) const;
    inline         long             getHabNumber() const;
    inline         Individual      &getInd(long p, long h, long i) const;
    inline         long             getIndNumber(long p, long h) const;
    inline         LocalCondition  &getLcn(long c, long h) const;
    inline         LocalResource   &getLrs(long r, long h) const;
    inline         Population      &getPop(long p) const;
    inline         long             getPopNumber() const;
    inline         Individual      &getRndInd(long p, long h) const;
    inline         System          &getSys() const;
    inline         long             getTotGblIndNumber() const;
           static  int              main(void (*fPtr)(), char *cPtr);
           static  void             printCopyright();
    inline         void             printToLogFile();
    inline         Analyser        &setAnaDblReady(double t);
    inline         Analyser        &setAnaLngReady(long n);
    inline static  void             setConstructPtr(void (*fPtr)(Analyser &a));
    inline static  void             setDataPtr(void (*fPtr)());
    inline static  void             setDblNumber(int i);
    inline static  void             setDestructPtr(void (*fPtr)(Analyser &a));
    inline static  void             setEvaluatePtr(void (*fPtr)(Analyser &a));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
    inline static  void             setMainPtr(int (*fPtr)(Analyser &a));
    inline         AnalyserTask    &setNewAnaTask(double t, double (*tPtr)(AnalyserTask &t));
    inline static  void             setOutOfMemoryPtr(void (*fPtr)());
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
    inline static  DatFile         &getDatFile();
           static  int              version;
  protected :
    // Friends
           friend  class            System;
           friend  class            AnalyserTask;
    // Member functions
    inline                          Analyser(DatFile &f);
    inline                          Analyser(const Analyser &a);
    inline virtual                 ~Analyser();
    inline         Analyser        &operator =(const Analyser &a);
    inline static  void             setAnalyserPtr(Analyser *aPtr);
    inline         Analyser        &setSystemPtr(System *sPtr);
    // Data members
           static  void           (*constructPtr)(Analyser &a);
           static  void           (*dataPtr)();
           static  void           (*destructPtr)(Analyser &a);
           static  int            (*mainPtr)(Analyser &a);
           static  void           (*evaluatePtr)(Analyser &a);
           static  void           (*outOfMemoryPtr)();
           static  Analyser        *theAnalyserPtr;
           static  DatFile          theDatFile;
                   System          *theSystemPtr;
};

class AnalyserTask : public SimObjectTask
{ public :
    inline         Analyser        &getAna() {return (Analyser &)getSimObject();}
  private :
    // Friends
           friend  class            Analyser;
    // Member functions
    inline                          AnalyserTask(Analyser &s, double t, double (*tPtr)(AnalyserTask &t));
    inline                          AnalyserTask(const AnalyserTask &t);
    inline virtual                 ~AnalyserTask();
    inline         AnalyserTask    &operator =(const AnalyserTask &a);
    inline virtual void             run();
    // Data members
                   double         (*taskPtr)(AnalyserTask &t);
};

class System : public SimObject
{ public :
    // Member functions
    inline         Analyser        &getAna() const;
    inline static  ofstream        &getDmpFile();
    inline         long             getGblIndNumber(long p) const;
    inline         GlobalCondition &getGcn(long c) const;
    inline         long             getGcnNumber() const;
    inline         GlobalResource  &getGrs(long r) const;
    inline         long             getGrsNumber() const;
    inline         Habitat         &getHab(long h) const;
    inline         long             getHabNumber() const;
    inline         Individual      &getInd(long p, long h, long i) const;
    inline         long             getIndNumber(long p, long h) const;
    inline         LocalCondition  &getLcn(long c, long h) const;
    inline         LocalResource   &getLrs(long r, long h) const;
    inline         Population      &getPop(long p) const;
    inline         long             getPopNumber() const;
    inline         Individual      &getRndInd(long p, long h) const;
                   long             getTotGblIndNumber() const;
           static  int              main(Analyser &a);
    inline         void             printToLogFile();
    inline static  void             setConstructPtr(void (*fPtr)(System &s));
    inline static  void             setDblNumber(int i);
    inline static  void             setDestructPtr(void (*fPtr)(System &s));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
    inline         SystemTask      &setNewSysTask(double t, double (*tPtr)(SystemTask &i));
    inline static  void             setUpdatePtr(void (*fPtr)(System &s));
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
    inline static  DatFile         &getDatFile();
           static  double           update(SystemTask &t);
  protected :
    // Friends
           friend  class            Habitat;
           friend  class            GlobalCondition;
           friend  class            GlobalResource;
           friend  class            Population;
           friend  class            SystemTask;
    // Member functions
    inline                          System(Analyser &a, DatFile &f);
    inline                          System(const System &s);
    inline virtual                 ~System();
    inline         System          &addGcn(GlobalCondition &c);
    inline         System          &addHab(Habitat &h);
    inline         System          &addPop(Population &p);
    inline         System          &addGrs(GlobalResource &r);
    inline         VecConObject    &getGcnArray();
    inline         VecConObject    &getHabArray();
    inline         VecConObject    &getPopArray();
    inline         VecConObject    &getGrsArray();
    inline         System          &operator =(const System &s);
    // Data members
           static  void           (*constructPtr)(System &s);
           static  void           (*updatePtr)(System &s);
           static  void           (*destructPtr)(System &s);
                   Analyser        &theAnalyser;
                   VecConObject    *theGcnArrayPtr;
                   VecConObject    *theHabArrayPtr;
                   VecConObject    *thePopArrayPtr;
                   VecConObject    *theGrsArrayPtr;
           static  DatObject       *theSysDatObjectPtr;
           static  DatFile          theDatFile;
};

class SystemTask : public SimObjectTask
{ public :
    inline         System          &getSys() {return (System &)getSimObject();}
  private :
    // Friends
           friend  class            System;
    // Member functions
    inline                          SystemTask(System &s, double t, double (*tPtr)(SystemTask &t));
    inline                          SystemTask(const SystemTask &t);
    inline virtual                 ~SystemTask();
    inline         SystemTask      &operator =(const SystemTask &t);
    inline virtual void             run();
    // Data members
                   double         (*taskPtr)(SystemTask &t);
};

class SysObject : public SimObject
{ public :
    // Member functions
    inline         Analyser        &getAna() const;
    inline         long             getGblIndNumber(long p) const;
    inline         GlobalCondition &getGcn(long c) const;
    inline         long             getGcnNumber() const;
    inline         GlobalResource  &getGrs(long r) const;
    inline         long             getGrsNumber() const;
    inline         Habitat         &getHab(long h) const;
    inline         long             getHabNumber() const;
    inline         Individual      &getInd(long p, long h, long i) const;
    inline         long             getIndNumber(long p, long h) const;
    inline         LocalCondition  &getLcn(long c, long h) const;
    inline         LocalResource   &getLrs(long r, long h) const;
    inline         Population      &getPop(long p) const;
    inline         long             getPopNumber() const;
    inline         Individual      &getRndInd(long p, long h) const;
    inline         System          &getSys() const;
    inline         long             getTotGblIndNumber() const;
  protected :
    // Member functions
    inline                          SysObject(System &s, DatFile &f);
    inline                          SysObject(System &s, const DatObject &d);
    inline                          SysObject(const SysObject &s);
    inline virtual                 ~SysObject();
    inline         SysObject       &operator =(const SysObject &s);
    // Data members
                   System          &theSystem;
};

class GlobalCondition : public SysObject
{ public :
    // Member functions
    inline         LocalCondition  &getCspLcn(long h) const;
    inline         long             getGcnId() const;
    inline static  ofstream        &getDmpFile();
    inline static  long             getNumber();
    inline         void             printToLogFile();
    inline static  void             setConstructPtr(void (*fPtr)(GlobalCondition &c));
    inline static  void             setDblNumber(int i);
    inline static  void             setDestructPtr(void (*fPtr)(GlobalCondition &c));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
    inline         GlobalConditionTask &setNewGcnTask(double t, double (*tPtr)(GlobalConditionTask &t));
    inline static  void             setGcnNumber(long l);
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
    inline static  DatFile         &getDatFile();
  protected :
    // Friends
           friend  int              System::main(Analyser &a);
           friend  class            LocalCondition;
           friend  class            GlobalConditionTask;
    // Member functions
    inline                          GlobalCondition(System &s, DatFile &f);
    inline                          GlobalCondition(const GlobalCondition &p);
    inline virtual                 ~GlobalCondition();
    inline         GlobalCondition &addLcn(LocalCondition &c);
    inline static  long             getGcnInstanceCounter();
    inline         VecConObject    &getLcnArray();
    inline         GlobalCondition &operator =(const GlobalCondition &p);
    // Data members
                   long             gcnId;
           static  long             gcnInstanceCounter;
           static  void           (*constructPtr)(GlobalCondition &c);
           static  void           (*destructPtr)(GlobalCondition &c);
           static  long             gcnNumber;
                   VecConObject    *theLcnArrayPtr;
           static  DatFile          theDatFile;
};

class GlobalConditionTask : public SimObjectTask
{ public :
    inline         GlobalCondition &getGcn() {return (GlobalCondition &)getSimObject();}
  private :
    // Friends
           friend  class            GlobalCondition;
    // Member functions
    inline                          GlobalConditionTask(GlobalCondition &p, double t, double (*tPtr)(GlobalConditionTask &t));
    inline                          GlobalConditionTask(const GlobalConditionTask &t);
    inline virtual                 ~GlobalConditionTask();
    inline         GlobalConditionTask &operator =(const GlobalConditionTask &t);
    inline virtual void             run();
    // Data members
                   double         (*taskPtr)(GlobalConditionTask &t);
};

class GlobalResource : public SysObject
{ public :
    // Member functions
    inline         LocalResource   &getCspLrs(long h) const;
    inline static  ofstream        &getDmpFile();
    inline static  long             getNumber();
    inline         long             getGrsId() const;
    inline         void             printToLogFile();
    inline static  void             setConstructPtr(void (*fPtr)(GlobalResource &r));
    inline static  void             setDblNumber(int i);
    inline static  void             setDestructPtr(void (*fPtr)(GlobalResource &r));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
    inline         GlobalResourceTask &setNewGrsTask(double t, double (*tPtr)(GlobalResourceTask &t));
    inline static  void             setGrsNumber(long l);
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
    inline static  DatFile         &getDatFile();
  protected :
    // Friends
           friend  int              System::main(Analyser &a);
           friend  class            LocalResource;
           friend  class            GlobalResourceTask;
    // Member functions
    inline                          GlobalResource(System &s, DatFile &f);
    inline                          GlobalResource(const GlobalResource &p);
    inline virtual                 ~GlobalResource();
    inline         GlobalResource  &addLrs(LocalResource &r);
    inline static  long             getGrsInstanceCounter();
    inline         VecConObject    &getLrsArray();
    inline         GlobalResource  &operator =(const GlobalResource &p);
    // Data members
           static  void           (*constructPtr)(GlobalResource &r);
           static  void           (*destructPtr)(GlobalResource &r);
           static  long             grsNumber;
                   long             grsId;
           static  long             grsInstanceCounter;
           static  DatFile          theDatFile;
                   VecConObject    *theLrsArrayPtr;
};

class GlobalResourceTask : public SimObjectTask
{ public :
    inline         GlobalResource  &getGrs() {return (GlobalResource &)getSimObject();}
  private :
    // Friends
           friend  class            GlobalResource;
    // Member functions
    inline                          GlobalResourceTask(GlobalResource &p, double t, double (*tPtr)(GlobalResourceTask &t));
    inline                          GlobalResourceTask(const GlobalResourceTask &t);
    inline virtual                 ~GlobalResourceTask();
    inline         GlobalResourceTask &operator =(const GlobalResourceTask &t);
    inline virtual void             run();
    // Data members
                   double         (*taskPtr)(GlobalResourceTask &t);
};

class Population : public SysObject
{ public :
    // Member functions
                   long             getCspGblIndNumber() const;
    inline         Individual      &getCspInd(long h, long i) const;
    inline         long             getCspIndNumber(long h) const;
    inline         Individual      &getCspRndInd(long h) const;
    inline static  ofstream        &getDmpFile();
    inline static  long             getNumber();
    inline         void             printToLogFile();
    inline         long             getPopId() const;
    inline static  void             setConstructPtr(void (*fPtr)(Population &p));
    inline static  void             setDblNumber(int i);
    inline         Population      &setDeleteInd(Individual &i);
    inline static  void             setDestructPtr(void (*fPtr)(Population &p));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
    inline         Individual      &setNewInd(Individual &i);
    inline         PopulationTask  &setNewPopTask(double t, double (*tPtr)(PopulationTask &t));
    inline static  void             setPopNumber(long l);
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
    inline static  DatFile         &getDatFile();
  protected :
    // Friends
           friend  int              System::main(Analyser &a);
           friend  class            Individual;
           friend  class            PopulationTask;
    // Member functions
    inline                          Population(System &s, DatFile &f);
    inline                          Population(const Population &p);
    inline virtual                 ~Population();
    inline         Population      &addInd(long h, Individual &i);
    inline         Population      &addIndBtree(BtrConObject &b);
    inline         BtrConObject    &getIndBtree(long h) const;
    inline         VecConObject    &getIndBtreeArray();
    inline         Individual      &detachInd(long h, Individual &i);
    inline static  long             getPopInstanceCounter();
    inline         Population      &operator =(const Population &p);
    // Data members
           static  void           (*constructPtr)(Population &p);
           static  void           (*destructPtr)(Population &p);
                   long             popId;
           static  long             popInstanceCounter;
           static  long             popNumber;
           static  DatFile          theDatFile;
                   VecConObject    *theIndBtreeArrayPtr;
};

class PopulationTask : public SimObjectTask
{ public :
    inline         Population      &getPop() {return (Population &)getSimObject();}
  private :
    // Friends
           friend  class            Population;
    // Member functions
    inline                          PopulationTask(Population &p, double t, double (*tPtr)(PopulationTask &t));
    inline                          PopulationTask(const PopulationTask &t);
    inline virtual                 ~PopulationTask();
    inline         PopulationTask  &operator =(const PopulationTask &t);
    inline virtual void             run();
    // Data members
                   double         (*taskPtr)(PopulationTask &t);
};

class Habitat : public SysObject
{ public :
    // Member functions
    inline         long             getHabId() const;
    inline         LocalCondition  &getLocLcn(long c) const;
    inline         Individual      &getLocInd(long p, long i) const;
    inline         long             getLocIndNumber(long p) const;
    inline         LocalResource   &getLocLrs(long r) const;
    inline         Individual      &getLocRndInd(long p) const;
    inline static  ofstream        &getDmpFile();
    inline static  long             getNumber();
                   long             getTotLocIndNumber() const;
    inline         void             printToLogFile();
    inline static  void             setConstructPtr(void (*fPtr)(Habitat &h));
    inline static  void             setDblNumber(int i);
    inline static  void             setDestructPtr(void (*fPtr)(Habitat &h));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
    inline         HabitatTask     &setNewHabTask(double t, double (*tPtr)(HabitatTask &t));
    inline static  void             setHabNumber(long l);
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
    inline static  DatFile         &getDatFile();
  protected :
    // Friends
           friend  int              System::main(Analyser &a);
           friend  class            LocalCondition;
           friend  class            LocalResource;
           friend  class            Individual;
           friend  class            HabitatTask;
    // Member functions
    inline                          Habitat(System &s, DatFile &f);
    inline                          Habitat(const Habitat &h);
    inline virtual                 ~Habitat();
    inline         Habitat         &addLcn(LocalCondition &c);
    inline         Habitat         &addIndBtree(BtrConObject &b);
    inline         VecConObject    &getLcnArray();
    inline         Habitat         &addLrs(LocalResource &r);
    inline static  long             getHabInstanceCounter();
    inline         BtrConObject    &getIndBtree(long p) const;
    inline         VecConObject    &getIndBtreeArray();
    inline         VecConObject    &getLrsArray();
    inline         Habitat         &operator =(const Habitat &h);
    // Data members
           static  void           (*constructPtr)(Habitat &h);
           static  void           (*destructPtr)(Habitat &h);
                   long             habId;
           static  long             habInstanceCounter;
           static  long             habNumber;
                   VecConObject    *theLcnArrayPtr;
                   VecConObject    *theIndBtreeArrayPtr;
                   VecConObject    *theLrsArrayPtr;
           static  DatFile          theDatFile;
};

class HabitatTask : public SimObjectTask
{ public :
    inline         Habitat         &getHab() {return (Habitat &)getSimObject();}
  private :
    // Friends
           friend  class            Habitat;
    // Member functions
    inline                          HabitatTask(Habitat &s, double t, double (*tPtr)(HabitatTask &t));
    inline                          HabitatTask(const HabitatTask &t);
    inline virtual                 ~HabitatTask();
    inline         HabitatTask     &operator =(const HabitatTask &t);
    inline virtual void             run();
    // Data members
           double                 (*taskPtr)(HabitatTask &t);
};

class HabObject : public SysObject
{ public :
    // Member functions
    inline         long             getHabId() const;
    inline         Habitat         &getLocHab() const;
    inline         Individual      &getLocInd(long p, long i) const;
    inline         long             getLocIndNumber(long p) const;
    inline         LocalCondition  &getLocLcn(long c) const;
    inline         LocalResource   &getLocLrs(long r) const;
    inline         Individual      &getLocRndInd(long p) const;
    inline         long             getObjLngHabId() const;
    inline         long             getTotLocIndNumber() const;
    inline         HabObject       &setObjLngHabId(long h);
  protected :
    // Friends
           friend  class            Individual;
    // Member functions
    inline                          HabObject(System &s, DatFile &f);
    inline                          HabObject(System &s, const DatObject &d);
    inline                          HabObject(const HabObject &h);
    inline virtual                 ~HabObject();
    inline         HabObject       &operator =(const HabObject &h);
    inline         HabObject       &setHabitatPtr(Habitat *hPtr);
    // Data members
                   Habitat         *theHabitatPtr;
};

class LocalCondition : public HabObject
{ public :
    // Member functions
    inline         GlobalCondition &getCspGcn() const;
    inline static  ofstream        &getDmpFile();
    inline         long             getGcnId() const;
    inline         long             getLcnId() const;
    inline         long             getLcnLngGcnId() const;
    inline         long             getLcnLngHabId() const;
//    inline         LocalConditionStlt &getLcnStlt(int i) const;
    inline         void             printToLogFile();
    inline         LocalCondition  &setLcnLngGcnId(long p);
    inline         LocalCondition  &setLcnLngHabId(long p);
    inline static  void             setConstructPtr(void (*fPtr)(LocalCondition &i));
    inline static  void             setDblNumber(int i);
    inline static  void             setDestructPtr(void (*fPtr)(LocalCondition &i));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
//    inline         LocalConditionStlt &setNewStlt(int f);
    inline         LocalConditionTask &setNewLcnTask(double t, double (*tPtr)(LocalConditionTask &t));
    inline static  void             setUpdatePtr(void (*fPtr)(LocalCondition &t));
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
           static  double           update(SystemTask &t);
    // Data members
                   LocalConditionLink *theLcnLink;
    inline static  DatFile         &getDatFile();
  protected :
    // Friends
           friend  int              System::main(Analyser &a);
           friend  class            LocalConditionTask;
    // Member functions
    inline                          LocalCondition(System &s, DatFile &f);
    inline                          LocalCondition(System &s, const DatObject &d);
    inline                          LocalCondition(const LocalCondition &i);
    inline virtual                 ~LocalCondition();
    inline static  long             getLcnInstanceCounter();
    inline         LocalCondition  &operator =(const LocalCondition &i);
    // Data members
           static  void           (*constructPtr)(LocalCondition &i);
           static  void           (*updatePtr)(LocalCondition &i);
           static  void           (*destructPtr)(LocalCondition &i);
                   long             lcnId;
           static  long             lcnInstanceCounter;
                   GlobalCondition &theGlobalCondition;
           static  DatObject       *theLcnDatObjectPtr;
           static  DatFile          theDatFile;
};

class LocalConditionLink : public SrtObject
{ public :
    // Member functions
    inline                          LocalConditionLink(LocalCondition &i);
    inline                          LocalConditionLink(const LocalConditionLink &l);
    inline virtual                 ~LocalConditionLink();
    inline virtual int              getClassId() const;
    inline virtual char            *getClassName() const;
    inline virtual long             getHashId() const;
    inline         LocalCondition  &getLcn() const;
    inline virtual int              isEqual(const WmmObject &o) const;
    inline virtual int              isSmaller(const WmmObject &o) const;
    inline         LocalConditionLink &operator =(const LocalConditionLink &l);
    inline virtual void             setTo(ostream &o) const;
  private :
    // Data members
                   LocalCondition  &theLcn;
};

/*
class LocalConditionStlt : public SimObjectStlt
{ public :
    // Member functions
    inline                          LocalConditionStlt(LocalCondition &c, int f);
    inline                          LocalConditionStlt(const LocalConditionStlt &s);
    inline virtual                 ~LocalConditionStlt();
    inline         LocalCondition  &getLcn() const;
    inline         LocalConditionStlt &operator =(const LocalConditionStlt &l);
  private :
    // Data members
};
*/

class LocalConditionTask : public SimObjectTask
{ public :
    inline         LocalCondition  &getLcn() {return (LocalCondition &)getSimObject();}
  protected :
    // Friends
           friend  class            LocalCondition;
    // Member functions
    inline                          LocalConditionTask(LocalCondition &i, double t, double (*tPtr)(LocalConditionTask &t));
    inline                          LocalConditionTask(const LocalConditionTask &t);
    inline virtual                 ~LocalConditionTask();
    inline         LocalConditionTask &operator =(const LocalConditionTask &t);
    inline virtual void             run();
    // Data members
                   double         (*taskPtr)(LocalConditionTask &t);
};

class LocalResource : public HabObject
{ public :
    // Member functions
    inline         GlobalResource  &getCspGrs() const;
    inline static  ofstream        &getDmpFile();
    inline         long             getGrsId() const;
    inline         long             getLrsId() const;
    inline         long             getLrsLngHabId() const;
    inline         long             getLrsLngGrsId() const;
//    inline         LocalResourceStlt &getLrsStlt(int i) const;
    inline         void             printToLogFile();
    inline static  void             setConstructPtr(void (*fPtr)(LocalResource &i));
    inline static  void             setDblNumber(int i);
    inline static  void             setDestructPtr(void (*fPtr)(LocalResource &i));
    inline static  void             setFltNumber(int i);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
//    inline         LocalResourceStlt &setNewStlt(int f);
    inline         LocalResourceTask &setNewLrsTask(double t, double (*tPtr)(LocalResourceTask &t));
    inline         LocalResource   &setLrsLngHabId(long p);
    inline         LocalResource   &setLrsLngGrsId(long p);
    inline static  void             setUpdatePtr(void (*fPtr)(LocalResource &t));
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
           static  double           update(SystemTask &t);
    // Data members
                   LocalResourceLink *theLrsLink;
    inline static  DatFile         &getDatFile();
  protected :
    // Friends
           friend  int              System::main(Analyser &a);
           friend  class            LocalResourceTask;
    // Member functions
    inline                          LocalResource(System &s, DatFile &f);
    inline                          LocalResource(System &s, const DatObject &d);
    inline                          LocalResource(const LocalResource &i);
    inline virtual                 ~LocalResource();
    inline static  long             getLrsInstanceCounter();
    inline         LocalResource   &operator =(const LocalResource &i);
    // Data members
           static  void           (*constructPtr)(LocalResource &i);
           static  void           (*updatePtr)(LocalResource &i);
           static  void           (*destructPtr)(LocalResource &i);
                   long             lrsId;
           static  long             lrsInstanceCounter;
           static  DatFile          theDatFile;
           static  DatObject       *theLrsDatObjectPtr;
                   GlobalResource  &theGlobalResource;
};

class LocalResourceLink : public SrtObject
{ public :
    // Member functions
    inline                          LocalResourceLink(LocalResource &i);
    inline                          LocalResourceLink(const LocalResourceLink &l);
    inline virtual                 ~LocalResourceLink();
    inline virtual int              getClassId() const;
    inline virtual char            *getClassName() const;
    inline virtual long             getHashId() const;
    inline         LocalResource    &getLrs() const;
    inline virtual int              isEqual(const WmmObject &o) const;
    inline virtual int              isSmaller(const WmmObject &o) const;
    inline         LocalResourceLink &operator =(const LocalResourceLink &l);
    inline virtual void             setTo(ostream &o) const;
  private :
    // Data members
                   LocalResource   &theLrs;
};

/*
class LocalResourceStlt : public SimObjectStlt
{ public :
    // Member functions
    inline                          LocalResourceStlt(LocalResource &r, int f);
    inline                          LocalResourceStlt(const LocalResourceStlt &s);
    inline virtual                 ~LocalResourceStlt();
    inline         LocalResource   &getLrs() const;
    inline         LocalResourceStlt &operator =(const LocalResourceStlt &l);
  private :
    // Data members
};
*/

class LocalResourceTask : public SimObjectTask
{ public :
    inline         LocalResource   &getLrs() {return (LocalResource &)getSimObject();}
  protected :
    // Friends
           friend  class            LocalResource;
    // Member functions
    inline                          LocalResourceTask(LocalResource &i, double t, double (*tPtr)(LocalResourceTask &t));
    inline                          LocalResourceTask(const LocalResourceTask &t);
    inline virtual                 ~LocalResourceTask();
    inline         LocalResourceTask &operator =(const LocalResourceTask &t);
    inline virtual void             run();
    // Data members
                   double         (*taskPtr)(LocalResourceTask &t);
};

class Individual : public HabObject
{ public :
    // Member functions
    inline         long             getCspGblIndNumber() const;
    inline         Individual      &getCspInd(long h, long i) const;
    inline         long             getCspIndNumber(long h) const;
    inline         Individual      &getCspLocInd(long i) const;
    inline         long             getCspLocIndNumber() const;
    inline         Individual      &getCspLocRndInd() const;
    inline         Population      &getCspPop() const;
    inline         Individual      &getCspRndInd(long h) const;
    inline static  ofstream        &getDmpFile();
    inline         long             getIndId() const;
    inline         long             getIndLngHabId() const;
    inline         long             getIndLngPopId() const;
//    inline         IndividualStlt  &getIndStlt(int i) const;
    inline         long             getPopId() const;
    inline         void             printToLogFile();
    inline void                     resetIndTask(int i);
    inline static  void             setConstructPtr(void (*fPtr)(Individual &i));
    inline static  void             setDblNumber(int i);
    inline         Individual      &setDeleteThisInd();
    inline         Individual      &setDeleteInd(Individual &i);
    inline static  void             setDestructPtr(void (*fPtr)(Individual &i));
    inline static  void             setFltNumber(int i);
    inline         Individual      &setIndLngHabId(long p);
    inline         Individual      &setIndLngPopId(long p);
    inline         Individual      &setLocRndHab();
    inline         Individual      &setLocHab(Habitat &h);
    inline static  void             setIntNumber(int i);
    inline static  void             setLngNumber(int i);
    inline         Individual      &setNewInd(Individual &i);
//    inline         IndividualStlt  &setNewStlt(int f);
    inline         IndividualTask  &setNewIndTask(double t, double (*tPtr)(IndividualTask &t));
    inline         Individual      &setNewThisInd();
    inline static  void             setStrNumber(int i);
    inline static  void             setSumNumber(int i);
           static  double           create(SystemTask &t);
    // Data members
                   IndividualLink  *theIndLink;
    inline static  DatFile         &getDatFile();
  protected :
    // Friends
           friend  int              System::main(Analyser &a);
           friend  Population      &Population::setDeleteInd(Individual &i);
           friend  Individual      &Population::setNewInd(Individual &i);
           friend  class            IndividualTask;
    // Member functions
    inline                          Individual(System &s, DatFile &f);
    inline                          Individual(System &s, const DatObject &d);
    inline                          Individual(const Individual &i);
    inline virtual                 ~Individual();
    inline static  long             getIndInstanceCounter();
    inline static  int              getMakeOrbituary();
    inline         Individual      &operator =(const Individual &i);
    inline static  void             setMakeOrbituary(int o);
    // Data members
           static  void           (*constructPtr)(Individual &i);
           static  void           (*destructPtr)(Individual &i);
                   long             indId;
           static  long             indInstanceCounter;
           static  int              makeOrbituary;
           static  DatFile          theDatFile;
           static  DatObject       *theIndDatObjectPtr;
                   Population      &thePopulation;
};

class IndividualLink : public SrtObject
{ public :
    // Member functions
    inline                          IndividualLink(Individual &i);
    inline                          IndividualLink(const IndividualLink &l);
    inline virtual                 ~IndividualLink();
    inline virtual int              getClassId() const;
    inline virtual char            *getClassName() const;
    inline virtual long             getHashId() const;
    inline         Individual      &getInd() const;
    inline virtual int              isEqual(const WmmObject &o) const;
    inline virtual int              isSmaller(const WmmObject &o) const;
    inline         IndividualLink &operator =(const IndividualLink &l);
    inline virtual void             setTo(ostream &o) const;
  private :
    // Data members
            Individual             &theInd;
};

/*
class IndividualStlt : public SimObjectStlt
{ public :
    // Member functions
    inline                          IndividualStlt(Individual &i, int f);
    inline                          IndividualStlt(const IndividualStlt &s);
    inline virtual                 ~IndividualStlt();
    inline         Individual      &getInd() const;
    inline         IndividualStlt  &operator =(const IndividualStlt &l);
  private :
    // Data members
};
*/

class IndividualTask : public SimObjectTask
{ public :
    inline         Individual      &getInd() {return (Individual &)getSimObject();}
  protected :
    // Friends
           friend  class            Individual;
    // Member functions
    inline                          IndividualTask(Individual &i, double t, double (*tPtr)(IndividualTask &t));
    inline                          IndividualTask(const IndividualTask &t);
    inline virtual                 ~IndividualTask();
    inline         IndividualTask  &operator =(const IndividualTask &t);
    inline virtual void             run();
    // Data members
           double                 (*taskPtr)(IndividualTask &t);
};

inline void Analyser::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void System::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void GlobalCondition::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void GlobalResource::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void Population::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void Habitat::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void LocalCondition::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void LocalResource::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

inline void Individual::printToLogFile()
{   DatObject::printToLogFile(getDatFile());
}

// Member functions Analyser

inline Analyser::Analyser(DatFile &f) : SimObject (f)
{   (*constructPtr)(*this);
}

inline Analyser::Analyser(const Analyser &a) : SimObject(a)
{   theOutObject.printError("Copy constructor Analyser not yet implemented");
    a;
}

inline Analyser::~Analyser()
{   (*destructPtr)(*this);
}

inline double Analyser::getAnaDblReady() const
{   return getDbl(ANA_DBL_READY);
}

inline long Analyser::getAnaLngReady() const
{   return getLng(ANA_LNG_READY);
}

inline Analyser &Analyser::getAna()
{   return *theAnalyserPtr;
}

inline GlobalCondition &Analyser::getGcn(long c) const
{   return getSys().getGcn(c);
}

inline LocalCondition &Analyser::getLcn(long c, long h) const
{   return getSys().getLcn(c, h);
}

inline long Analyser::getGcnNumber() const
{   return getSys().getGcnNumber();
}

inline DatFile &Analyser::getDatFile()
{   return theDatFile;
}

inline long Analyser::getGblIndNumber(long p) const
{   return getSys().getGblIndNumber(p);
}

inline Habitat &Analyser::getHab(long h) const
{   return getSys().getHab(h);
}

inline long Analyser::getHabNumber() const
{   return getSys().getHabNumber();
}

inline Individual &Analyser::getInd(long p, long h, long i) const
{   return getSys().getInd(p, h, i);
}

inline long Analyser::getIndNumber(long p, long h) const
{   return getSys().getIndNumber(p, h);
}

inline ofstream &Analyser::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline Population &Analyser::getPop(long p) const
{   return getSys().getPop(p);
}

inline long Analyser::getPopNumber() const
{   return getSys().getPopNumber();
}

inline GlobalResource &Analyser::getGrs(long r) const
{   return getSys().getGrs(r);
}

inline LocalResource &Analyser::getLrs(long r, long h) const
{   return getSys().getLrs(r, h);
}

inline long Analyser::getGrsNumber() const
{   return getSys().getGrsNumber();
}

inline Individual &Analyser::getRndInd(long p, long h) const
{   return getSys().getRndInd(p, h);
}

inline System &Analyser::getSys() const
{   return *theSystemPtr;
}

inline long Analyser::getTotGblIndNumber() const
{   return getSys().getTotGblIndNumber();
}

inline Analyser &Analyser::operator =(const Analyser &a)
{   theOutObject.printError("Assignment operator Analyser not yet implemented");
    a;
    return *this;
}

inline Analyser &Analyser::setAnaDblReady(double t)
{   setDbl(ANA_DBL_READY, t);
    return *this;
}

inline Analyser &Analyser::setAnaLngReady(long n)
{   setLng(ANA_LNG_READY, n);
    return *this;
}

inline void Analyser::setConstructPtr(void (*fPtr)(Analyser &a))
{   constructPtr = fPtr;
}

inline void Analyser::setDataPtr(void (*fPtr)())
{   dataPtr = fPtr;
}

inline void Analyser::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline void Analyser::setDestructPtr(void (*fPtr)(Analyser &a))
{   destructPtr = fPtr;
}

inline void Analyser::setEvaluatePtr(void (*fPtr)(Analyser &a))
{   evaluatePtr = fPtr;
}

inline void Analyser::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void Analyser::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void Analyser::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

inline void Analyser::setOutOfMemoryPtr(void (*fPtr)())
{   outOfMemoryPtr = fPtr;
}

inline void Analyser::setMainPtr(int (*fPtr)(Analyser &a))
{   mainPtr = fPtr;
}

inline void Analyser::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void Analyser::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

inline void Analyser::setAnalyserPtr(Analyser *aPtr)
{   theAnalyserPtr = aPtr;
}

inline Analyser &Analyser::setSystemPtr(System *sPtr)
{   theSystemPtr = sPtr;
    return *this;
}

// Member functions AnalyserTask

inline AnalyserTask::AnalyserTask(Analyser &s, double t, double (*tPtr)(AnalyserTask &t)) :
  SimObjectTask(s, t), taskPtr(tPtr)
{
}

inline AnalyserTask::AnalyserTask(const AnalyserTask &t) : SimObjectTask(t)
{   theOutObject.printError("Copy constructor AnalyserTask not yet implemented");
    t;
}

inline AnalyserTask::~AnalyserTask()
{
}

inline AnalyserTask &AnalyserTask::operator =(const AnalyserTask &t)
{   theOutObject.printError("Assignment operator AnalyserTask not yet implemented");
    t;
    return *this;
}

inline void AnalyserTask::run()
{   time = (*taskPtr)(*this);
}

inline AnalyserTask &Analyser::setNewAnaTask(double t, double (*tPtr)(AnalyserTask &t))
{   return *(new AnalyserTask(*this, t, tPtr));
}

// Member functions System

inline System::System(Analyser &a, DatFile &f) :
  SimObject(f), theAnalyser(a)
  , theHabArrayPtr(0), theGcnArrayPtr(0), theGrsArrayPtr(0), thePopArrayPtr(0)
//{   theHabArrayPtr = new VecConObject(Habitat::getNumber());
//    theGcnArrayPtr = new VecConObject(GlobalCondition::getNumber());
//    theGrsArrayPtr = new VecConObject(GlobalResource::getNumber());
//    thePopArrayPtr = new VecConObject(Population::getNumber());
{   theHabArrayPtr = new VecConObject(Habitat::getDatFile().getInputRecordNumber());
    theGcnArrayPtr = new VecConObject(GlobalCondition::getDatFile().getInputRecordNumber());
    theGrsArrayPtr = new VecConObject(GlobalResource::getDatFile().getInputRecordNumber());
    thePopArrayPtr = new VecConObject(Population::getDatFile().getInputRecordNumber());
    getAna().setSystemPtr(this);
    (*constructPtr)(*this);
}

inline System::System(const System &s) :
  SimObject(s), theAnalyser(s.theAnalyser)
  , theHabArrayPtr(0), theGcnArrayPtr(0), theGrsArrayPtr(0), thePopArrayPtr(0)
{   theOutObject.printError("Copy constructor System not yet implemented");
    s;
}

inline System::~System()
{   (*destructPtr)(*this);
    if (Analyser::version != 302) printToOutFile(System::getDatFile());
    if (thePopArrayPtr)
    {   delete thePopArrayPtr;
        thePopArrayPtr = 0;
    }
    if (theGrsArrayPtr)
    {   delete theGrsArrayPtr;
        theGrsArrayPtr = 0;
    }
    if (theGcnArrayPtr)
    {   delete theGcnArrayPtr;
        theGcnArrayPtr = 0;
    }
    if (theHabArrayPtr)
    {   delete theHabArrayPtr;
        theHabArrayPtr = 0;
    }
}

inline System &System::addGcn(GlobalCondition &p)
{   getGcnArray().set(p);
    return *this;
}

inline System &System::addHab(Habitat &h)
{   getHabArray().set(h);
    return *this;
}

inline System &System::addPop(Population &p)
{   getPopArray().set(p);
    return *this;
}

inline System &System::addGrs(GlobalResource &p)
{   getGrsArray().set(p);
    return *this;
}

inline Analyser &System::getAna() const
{   return theAnalyser;
}

inline GlobalCondition &System::getGcn(long c) const
{   if (c < 0 || c >= getGcnNumber())
    {   theOutObject.printError("Out of System::GlobalCondition array limits");
    }
    return (GlobalCondition &)(*theGcnArrayPtr)[(int)c];
}

inline VecConObject &System::getGcnArray()
{   return *theGcnArrayPtr;
}

inline LocalCondition &System::getLcn(long c, long h) const
{   return getGcn(c).getCspLcn(h);
}

inline long System::getGcnNumber() const
{   return (*theGcnArrayPtr).getNumber();
}

inline DatFile &System::getDatFile()
{   return theDatFile;
}

inline long System::getGblIndNumber(long p) const
{   return getPop(p).getCspGblIndNumber();
}

inline Habitat &System::getHab(long h) const
{   if (h < 0 || h >= getHabNumber())
    {   theOutObject.printError("Out of System::Habitat array limits");
    }
    return (Habitat &)(*theHabArrayPtr)[(int)h];
}

inline VecConObject &System::getHabArray()
{   return *theHabArrayPtr;
}

inline long System::getHabNumber() const
{   return (*theHabArrayPtr).getNumber();
}

inline Individual &System::getInd(long p, long h, long i) const
{   return getPop(p).getCspInd(h, i);
}

inline long System::getIndNumber(long p, long h) const
{   return getPop(p).getCspIndNumber(h);
}

inline ofstream &System::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline Population &System::getPop(long p) const
{   if (p < 0 || p >= getPopNumber())
    {   theOutObject.printError("Out of System::Population array limits");
    }
    return (Population &)(*thePopArrayPtr)[(int)p];
}

inline VecConObject &System::getPopArray()
{   return *thePopArrayPtr;
}

inline long System::getPopNumber() const
{   return (*thePopArrayPtr).getNumber();
}

inline GlobalResource &System::getGrs(long r) const
{   if (r < 0 || r >= getGrsNumber())
    {   theOutObject.printError("Out of System::GlobalResource array limits");
    }
    return (GlobalResource &)(*theGrsArrayPtr)[(int)r];
}

inline VecConObject &System::getGrsArray()
{   return *theGrsArrayPtr;
}

inline LocalResource &System::getLrs(long r, long h) const
{   return getGrs(r).getCspLrs(h);
}

inline long System::getGrsNumber() const
{   return (*theGrsArrayPtr).getNumber();
}

inline Individual &System::getRndInd(long p, long h) const
{   return getPop(p).getCspRndInd(h);
}

inline System &System::operator =(const System &s)
{   theOutObject.printError("Assignment operator System not yet implemented");
    s;
    return *this;
}

inline void System::setConstructPtr(void (*fPtr)(System &s))
{   constructPtr = fPtr;
}

inline void System::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline void System::setDestructPtr(void (*fPtr)(System &s))
{   destructPtr = fPtr;
}

inline void System::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void System::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void System::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

inline void System::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void System::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

inline void System::setUpdatePtr(void (*fPtr)(System &s))
{   updatePtr = fPtr;
}

// Member functions SystemTask

inline SystemTask::SystemTask(System &s, double t, double (*tPtr)(SystemTask &t)) :
  SimObjectTask(s, t), taskPtr(tPtr)
{
}

inline SystemTask::SystemTask(const SystemTask &t) : SimObjectTask(t)
{   theOutObject.printError("Copy constructor SystemTask not yet implemented");
    t;
}

inline SystemTask::~SystemTask()
{
}

inline SystemTask &SystemTask::operator =(const SystemTask &t)
{   theOutObject.printError("Assignment operator SystemTask not yet implemented");
    t;
    return *this;
}

inline void SystemTask::run()
{   time = (*taskPtr)(*this);
}

inline SystemTask &System::setNewSysTask(double t, double (*tPtr)(SystemTask &t))
{   return *(new SystemTask(*this, t, tPtr));
}

// Member functions SysObject

inline SysObject::SysObject(System &s, DatFile &f) :
  SimObject(f), theSystem(s)
{
}

inline SysObject::SysObject(System &s, const DatObject &d) :
  SimObject(d), theSystem(s)
{
}

inline SysObject::SysObject(const SysObject &s) :
  SimObject(s), theSystem(s.theSystem)
{
}

inline SysObject::~SysObject()
{
}

inline Analyser &SysObject::getAna() const
{   return getSys().getAna();
}

inline GlobalCondition &SysObject::getGcn(long c) const
{   return getSys().getGcn(c);
}

inline LocalCondition &SysObject::getLcn(long c, long h) const
{   return getSys().getLcn(c, h);
}

inline long SysObject::getGcnNumber() const
{   return getSys().getGcnNumber();
}

inline long SysObject::getGblIndNumber(long p) const
{   return getSys().getGblIndNumber(p);
}

inline Habitat &SysObject::getHab(long h) const
{   return getSys().getHab(h);
}

inline long SysObject::getHabNumber() const
{   return getSys().getHabNumber();
}

inline Individual &SysObject::getInd(long p, long h, long i) const
{   return getSys().getInd(p, h, i);
}

inline long SysObject::getIndNumber(long p, long h) const
{   return getSys().getIndNumber(p, h);
}

inline Population &SysObject::getPop(long p) const
{   return getSys().getPop(p);
}

inline long SysObject::getPopNumber() const
{   return getSys().getPopNumber();
}

inline GlobalResource &SysObject::getGrs(long r) const
{   return getSys().getGrs(r);
}

inline LocalResource &SysObject::getLrs(long r, long h) const
{   return getSys().getLrs(r, h);
}

inline long SysObject::getGrsNumber() const
{   return getSys().getGrsNumber();
}

inline Individual &SysObject::getRndInd(long p, long h) const
{   return getSys().getRndInd(p, h);
}

inline System &SysObject::getSys() const
{   return theSystem;
}

inline long SysObject::getTotGblIndNumber() const
{   return getSys().getTotGblIndNumber();
}

inline SysObject &SysObject::operator =(const SysObject &s)
{   theOutObject.printError("Assignment operator SysObject not yet implemented");
    s;
    return *this;
}

// Member functions GlobalCondition

inline GlobalCondition::GlobalCondition(System &s, DatFile &f) :
  SysObject(s, f), gcnId(GlobalCondition::gcnInstanceCounter++)
  , theLcnArrayPtr(0)
//{   theLcnArrayPtr = new VecConObject(Habitat::getNumber());
{   theLcnArrayPtr = new VecConObject(Habitat::getDatFile().getInputRecordNumber());
    if (getSimLngId() != gcnId)
    {   theOutObject.printError("Unexpected GlobalCondition Id");
    }
    s.addGcn(*this);
    (*constructPtr)(*this);
}

inline GlobalCondition::GlobalCondition(const GlobalCondition &p) :
  SysObject(p)
  , theLcnArrayPtr(0)
{   theOutObject.printError("Copy constructor GlobalCondition not yet implemented");
    p;
}

inline GlobalCondition::~GlobalCondition()
{   (*destructPtr)(*this);
    if (Analyser::version != 302) printToOutFile(GlobalCondition::getDatFile());
    if (theLcnArrayPtr)
    {   delete theLcnArrayPtr;
        theLcnArrayPtr = 0;
    }
}

inline GlobalCondition &GlobalCondition::addLcn(LocalCondition &c)
{   getLcnArray().set(c);
    return *this;
}

inline LocalCondition &GlobalCondition::getCspLcn(long h) const
{   if (h < 0 || h >= getHabNumber())
    {   theOutObject.printError("Out of GlobalCondition::LocalCondition array limits");
    }
    return (LocalCondition &)(*theLcnArrayPtr)[(int)h];
}

inline long GlobalCondition::getGcnId() const
{   return gcnId;
}

inline long GlobalCondition::getGcnInstanceCounter()
{   return gcnInstanceCounter;
}

inline VecConObject &GlobalCondition::getLcnArray()
{   return *theLcnArrayPtr;
}

inline DatFile &GlobalCondition::getDatFile()
{   return theDatFile;
}

inline ofstream &GlobalCondition::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline long GlobalCondition::getNumber()
{   return gcnNumber;
}

inline GlobalCondition &GlobalCondition::operator =(const GlobalCondition &p)
{   theOutObject.printError("Assignment operator GlobalCondition not yet implemented");
    p;
    return *this;
}

inline void GlobalCondition::setConstructPtr(void (*fPtr)(GlobalCondition &c))
{   constructPtr = fPtr;
}

inline void GlobalCondition::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline void GlobalCondition::setDestructPtr(void (*fPtr)(GlobalCondition &c))
{   destructPtr = fPtr;
}

inline void GlobalCondition::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void GlobalCondition::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void GlobalCondition::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

inline void GlobalCondition::setGcnNumber(long l)
{   gcnNumber = l;
}

inline void GlobalCondition::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void GlobalCondition::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

// Member functions GlobalConditionTask

inline GlobalConditionTask::GlobalConditionTask(GlobalCondition &p, double t, double (*tPtr)(GlobalConditionTask &t)) :
  SimObjectTask(p, t), taskPtr(tPtr)
{
}

inline GlobalConditionTask::GlobalConditionTask(const GlobalConditionTask &t) : SimObjectTask(t)
{   theOutObject.printError("Copy constructor GlobalConditionTask not yet implemented");
    t;
}

inline GlobalConditionTask::~GlobalConditionTask()
{
}

inline GlobalConditionTask &GlobalConditionTask::operator =(const GlobalConditionTask &t)
{   theOutObject.printError("Assignment operator GlobalConditionTask not yet implemented");
    t;
    return *this;
}

inline void GlobalConditionTask::run()
{   time = (*taskPtr)(*this);
}

inline GlobalConditionTask &GlobalCondition::setNewGcnTask(double t, double (*tPtr)(GlobalConditionTask &t))
{   return *(new GlobalConditionTask(*this, t, tPtr));
}

// Member functions GlobalResource

inline GlobalResource::GlobalResource(System &s, DatFile &f) :
  SysObject(s, f), grsId(GlobalResource::grsInstanceCounter++)
  , theLrsArrayPtr(0)
//{   theLrsArrayPtr = new VecConObject(Habitat::getNumber());
{   theLrsArrayPtr = new VecConObject(Habitat::getDatFile().getInputRecordNumber());
    if (getSimLngId() != grsId)
    {   theOutObject.printError("Unexpected GlobalResource Id");
    }
    s.addGrs(*this);
    (*constructPtr)(*this);
}

inline GlobalResource::GlobalResource(const GlobalResource &p) :
  SysObject(p)
  , theLrsArrayPtr(0)
{   theOutObject.printError("Copy constructor GlobalResource not yet implemented");
    p;
}

inline GlobalResource::~GlobalResource()
{   (*destructPtr)(*this);
    if (Analyser::version != 302) printToOutFile(GlobalResource::getDatFile());
    if (theLrsArrayPtr)
    {   delete theLrsArrayPtr;
        theLrsArrayPtr = 0;
    }
}

inline GlobalResource &GlobalResource::addLrs(LocalResource &r)
{   getLrsArray().set(r);
    return *this;
}

inline LocalResource &GlobalResource::getCspLrs(long h) const
{   if (h < 0 || h >= getHabNumber())
    {   theOutObject.printError("Out of GlobalResource::LocalResource array limits");
    }
    return (LocalResource &)(*theLrsArrayPtr)[(int)h];
}

inline DatFile &GlobalResource::getDatFile()
{   return theDatFile;
}

inline ofstream &GlobalResource::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline long GlobalResource::getNumber()
{   return grsNumber;
}

inline long GlobalResource::getGrsId() const
{   return grsId;
}

inline long GlobalResource::getGrsInstanceCounter()
{   return grsInstanceCounter;
}

inline VecConObject &GlobalResource::getLrsArray()
{   return *theLrsArrayPtr;
}

inline GlobalResource &GlobalResource::operator =(const GlobalResource &p)
{   theOutObject.printError("Assignment operator GlobalResource not yet implemented");
    p;
    return *this;
}

inline void GlobalResource::setConstructPtr(void (*fPtr)(GlobalResource &r))
{   constructPtr = fPtr;
}

inline void GlobalResource::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline void GlobalResource::setDestructPtr(void (*fPtr)(GlobalResource &r))
{   destructPtr = fPtr;
}

inline void GlobalResource::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void GlobalResource::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void GlobalResource::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

inline void GlobalResource::setGrsNumber(long l)
{   grsNumber = l;
}

inline void GlobalResource::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void GlobalResource::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

// Member functions GlobalResourceTask

inline GlobalResourceTask::GlobalResourceTask(GlobalResource &p, double t, double (*tPtr)(GlobalResourceTask &t)) :
  SimObjectTask(p, t), taskPtr(tPtr)
{
}

inline GlobalResourceTask::GlobalResourceTask(const GlobalResourceTask &t) : SimObjectTask(t)
{   theOutObject.printError("Copy constructor GlobalResourceTask not yet implemented");
    t;
}

inline GlobalResourceTask::~GlobalResourceTask()
{
}

inline GlobalResourceTask &GlobalResourceTask::operator =(const GlobalResourceTask &t)
{   theOutObject.printError("Assignment operator GlobalResourceTask not yet implemented");
    t;
    return *this;
}

inline void GlobalResourceTask::run()
{   time = (*taskPtr)(*this);
}

inline GlobalResourceTask &GlobalResource::setNewGrsTask(double t, double (*tPtr)(GlobalResourceTask &t))
{   return *(new GlobalResourceTask(*this, t, tPtr));
}

// Member functions Population

inline Population::Population(System &s, DatFile &f) :
  SysObject(s, f), popId(Population::popInstanceCounter++)
  , theIndBtreeArrayPtr(0)
//{   theIndBtreeArrayPtr = new VecConObject(Habitat::getNumber());
{   theIndBtreeArrayPtr = new VecConObject(Habitat::getDatFile().getInputRecordNumber());
    if (getSimLngId() != popId)
    {   theOutObject.printError("Unexpected Population Id");
    }
    s.addPop(*this);
    (*constructPtr)(*this);
}

inline Population::Population(const Population &p) :
  SysObject(p)
  , theIndBtreeArrayPtr(0)
{   theOutObject.printError("Copy constructor Population not yet implemented");
    p;
}

inline Population::~Population()
{   (*destructPtr)(*this);
    if (Analyser::version != 302) printToOutFile(Population::getDatFile());
    if (theIndBtreeArrayPtr)
    {   delete theIndBtreeArrayPtr;
        theIndBtreeArrayPtr = 0;
    }
}

inline Population &Population::addInd(long h, Individual &i)
{   getIndBtree(h).set(i);
    return *this;
}

inline Population &Population::addIndBtree(BtrConObject &b)
{   getIndBtreeArray().set(b);
    return *this;
}

inline Individual &Population::getCspInd(long h, long i) const
{   if (i < 0 || i >= getCspIndNumber(h))
    {   theOutObject.printError("Out of Population::Individual btree limits");
    }
    return (Individual &)getIndBtree(h)[i];
}

inline Individual &Population::getCspRndInd(long h) const
{   return getCspInd(h, wmmRndLng(getCspIndNumber(h)));
}

inline long Population::getCspIndNumber(long h) const
{   return getIndBtree(h).getNumber();
}

inline Individual &Population::detachInd(long h, Individual &i)
{   getIndBtree(h).reset(i);
    return i;
}

inline DatFile &Population::getDatFile()
{   return theDatFile;
}

inline BtrConObject &Population::getIndBtree(long h) const
{   if (h < 0 || h >= getHabNumber())
    {   theOutObject.printError("Out of Population::IndividualBtree array limits");
    }
    return (BtrConObject &)(*theIndBtreeArrayPtr)[(int)h];
}

inline VecConObject &Population::getIndBtreeArray()
{   return *theIndBtreeArrayPtr;
}

inline ofstream &Population::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline long Population::getNumber()
{   return popNumber;
}

inline long Population::getPopId() const
{   return popId;
}

inline long Population::getPopInstanceCounter()
{   return popInstanceCounter;
}

inline Population &Population::operator =(const Population &p)
{   theOutObject.printError("Assignment operator Population not yet implemented");
    p;
    return *this;
}

inline void Population::setConstructPtr(void (*fPtr)(Population &p))
{   constructPtr = fPtr;
}

inline void Population::setDblNumber(int i)
{   theDatFile.setDblNumber(i);
}

inline Population &Population::setDeleteInd(Individual &i)
{   if (i != getNullObject())
    {   delete &i;
    }
    return *this;
}

inline void Population::setDestructPtr(void (*fPtr)(Population &p))
{   destructPtr = fPtr;
}

inline void Population::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void Population::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void Population::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

inline Individual &Population::setNewInd(Individual &i)
{   return *(new Individual(i));
}

inline void Population::setPopNumber(long l)
{   popNumber = l;
}

inline void Population::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void Population::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

// Member functions of PopulationTask

inline PopulationTask::PopulationTask(Population &p, double t, double (*tPtr)(PopulationTask &t)) :
  SimObjectTask(p, t), taskPtr(tPtr)
{
}

inline PopulationTask::PopulationTask(const PopulationTask &t) :
  SimObjectTask(t)
{   theOutObject.printError("Copy constructor PopulationTask not yet implemented");
    t;
}

inline PopulationTask::~PopulationTask()
{
}

inline PopulationTask &PopulationTask::operator =(const PopulationTask &t)
{   theOutObject.printError("Assignment operator PopulationTask not yet implemented");
    t;
    return *this;
}

inline void PopulationTask::run()
{   time = (*taskPtr)(*this);
}

inline PopulationTask &Population::setNewPopTask(double t, double (*tPtr)(PopulationTask &t))
{   return *(new PopulationTask(*this, t, tPtr));
}

// Member functions Habitat

inline Habitat::Habitat(System &s, DatFile &f) :
  SysObject(s, f), habId(Habitat::habInstanceCounter++)
  , theLcnArrayPtr(0), theLrsArrayPtr(0), theIndBtreeArrayPtr(0)
//{   theLcnArrayPtr = new VecConObject(GlobalCondition::getNumber());
//    theLrsArrayPtr = new VecConObject(GlobalResource::getNumber());
//    theIndBtreeArrayPtr = new VecConObject(Population::getNumber());
{   theLcnArrayPtr = new VecConObject(GlobalCondition::getDatFile().getInputRecordNumber());
    theLrsArrayPtr = new VecConObject(GlobalResource::getDatFile().getInputRecordNumber());
    theIndBtreeArrayPtr = new VecConObject(Population::getDatFile().getInputRecordNumber());
    getLcnArray().setOwner(0);
    getLrsArray().setOwner(0);
    getIndBtreeArray().setOwner(0);
    if (getSimLngId() != habId)
    {   theOutObject.printError("Unexpected Habitat Id");
    }
    s.addHab(*this);
    (*constructPtr)(*this);
}

inline Habitat::Habitat(const Habitat &h) :
  SysObject(h)
  , theLcnArrayPtr(0), theLrsArrayPtr(0), theIndBtreeArrayPtr(0)
{   theOutObject.printError("Copy constructor Habitat not yet implemented");
    h;
}

inline Habitat::~Habitat()
{   (*destructPtr)(*this);
    if (Analyser::version != 302) printToOutFile(Habitat::getDatFile());
    if (theIndBtreeArrayPtr)
    {   delete theIndBtreeArrayPtr;
        theIndBtreeArrayPtr = 0;
    }
    if (theLrsArrayPtr)
    {   delete theLrsArrayPtr;
        theLrsArrayPtr = 0;
    }
    if (theLcnArrayPtr)
    {   delete theLcnArrayPtr;
        theLcnArrayPtr = 0;
    }
}

inline Habitat &Habitat::addLcn(LocalCondition &c)
{   getLcnArray().set(c);
    return *this;
}

inline Habitat &Habitat::addIndBtree(BtrConObject &b)
{   getIndBtreeArray().set(b);
    return *this;
}

inline Habitat &Habitat::addLrs(LocalResource &r)
{   getLrsArray().set(r);
    return *this;
}

inline VecConObject &Habitat::getLcnArray()
{   return *theLcnArrayPtr;
}

inline DatFile &Habitat::getDatFile()
{   return theDatFile;
}

inline long Habitat::getHabId() const
{   return habId;
}

inline long Habitat::getHabInstanceCounter()
{   return habInstanceCounter;
}

inline BtrConObject &Habitat::getIndBtree(long p) const
{   if (p < 0 || p >= getPopNumber())
    {   theOutObject.printError("Out of Habitat::IndividualBtree array limits");
    }
    return (BtrConObject &)(*theIndBtreeArrayPtr)[(int)p];
}

inline VecConObject &Habitat::getIndBtreeArray()
{   return *theIndBtreeArrayPtr;
}

inline LocalCondition &Habitat::getLocLcn(long c) const
{   if (c < 0 || c >= getGcnNumber())
    {   theOutObject.printError("Out of Habitat::LocalCondition array limits");
    }
    return (LocalCondition &)(*theLcnArrayPtr)[(int)c];
}

inline Individual &Habitat::getLocInd(long p, long i) const
{   if (i < 0 || i >= getLocIndNumber(p))
    {   theOutObject.printError("Out of Habitat::Individual btree limits");
    }
    return (Individual &)getIndBtree(p)[i];
}

inline long Habitat::getLocIndNumber(long p) const
{   return getIndBtree(p).getNumber();
}

inline LocalResource &Habitat::getLocLrs(long r) const
{   if (r < 0 || r >= getGrsNumber())
    {   theOutObject.printError("Out of Habitat::LocalResource array limits");
    }
    return (LocalResource &)(*theLrsArrayPtr)[(int)r];
}

inline Individual &Habitat::getLocRndInd(long p) const
{   return getLocInd(p, wmmRndLng(getLocIndNumber(p)));
}

inline ofstream &Habitat::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline long Habitat::getNumber()
{   return habNumber;
}

inline VecConObject &Habitat::getLrsArray()
{   return *theLrsArrayPtr;
}

inline Habitat &Habitat::operator =(const Habitat &h)
{   theOutObject.printError("Assignment operator Habitat not yet implemented");
    h;
    return *this;
}

inline void Habitat::setConstructPtr(void (*fPtr)(Habitat &h))
{   constructPtr = fPtr;
}

inline void Habitat::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline void Habitat::setDestructPtr(void (*fPtr)(Habitat &h))
{   destructPtr = fPtr;
}

inline void Habitat::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void Habitat::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void Habitat::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

inline void Habitat::setHabNumber(long l)
{   habNumber = l;
}

inline void Habitat::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void Habitat::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

// Member functions of HabitatTask

inline HabitatTask::HabitatTask(Habitat &h, double t, double (*tPtr)(HabitatTask &t)) :
  SimObjectTask(h, t), taskPtr(tPtr)
{
}

inline HabitatTask::HabitatTask(const HabitatTask &t) : SimObjectTask(t)
{   theOutObject.printError("Copy constructor HabitatTask not yet implemented");
    t;
}

inline HabitatTask::~HabitatTask()
{
}

inline HabitatTask &HabitatTask::operator =(const HabitatTask &t)
{   theOutObject.printError("Assignment operator HabitatTask not yet implemented");
    t;
    return *this;
}

inline void HabitatTask::run()
{   time = (*taskPtr)(*this);
}

inline HabitatTask &Habitat::setNewHabTask(double t, double (*tPtr)(HabitatTask &t))
{   return *(new HabitatTask(*this, t, tPtr));
}

// Member functionsHabObject

inline HabObject::HabObject(System &s, DatFile &f) : SysObject(s, f),
  theHabitatPtr(&s.getHab(getObjLngHabId()))
{
}

inline HabObject::HabObject(System &s, const DatObject &d) : SysObject(s, d),
  theHabitatPtr(&s.getHab(getObjLngHabId()))
{
}

inline HabObject::HabObject(const HabObject &i) :
  SysObject(i), theHabitatPtr(i.theHabitatPtr)
{
}

inline HabObject::~HabObject()
{
}

inline long HabObject::getHabId() const
{   return getLocHab().getHabId();
}

inline long HabObject::getObjLngHabId() const
{   return getLng(IND_LNG_HAB_ID);
}

inline LocalCondition &HabObject::getLocLcn(long c) const
{   return getLocHab().getLocLcn(c);
}

inline Habitat &HabObject::getLocHab() const
{   return *theHabitatPtr;
}

inline Individual &HabObject::getLocInd(long p, long i) const
{   return getLocHab().getLocInd(p, i);
}

inline long HabObject::getLocIndNumber(long p) const
{   return getLocHab().getLocIndNumber(p);
}

inline LocalResource &HabObject::getLocLrs(long r) const
{   return getLocHab().getLocLrs(r);
}

inline Individual &HabObject::getLocRndInd(long p) const
{   return getLocHab().getLocRndInd(p);
}

inline long HabObject::getTotLocIndNumber() const
{   return getLocHab().getTotLocIndNumber();
}

inline HabObject &HabObject::operator =(const HabObject &h)
{   theOutObject.printError("Assignment operator HabObject not yet implemented");
    h;
    return *this;
}

inline HabObject &HabObject::setHabitatPtr(Habitat *hPtr)
{   theHabitatPtr = hPtr;
    return *this;
}

inline HabObject &HabObject::setObjLngHabId(long h)
{   setLng(IND_LNG_HAB_ID, h);
    return *this;
}

// Member functions LocalCondition

inline LocalCondition::LocalCondition(System &s, DatFile &f) :
  HabObject(s, f), lcnId(LocalCondition::lcnInstanceCounter++),
  theGlobalCondition(s.getGcn(getLcnLngGcnId())), theLcnLink(0)
{   if (getSimLngId() != lcnId)
    {   theOutObject.printError("Unexpected LocalCondition Id");
    }
    if (getLcnLngGcnId() * getHabNumber() +
      getLcnLngHabId() != lcnId)
    {   theOutObject.printError("Unexpected LocalCondition HabId or ConId");
    }
    getCspGcn().addLcn(*this);
    getLocHab().addLcn(*this);
    (*constructPtr)(*this);
}

inline LocalCondition::LocalCondition(System &s, const DatObject &d) :
  HabObject(s, d), lcnId(LocalCondition::lcnInstanceCounter++),
  theGlobalCondition(s.getGcn(getLcnLngGcnId())), theLcnLink(0)
{   getCspGcn().addLcn(*this);
    getLocHab().addLcn(*this);
    (*constructPtr)(*this);
}

inline LocalCondition::LocalCondition(const LocalCondition &p) :
  HabObject(p), theGlobalCondition(p.theGlobalCondition), theLcnLink(0)
{   theOutObject.printError("Copy constructor LocalCondition not yet implemented");
    p;
}

inline LocalCondition::~LocalCondition()
{   (*destructPtr)(*this);
    if (Analyser::version != 302) printToOutFile(LocalCondition::getDatFile());
}

inline GlobalCondition &LocalCondition::getCspGcn() const
{   return theGlobalCondition;
}

inline long LocalCondition::getGcnId() const
{   return getCspGcn().getGcnId();
}

inline long LocalCondition::getLcnId() const
{   return lcnId;
}

inline long LocalCondition::getLcnInstanceCounter()
{   return lcnInstanceCounter;
}

inline long LocalCondition::getLcnLngGcnId() const
{   return getLng(LCN_LNG_GCN_ID);
}

inline long LocalCondition::getLcnLngHabId() const
{   return getLng(LCN_LNG_HAB_ID);
}

//inline LocalConditionStlt &LocalCondition::getLcnStlt(int i) const
//{   return (LocalConditionStlt &)getSimObjectStlt(i);
//}

inline DatFile &LocalCondition::getDatFile()
{   return theDatFile;
}

inline ofstream &LocalCondition::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline LocalCondition &LocalCondition::operator =(const LocalCondition &i)
{   theOutObject.printError("Assignment operator LocalCondition not yet implemented");
    i;
    return *this;
}

inline LocalCondition &LocalCondition::setLcnLngGcnId(long p)
{   setLng(LCN_LNG_GCN_ID, p);
    return *this;
}

inline LocalCondition &LocalCondition::setLcnLngHabId(long p)
{   setLng(LCN_LNG_HAB_ID, p);
    return *this;
}

inline void LocalCondition::setConstructPtr(void (*fPtr)(LocalCondition &i))
{   constructPtr = fPtr;
}

inline void LocalCondition::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline void LocalCondition::setDestructPtr(void (*fPtr)(LocalCondition &i))
{   destructPtr = fPtr;
}

inline void LocalCondition::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void LocalCondition::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void LocalCondition::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

//inline LocalConditionStlt &LocalCondition::setNewStlt(int f)
//{   return *(new LocalConditionStlt(*this, f));
//}

inline void LocalCondition::setUpdatePtr(void (*fPtr)(LocalCondition &c))
{   updatePtr = fPtr;
}

inline void LocalCondition::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void LocalCondition::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

// Member functions LocalConditionLink

inline LocalConditionLink::LocalConditionLink(LocalCondition &i) : theLcn(i)
{
}

inline LocalConditionLink::LocalConditionLink(const LocalConditionLink &l) : theLcn(l.theLcn)
{   theOutObject.printError("Copy constructor LocalConditionLink not yet implemented");
    l;
}

inline LocalConditionLink::~LocalConditionLink()
{
}

inline int LocalConditionLink::getClassId() const
{   return conditionItemLinkClass;
}

inline char *LocalConditionLink::getClassName() const
{   return "LocalConditionLink";
}

inline long LocalConditionLink::getHashId() const
{   return theLcn.getSimLngId();
}

inline LocalCondition &LocalConditionLink::getLcn() const
{   return theLcn;
}

inline int LocalConditionLink::isEqual(const WmmObject &o) const
{   return theLcn == ((LocalConditionLink &)o).theLcn;
}

inline int LocalConditionLink::isSmaller(const WmmObject &o) const
{   float theValue = theLcn.getSimFltValue();
    float theOtherValue = ((LocalConditionLink &)o).theLcn.getSimFltValue();
    if (theValue != theOtherValue)
    {   return theValue < theOtherValue;
    }
    else
    {   return theLcn < ((LocalConditionLink &)o).theLcn;
    }
}

inline LocalConditionLink &LocalConditionLink::operator =(const LocalConditionLink &l)
{   theOutObject.printError("Assignment operator LocalResourceLink not yet implemented");
    l;
    return *this;
}

inline void LocalConditionLink::setTo(ostream &o) const
{   o << getClassName();
}

/*
// Member functions LocalConditionStlt

inline LocalConditionStlt::LocalConditionStlt(LocalCondition &c, int f) : SimObjectStlt(c, f)
{
}

inline LocalConditionStlt::LocalConditionStlt(const LocalConditionStlt &s) : SimObjectStlt(s)
{   theOutObject.printError("Copy constructor LocalConditionStlt not yet implemented");
    s;
}

inline LocalConditionStlt::~LocalConditionStlt()
{
}

inline LocalCondition &LocalConditionStlt::getLcn() const
{   return (LocalCondition &)getSimObject();
}

inline LocalConditionStlt &LocalConditionStlt::operator =(const LocalConditionStlt &l)
{   theOutObject.printError("Assignment operator LocalConditionStlt not yet implemented");
    l;
    return *this;
}
*/

// Member functions of LocalConditionTask

inline LocalConditionTask::LocalConditionTask(LocalCondition &i, double t, double (*tPtr)(LocalConditionTask &t)) :
  SimObjectTask(i, t), taskPtr(tPtr)
{
}

inline LocalConditionTask::LocalConditionTask(const LocalConditionTask &t) :
  SimObjectTask(t)
{   theOutObject.printError("Copy constructor LocalConditionTask not yet implemented");
    t;
}

inline LocalConditionTask::~LocalConditionTask()
{
}

inline LocalConditionTask &LocalConditionTask::operator =(const LocalConditionTask &t)
{   theOutObject.printError("Assignment operator LocalConditionTask not yet implemented");
    t;
    return *this;
}

inline void LocalConditionTask::run()
{   time = (*taskPtr)(*this);
}

inline LocalConditionTask &LocalCondition::setNewLcnTask(double t, double (*tPtr)(LocalConditionTask &t))
{   return *(new LocalConditionTask(*this, t, tPtr));
}

// Member functions LocalResource

inline LocalResource::LocalResource(System &s, DatFile &f) :
  HabObject(s, f), lrsId(LocalResource::lrsInstanceCounter++),
  theGlobalResource(s.getGrs(getLrsLngGrsId())), theLrsLink(0)
{   if (getSimLngId() != lrsId)
    {   theOutObject.printError("Unexpected LocalResource Id");
    }
    if (getLrsLngGrsId() * getHabNumber() +
      getLrsLngHabId() != lrsId)
    {   theOutObject.printError("Unexpected LocalResource HabId or ConId");
    }
    getCspGrs().addLrs(*this);
    getLocHab().addLrs(*this);
    (*constructPtr)(*this);
}

inline LocalResource::LocalResource(System &s, const DatObject &d) :
  HabObject(s, d), lrsId(LocalResource::lrsInstanceCounter++),
  theGlobalResource(s.getGrs(getLrsLngGrsId())), theLrsLink(0)
{   getCspGrs().addLrs(*this);
    getLocHab().addLrs(*this);
    (*constructPtr)(*this);
}

inline LocalResource::LocalResource(const LocalResource &p) :
  HabObject(p), theGlobalResource(p.theGlobalResource), theLrsLink(0)
{   theOutObject.printError("Copy constructor GlobalResource not yet implemented");
    p;
}

inline LocalResource::~LocalResource()
{   (*destructPtr)(*this);
    if (Analyser::version != 302) printToOutFile(LocalResource::getDatFile());
}

inline GlobalResource &LocalResource::getCspGrs() const
{   return theGlobalResource;
}

inline DatFile &LocalResource::getDatFile()
{   return theDatFile;
}

inline ofstream &LocalResource::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline long LocalResource::getGrsId() const
{   return getCspGrs().getGrsId();
}

inline long LocalResource::getLrsId() const
{   return lrsId;
}

inline long LocalResource::getLrsInstanceCounter()
{   return lrsInstanceCounter;
}

inline long LocalResource::getLrsLngHabId() const
{   return getLng(LRS_LNG_HAB_ID);
}

inline long LocalResource::getLrsLngGrsId() const
{   return getLng(LRS_LNG_GRS_ID);
}

//inline LocalResourceStlt &LocalResource::getLrsStlt(int i) const
//{   return (LocalResourceStlt &)getSimObjectStlt(i);
//}

inline LocalResource &LocalResource::operator =(const LocalResource &i)
{   theOutObject.printError("Assignment operator LocalResource not yet implemented");
    i;
    return *this;
}

inline void LocalResource::setConstructPtr(void (*fPtr)(LocalResource &i))
{   constructPtr = fPtr;
}

inline void LocalResource::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline void LocalResource::setDestructPtr(void (*fPtr)(LocalResource &i))
{   destructPtr = fPtr;
}

inline void LocalResource::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline void LocalResource::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void LocalResource::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

//inline LocalResourceStlt &LocalResource::setNewStlt(int f)
//{   return *(new LocalResourceStlt(*this, f));
//}

inline LocalResource &LocalResource::setLrsLngHabId(long p)
{   setLng(LRS_LNG_HAB_ID, p);
    return *this;
}

inline LocalResource &LocalResource::setLrsLngGrsId(long p)
{   setLng(LRS_LNG_GRS_ID, p);
    return *this;
}

inline void LocalResource::setUpdatePtr(void (*fPtr)(LocalResource &r))
{   updatePtr = fPtr;
}

inline void LocalResource::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void LocalResource::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

// Member functions LocalResourceLink

inline LocalResourceLink::LocalResourceLink(LocalResource &i) : theLrs(i)
{
}

inline LocalResourceLink::LocalResourceLink(const LocalResourceLink &l) : theLrs(l.theLrs)
{   theOutObject.printError("Copy constructor LocalResourceLink not yet implemented");
    l;
}

inline LocalResourceLink::~LocalResourceLink()
{
}

inline int LocalResourceLink::getClassId() const
{   return resourceItemLinkClass;
}

inline char *LocalResourceLink::getClassName() const
{   return "LocalResourceLink";
}

inline long LocalResourceLink::getHashId() const
{   return theLrs.getSimLngId();
}

inline LocalResource &LocalResourceLink::getLrs() const
{   return theLrs;
}

inline int LocalResourceLink::isEqual(const WmmObject &o) const
{   return theLrs == ((LocalResourceLink &)o).theLrs;
}

inline int LocalResourceLink::isSmaller(const WmmObject &o) const
{   float theValue = theLrs.getSimFltValue();
    float theOtherValue = ((LocalResourceLink &)o).theLrs.getSimFltValue();
    if (theValue != theOtherValue)
    {   return theValue < theOtherValue;
    }
    else
    {   return theLrs < ((LocalResourceLink &)o).theLrs;
    }
}

inline LocalResourceLink &LocalResourceLink::operator =(const LocalResourceLink &l)
{   theOutObject.printError("Assignment operator LocalResourceLink not yet implemented");
    l;
    return *this;
}

inline void LocalResourceLink::setTo(ostream &o) const
{   o << getClassName();
}

/*
// Member functions LocalResourceStlt

inline LocalResourceStlt::LocalResourceStlt(LocalResource &r, int f) : SimObjectStlt(r, f)
{
}

inline LocalResourceStlt::LocalResourceStlt(const LocalResourceStlt &s) : SimObjectStlt(s)
{   theOutObject.printError("Copy constructor LocalResourceStlt not yet implemented");
    s;
}

inline LocalResourceStlt::~LocalResourceStlt()
{
}

inline LocalResource &LocalResourceStlt::getLrs() const
{   return (LocalResource &)getSimObject();
}

inline LocalResourceStlt &LocalResourceStlt::operator =(const LocalResourceStlt &l)
{   theOutObject.printError("Assignment operator LocalResourceStlt not yet implemented");
    l;
    return *this;
}
*/

// Member functions of LocalResourceTask

inline LocalResourceTask::LocalResourceTask(LocalResource &i, double t, double (*tPtr)(LocalResourceTask &t)) :
  SimObjectTask(i, t), taskPtr(tPtr)
{
}

inline LocalResourceTask::LocalResourceTask(const LocalResourceTask &t) :
  SimObjectTask(t)
{   theOutObject.printError("Copy constructor LocalResourceTask not yet implemented");
    t;
}

inline LocalResourceTask::~LocalResourceTask()
{
}

inline LocalResourceTask &LocalResourceTask::operator =(const LocalResourceTask &t)
{   theOutObject.printError("Assignment operator LocalResourceTask not yet implemented");
    t;
    return *this;
}

inline void LocalResourceTask::run()
{   time = (*taskPtr)(*this);
}

inline LocalResourceTask &LocalResource::setNewLrsTask(double t, double (*tPtr)(LocalResourceTask &t))
{   return *(new LocalResourceTask(*this, t, tPtr));
}

// Member functions Individual

inline Individual::Individual(System &s, DatFile &f) :
  HabObject(s, f), indId(Individual::indInstanceCounter++),
  thePopulation(s.getPop(getIndLngPopId())), theIndLink(0)
{   setSimLngId(indId);
    getCspPop().addInd(getHabId(), *this);
    (*constructPtr)(*this);
}

inline Individual::Individual(System &s, const DatObject &d) :
  HabObject(s, d), indId(Individual::indInstanceCounter++),
  thePopulation(s.getPop(getIndLngPopId())), theIndLink(0)
{   setSimLngId(indId);
    getCspPop().addInd(getHabId(), *this);
    (*constructPtr)(*this);
}

inline Individual::Individual(const Individual &i) :
  HabObject(i), indId(Individual::indInstanceCounter++),
  thePopulation(i.thePopulation), theIndLink(0)
{   setSimLngId(indId);
    getCspPop().addInd(getHabId(), *this);
    (*constructPtr)(*this);
}

inline Individual::~Individual()
{   (*destructPtr)(*this);
    getCspPop().detachInd(getHabId(), *this);
    if (Individual::makeOrbituary)
    {   if (Analyser::version != 302) printToOutFile(Individual::getDatFile());
    }
}

inline long Individual::getCspGblIndNumber() const
{   return getCspPop().getCspGblIndNumber();
}

inline Individual &Individual::getCspInd(long h, long i) const
{   return getCspPop().getCspInd(h, i);
}

inline long Individual::getCspIndNumber(long h) const
{   return getCspPop().getCspIndNumber(h);
}

inline Individual &Individual::getCspLocInd(long i) const
{   return getCspPop().getCspInd(getHabId(), i);
}

inline long Individual::getCspLocIndNumber() const
{   return getCspPop().getCspIndNumber(getHabId());
}

inline Individual &Individual::getCspLocRndInd() const
{   return getCspPop().getCspRndInd(getHabId());
}

inline Population &Individual::getCspPop() const
{   return thePopulation;
}

inline Individual &Individual::getCspRndInd(long h) const
{   return getCspPop().getCspRndInd(h);
}

inline DatFile &Individual::getDatFile()
{   return theDatFile;
}

inline long Individual::getIndId() const
{   return indId;
}

inline long Individual::getIndInstanceCounter()
{   return indInstanceCounter;
}

inline long Individual::getIndLngHabId() const
{   return getLng(IND_LNG_HAB_ID);
}

inline long Individual::getIndLngPopId() const
{   return getLng(IND_LNG_POP_ID);
}

//inline IndividualStlt &Individual::getIndStlt(int i) const
//{   return (IndividualStlt &)getSimObjectStlt(i);
//}

inline ofstream &Individual::getDmpFile()
{   return getDatFile().getDmpFile();
}

inline int Individual::getMakeOrbituary()
{   return makeOrbituary;
}

inline long Individual::getPopId() const
{   return getCspPop().getPopId();
}

inline Individual &Individual::operator =(const Individual &i)
{   theOutObject.printError("Assignment operator Individual not yet implemented");
    i;
    return *this;
}

inline void Individual::setConstructPtr(void (*fPtr)(Individual &i))
{   constructPtr = fPtr;
}

inline void Individual::setDblNumber(int i)
{   getDatFile().setDblNumber(i);
}

inline Individual &Individual::setDeleteThisInd()
{   SimObjectTask::setRunStatus(SIM_DELETE);
    return *this;
}

inline Individual &Individual::setDeleteInd(Individual &i)
{   if (i != *this && i != getNullObject())
    {   delete &i;
    }
    return *this;
}

inline void Individual::setDestructPtr(void (*fPtr)(Individual &i))
{   destructPtr = fPtr;
}

inline void Individual::setFltNumber(int i)
{   getDatFile().setFltNumber(i);
}

inline Individual &Individual::setIndLngHabId(long p)
{   setLng(IND_LNG_HAB_ID, p);
    return *this;
}

inline Individual &Individual::setIndLngPopId(long p)
{   setLng(IND_LNG_POP_ID, p);
    return *this;
}

inline void Individual::setIntNumber(int i)
{   getDatFile().setIntNumber(i);
}

inline void Individual::setLngNumber(int i)
{   getDatFile().setLngNumber(i);
}

inline Individual &Individual::setLocHab(Habitat &h)
{   getCspPop().detachInd(getHabId(), *this);
    setHabitatPtr(&h);
    setObjLngHabId(getHabId());
    getCspPop().addInd(getHabId(), *this);
    return *this;
}

inline Individual &Individual::setLocRndHab()
{   setLocHab(getHab(wmmRndLng(getHabNumber())));
    return *this;
}

inline void Individual::setMakeOrbituary(int o)
{   makeOrbituary = o;
}

inline Individual &Individual::setNewInd(Individual &i)
{   return *(new Individual(i));
}

//inline IndividualStlt &Individual::setNewStlt(int f)
//{   return *(new IndividualStlt(*this, f));
//}

inline Individual &Individual::setNewThisInd()
{   return *(new Individual(*this));
}

inline void Individual::setStrNumber(int i)
{   getDatFile().setStrNumber(i);
}

inline void Individual::setSumNumber(int i)
{   getDatFile().setSumNumber(i);
}

// Member functions IndividualLink

inline IndividualLink::IndividualLink(Individual &i) : theInd(i)
{
}

inline IndividualLink::IndividualLink(const IndividualLink &l) : theInd(l.theInd)
{   theOutObject.printError("Copy constructor IndividualLink not yet implemented");
    l;
}

inline IndividualLink::~IndividualLink()
{
}

inline int IndividualLink::getClassId() const
{   return individualLinkClass;
}

inline char *IndividualLink::getClassName() const
{   return "IndividualLink";
}

inline long IndividualLink::getHashId() const
{   return theInd.getSimLngId();
}

inline Individual &IndividualLink::getInd() const
{   return theInd;
}

inline int IndividualLink::isEqual(const WmmObject &o) const
{   return theInd == ((IndividualLink &)o).theInd;
}

inline int IndividualLink::isSmaller(const WmmObject &o) const
{   float theValue = theInd.getSimFltValue();
    float theOtherValue = ((IndividualLink &)o).theInd.getSimFltValue();
    if (theValue != theOtherValue)
    {   return theValue < theOtherValue;
    }
    else
    {   return theInd < ((IndividualLink &)o).theInd;
    }
}

inline IndividualLink &IndividualLink::operator =(const IndividualLink &l)
{   theOutObject.printError("Assignment operator IndividualLink not yet implemented");
    l;
    return *this;
}

inline void IndividualLink::setTo(ostream &o) const
{   o << getClassName();
}

/*
// Member functions IndividualStlt

inline IndividualStlt::IndividualStlt(Individual &i, int f) : SimObjectStlt(i, f)
{
}

inline IndividualStlt::IndividualStlt(const IndividualStlt &s) : SimObjectStlt(s)
{   theOutObject.printError("Copy constructor IndividualStlt not yet implemented");
    s;
}

inline IndividualStlt::~IndividualStlt()
{
}

inline Individual &IndividualStlt::getInd() const
{   return (Individual &)getSimObject();
}

inline IndividualStlt &IndividualStlt::operator =(const IndividualStlt &l)
{   theOutObject.printError("Assignment operator IndividualStlt not yet implemented");
    l;
    return *this;
}
*/

// Member functions of IndividualTask

inline IndividualTask::IndividualTask(Individual &i, double t, double (*tPtr)(IndividualTask &t)) :
  SimObjectTask(i, t), taskPtr(tPtr)
{
}

inline IndividualTask::IndividualTask(const IndividualTask &t) :
  SimObjectTask(t)
{   theOutObject.printError("Copy constructor IndividualTask not yet implemented");
    t;
}

inline IndividualTask::~IndividualTask()
{
}

inline IndividualTask &IndividualTask::operator =(const IndividualTask &t)
{   theOutObject.printError("Assignment operator IndividualTask not yet implemented");
    t;
    return *this;
}

inline void IndividualTask::run()
{   time = (*taskPtr)(*this);
}

inline IndividualTask &Individual::setNewIndTask(double t, double (*tPtr)(IndividualTask &t))
{   return *(new IndividualTask(*this, t, tPtr));
}

inline void Individual::resetIndTask(int i)
{   SimObjectTask::theSimObjectTaskBtree.reset((SimObjectTask &)theSimObjectTaskArray.get(i));
}

// Member functions System

inline double System::update(SystemTask &t)
{   System &s = t.getSys();
    s.condCopyData(*System::theSysDatObjectPtr);
    (*updatePtr)(s);
    if (System::getDatFile().isMoreInput())
    {   System::theSysDatObjectPtr->readData(System::getDatFile());
        return System::theSysDatObjectPtr->getDbl(SIM_DBL_TIME);
    }
    else
    {   if (System::theSysDatObjectPtr)
        {   delete System::theSysDatObjectPtr;
        }
        System::theSysDatObjectPtr = 0;
        SimObjectTask::setRunStatus(SIM_NOT_ACTIVE);
        return 0.0;
    }
}

// Member functions LocalCondition

inline double LocalCondition::update(SystemTask &t)
{   System &s = t.getSys();
    LocalCondition &c = s.getLcn(LocalCondition::theLcnDatObjectPtr->getLng(LCN_LNG_GCN_ID),
      LocalCondition::theLcnDatObjectPtr->getLng(LCN_LNG_HAB_ID));
    c.condCopyData(*LocalCondition::theLcnDatObjectPtr);
    (*updatePtr)(c);
    if (LocalCondition::getDatFile().isMoreInput())
    {   LocalCondition::theLcnDatObjectPtr->readData(LocalCondition::getDatFile());
        //delete LocalCondition::theLcnDatObjectPtr;
        //LocalCondition::theLcnDatObjectPtr = 0;
        //LocalCondition::theLcnDatObjectPtr = new DatObject(LocalCondition::getDatFile());
        return LocalCondition::theLcnDatObjectPtr->getDbl(SIM_DBL_TIME);
    }
    else
    {   if (LocalCondition::theLcnDatObjectPtr)
        {   delete LocalCondition::theLcnDatObjectPtr;
        }
        LocalCondition::theLcnDatObjectPtr = 0;
        SimObjectTask::setRunStatus(SIM_NOT_ACTIVE);
        return 0.0;
    }
}

// Member functions LocalResource

inline double LocalResource::update(SystemTask &t)
{   System &s = t.getSys();
    LocalResource &r = s.getLrs(LocalResource::theLrsDatObjectPtr->getLng(LRS_LNG_GRS_ID),
      LocalResource::theLrsDatObjectPtr->getLng(LRS_LNG_HAB_ID));
    r.condCopyData(*LocalResource::theLrsDatObjectPtr);
    (*updatePtr)(r);
    if (LocalResource::getDatFile().isMoreInput())
    {   LocalResource::theLrsDatObjectPtr->readData(LocalResource::getDatFile());
        //delete LocalResource::theLrsDatObjectPtr;
        //LocalResource::theLrsDatObjectPtr = 0;
        //LocalResource::theLrsDatObjectPtr = new DatObject(LocalResource::getDatFile());
        return LocalResource::theLrsDatObjectPtr->getDbl(SIM_DBL_TIME);
    }
    else
    {   if (LocalResource::theLrsDatObjectPtr)
        {   delete LocalResource::theLrsDatObjectPtr;
        }
        LocalResource::theLrsDatObjectPtr = 0;
        SimObjectTask::setRunStatus(SIM_NOT_ACTIVE);
        return 0.0;
    }
}

// Member functions Individual

inline double Individual::create(SystemTask &t)
{   System &s = t.getSys();
    new Individual(s, *Individual::theIndDatObjectPtr);
    if (Individual::getDatFile().isMoreInput())
    {   //Individual::theIndDatObjectPtr->readData(Individual::getDatFile());
        if (Individual::theIndDatObjectPtr)
        {   delete Individual::theIndDatObjectPtr;
        }
        Individual::theIndDatObjectPtr = 0;
        Individual::theIndDatObjectPtr = new DatObject(Individual::getDatFile());
        return Individual::theIndDatObjectPtr->getDbl(SIM_DBL_TIME);
    }
    else
    {   if (Individual::theIndDatObjectPtr)
        {   delete Individual::theIndDatObjectPtr;
        }
        Individual::theIndDatObjectPtr = 0;
        SimObjectTask::setRunStatus(SIM_NOT_ACTIVE);
        return 0.0;
    }
}

#endif


