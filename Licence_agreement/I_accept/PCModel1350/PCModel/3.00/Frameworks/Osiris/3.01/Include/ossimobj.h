/*------------------------------------------------------------------------*/
/*  Ossimobj.h                                                            */
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

#ifndef __OSSIMOBJ_H
#define __OSSIMOBJ_H

#include "osdatobj.h"

// Constant definitions

#define SIM_LNG_ID      0
#define SIM_INT_STATUS  0
#define SIM_DBL_TIME    0
#define SIM_FLT_VALUE   0
#define SIM_STR_NAME    0
#define SIM_SUM_STAT    0

#define SIM_NOT_ACTIVE -1
#define SIM_DELETE      0
#define SIM_ACTIVE      1
#define SIM_READY       2

// Class declarations

class SimObject;
class SimObjectLink;
//class SimObjectStlt;
class SimObjectTask;

// Class definitions

class SimObject : public DatObject
{ public :
    // Member functions
            double         getSimDblTime() const;
            float          getSimFltValue() const;
            long           getSimId() const;
            int            getSimIntStatus() const;
            long           getSimLngId() const;
            SimObjectLink &getSimObjectLink(long i) const;
            long           getSimObjectLinkNumber() const;
//            SimObjectStlt &getSimObjectStlt(long i) const;
//            long           getSimObjectStltNumber() const;
            SimObjectTask &getSimObjectTask(long i) const;
            long           getSimObjectTaskNumber() const;
            StrObject     &getSimStrName() const;
            SumObject     &getSimSumStat() const;
                           operator const char *();
            SimObject     &setSimDblTime(double t);
            SimObject     &setSimFltValue(float v);
            SimObject     &setSimLngId(long i);
            SimObject     &setSimIntStatus(int i);
  protected :
    // Friends
    friend  class          SimObjectLink;
//    friend  class          SimObjectStlt;
    friend  class          SimObjectTask;
    // Member functions
                           SimObject(DatFile &f);
                           SimObject(const DatObject &d);
                           SimObject(const SimObject &s);
    virtual               ~SimObject();
            void           addSimObjectLink(WmmObject &o);
//            void           addSimObjectStlt(WmmObject &o);
            void           addSimObjectTask(WmmObject &o);
    static  long           getSimInstanceCounter();
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
                           operator long () const;
            SimObject     &operator=(const SimObject &s);
    virtual void           setTo(ostream &os) const;
    static  int            setCtrlBrk();
    // Data members
            long           simId;
    static  long           simInstanceCounter;
            VecConObject   theSimObjectLinkArray;
//            VecConObject   theSimObjectStltArray;
            VecConObject   theSimObjectTaskArray;
};

class SimObjectLink : public DatObject
{ public:
            SimObject     &getSimObjectFrom() const;
            SimObject     &getSimObjectTo() const;
  protected:
    // Member functions
                           SimObjectLink(DatFile &f);
                           SimObjectLink(const SimObjectLink &l);
    virtual               ~SimObjectLink();
            long           getLinkId() const;
    virtual long           getHashId() const;
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isSmaller(const WmmObject &o) const;
                           operator long () const;
            SimObjectLink &operator=(const SimObjectLink &l);
    virtual void           setTo(ostream &os) const;
    // Data members
            long           linkId;
            SimObject     *theSimObjectFromPtr;
            SimObject     *theSimObjectToPtr;
};

/*
class SimObjectStlt : public SrtObject
{ public:
            SimObject     &getSimObject() const;
  protected:
    // Member functions
                           SimObjectStlt(SimObject &s, int i);
                           SimObjectStlt(const SimObjectStlt &l);
    virtual               ~SimObjectStlt();
            long           getStltId() const;
    virtual long           getHashId() const;
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isSmaller(const WmmObject &o) const;
                           operator long () const;
            SimObjectStlt &operator=(const SimObjectStlt &l);
    virtual void           setTo(ostream &os) const;
    // Data members
            int            fltId;
            long           stltId;
            SimObject     &theSimObject;
};
*/

class SimObjectTask : public SrtObject
{ public :
    static  int            getRunStatus();
    static  BtrConObject  &getSimObjectTaskBtree();
            SimObject     &getSimObject() const;
            long           getTaskId() const;
            long           getNumber() const;
            void           setNumber(long n);
            double         getTime() const;
    static  void           setRunStatus(int s);
            SimObjectTask &setStopTask();
            SimObjectTask &setStopSimulation();
    static  int            runSimulation(); // check
    static  BtrConObject   theSimObjectTaskBtree; // check
  protected :
    // Member functions
                           SimObjectTask(SimObject &s, double t);
                           SimObjectTask(const SimObjectTask &t);
    virtual               ~SimObjectTask();
    virtual long           getHashId() const;
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isSmaller(const WmmObject &o) const;
                           operator long () const;
            SimObjectTask &operator=(const SimObjectTask &t);
    virtual void           setTo(ostream &os) const;
    virtual void           run() = 0;
            SimObjectTask &setTime(double t);
    // Data members
    static  int            runStatus;
            long           taskId;
            long           number;
            double         time;
            SimObject     &theSimObject;
};

// Member functions SimObject

inline SimObject::SimObject(DatFile &f) : DatObject(f),
  simId(simInstanceCounter++),
//  theSimObjectLinkArray(1, 1), theSimObjectStltArray(1, 1), theSimObjectTaskArray(1, 1)
  theSimObjectLinkArray(1, 1), theSimObjectTaskArray(1, 1)
{
}

inline SimObject::SimObject(const DatObject &d) : DatObject(d),
  simId(simInstanceCounter++),
//  theSimObjectLinkArray(1, 1), theSimObjectStltArray(1, 1), theSimObjectTaskArray(1, 1)
  theSimObjectLinkArray(1, 1), theSimObjectTaskArray(1, 1)
{
}

inline SimObject::SimObject(const SimObject &s) : DatObject(s),
  simId(simInstanceCounter++),
//  theSimObjectLinkArray(1, 1), theSimObjectStltArray(1, 1), theSimObjectTaskArray(1, 1)
  theSimObjectLinkArray(1, 1), theSimObjectTaskArray(1, 1)
{
}

inline SimObject::~SimObject()
{
}

inline void SimObject::addSimObjectLink(WmmObject &o)
{   theSimObjectLinkArray.set(o);
}

//inline void SimObject::addSimObjectStlt(WmmObject &o)
//{   theSimObjectStltArray.set(o);
//}

inline void SimObject::addSimObjectTask(WmmObject &o)
{   theSimObjectTaskArray.set(o);
}

inline double SimObject::getSimDblTime() const
{   return getDbl(SIM_DBL_TIME);
}

inline float SimObject::getSimFltValue() const
{   return getFlt(SIM_FLT_VALUE);
}

inline long SimObject::getSimId() const
{   return simId;
}

inline long SimObject::getSimInstanceCounter()
{   return simInstanceCounter;
}

inline int SimObject::getSimIntStatus() const
{   return getInt(SIM_INT_STATUS);
}

inline long SimObject::getSimLngId() const
{   return getLng(SIM_LNG_ID);
}

inline long SimObject::getSimObjectLinkNumber() const
{   return theSimObjectLinkArray.getNumber();
}

inline SimObjectLink &SimObject::getSimObjectLink(long i) const
{   if (i < 0 || i >= getSimObjectLinkNumber())
    {    theOutObject.printError("Out of link array limits");
    }
    return (SimObjectLink &)theSimObjectLinkArray[(int)i];
}

//inline long SimObject::getSimObjectStltNumber() const
//{   return theSimObjectStltArray.getNumber();
//}

//inline SimObjectStlt &SimObject::getSimObjectStlt(long i) const
//{   if (i < 0 || i >= getSimObjectStltNumber())
//    {    theOutObject.printError("Out of stlt array limits");
//    }
//    return (SimObjectStlt &)theSimObjectStltArray[(int)i];
//}

inline long SimObject::getSimObjectTaskNumber() const
{   return theSimObjectTaskArray.getNumber();
}

inline SimObjectTask &SimObject::getSimObjectTask(long i) const
{   if (i < 0 || i >= getSimObjectTaskNumber())
    {    theOutObject.printError("Out of task array limits");
    }
    return (SimObjectTask &)theSimObjectTaskArray[(int)i];
}

inline StrObject &SimObject::getSimStrName() const
{   return getStr(SIM_STR_NAME);
}

inline SumObject &SimObject::getSimSumStat() const
{   return getSum(SIM_SUM_STAT);
}

inline int SimObject::getClassId() const
{   return simObjectClass;
}

inline char *SimObject::getClassName() const
{   return "SimObject";
}

inline SimObject &SimObject::operator=(const SimObject &i)
{   theOutObject.printError("Assignment operator SimObject not yet implemented");
    i;
    return *this;
}

inline void SimObject::setTo(ostream &os) const
{   os << getSimStrName() << " "
       << getSimLngId() << " at "
       << getSimDblTime();
}

inline SimObject &SimObject::setSimDblTime(double t)
{   setDbl(SIM_DBL_TIME, t);
    return *this;
}

inline SimObject &SimObject::setSimFltValue(float v)
{   setFlt(SIM_FLT_VALUE, v);
    return *this;
}

inline SimObject &SimObject::setSimLngId(long i)
{   setLng(SIM_LNG_ID, i);
    return *this;
}

inline SimObject &SimObject::setSimIntStatus(int i)
{   setInt(SIM_INT_STATUS, i);
    return *this;
}

// Member functions SimObjectLink

inline SimObjectLink::SimObjectLink(DatFile &f) : DatObject(f)
{
}

inline SimObjectLink::SimObjectLink(const SimObjectLink &l) : DatObject(l)
{   theOutObject.printError("Copy constructor of SimObjectLink not yet implemented");
}

inline SimObjectLink::~SimObjectLink()
{
}

inline long SimObjectLink::getLinkId() const
{   return linkId;
}

inline SimObject &SimObjectLink::getSimObjectFrom() const
{   return *theSimObjectFromPtr;
}

inline SimObject &SimObjectLink::getSimObjectTo() const
{   return *theSimObjectToPtr;
}

inline long SimObjectLink::getHashId() const
{   return long(linkId);
}

inline int SimObjectLink::getClassId() const
{   return simObjectLinkClass;
}

inline char *SimObjectLink::getClassName() const
{   return "SimObjectLink";
}

inline int SimObjectLink::isEqual(const WmmObject &o) const
{   return theSimObjectFromPtr->getDatId() ==
      ((SimObjectLink &)o).theSimObjectFromPtr->getDatId() &&
      linkId == ((SimObjectLink &)o).linkId;
}

inline int SimObjectLink::isSmaller(const WmmObject &o) const
{   if (theSimObjectFromPtr->getDatId() !=
      ((SimObjectLink &)o).theSimObjectFromPtr->getDatId())
    {   return theSimObjectFromPtr->getDatId() <
          ((SimObjectLink &)o).theSimObjectFromPtr->getDatId();
    }
    else
    {   return linkId < ((SimObjectLink &)o).linkId;
    }
}

inline SimObjectLink::operator long() const
{   return linkId;
}

inline SimObjectLink &SimObjectLink::operator=(const SimObjectLink &l)
{   theOutObject.printError("Assignment operator of SimObjectLink not yet implemented");
    l;
    return *this;
}

inline void SimObjectLink::setTo(ostream &o) const
{   o << "Link " << linkId
      << " from " << *theSimObjectFromPtr
      << " to " << *theSimObjectToPtr;
}

/*
// Member functions SimObjectStlt

inline SimObjectStlt::SimObjectStlt(SimObject &s, int i) :
  theSimObject(s), stltId(s.getSimObjectStltNumber()), fltId(i)
{   theSimObject.addSimObjectStlt(*this);
}

inline SimObjectStlt::SimObjectStlt(const SimObjectStlt &s) :
  theSimObject(s.theSimObject), stltId(s.stltId), fltId(s.fltId)
{   theOutObject.printError("Copy constructor of SimObjectStlt not yet implemented");
    theSimObject.addSimObjectStlt(*this);
}

inline SimObjectStlt::~SimObjectStlt()
{
}

inline long SimObjectStlt::getStltId() const
{   return stltId;
}

inline SimObject &SimObjectStlt::getSimObject() const
{   return theSimObject;
}

inline long SimObjectStlt::getHashId() const
{   return long(stltId);
}

inline int SimObjectStlt::getClassId() const
{   return simObjectStltClass;
}

inline char *SimObjectStlt::getClassName() const
{   return "SimObjectStlt";
}

inline int SimObjectStlt::isEqual(const WmmObject &o) const
{   return theSimObject.getDatId() ==
      ((SimObjectStlt &)o).theSimObject.getDatId() &&
      stltId == ((SimObjectStlt &)o).stltId;
}

inline int SimObjectStlt::isSmaller(const WmmObject &o) const
{   float thisFloat = getSimObject().getFlt(fltId);
    float theOtherFloat = ((SimObjectStlt &)o).getSimObject().getFlt(fltId);
    if (thisFloat != theOtherFloat)
    {   return thisFloat < theOtherFloat;
    }
    else
    {   if (theSimObject.getDatId() !=
          ((SimObjectStlt &)o).theSimObject.getDatId())
        {   return theSimObject.getDatId() <
              ((SimObjectStlt &)o).theSimObject.getDatId();
        }
        else
        {   return stltId < ((SimObjectStlt &)o).stltId;
        }
    }
}

inline SimObjectStlt::operator long() const
{   return stltId;
}

inline SimObjectStlt &SimObjectStlt::operator=(const SimObjectStlt &l)
{   theOutObject.printError("Assignment operator of SimObjectStlt not yet implemented");
    l;
    return *this;
}

inline void SimObjectStlt::setTo(ostream &o) const
{   o << "Stlt " << stltId
      << " of " << theSimObject
      << " with fltId " << fltId << endl;
}
*/

// Member functions SimObjectTask

inline SimObjectTask::SimObjectTask(SimObject &s, double t) :
  theSimObject(s), taskId(s.getSimObjectTaskNumber()), time(t)
{   theSimObject.addSimObjectTask(*this);
    theSimObjectTaskBtree.set(*this);
}

inline SimObjectTask::SimObjectTask(const SimObjectTask &t) :
  theSimObject(t.theSimObject)
{   theOutObject.printError("Copy constructor of SimObjectTask not yet implemented");
}

inline SimObjectTask::~SimObjectTask()
{   theSimObjectTaskBtree.reset(*this);
}

inline SimObject &SimObjectTask::getSimObject() const
{   return theSimObject;
}

inline BtrConObject &SimObjectTask::getSimObjectTaskBtree()
{   return theSimObjectTaskBtree;
}

inline long SimObjectTask::getTaskId() const
{   return taskId;
}

inline long SimObjectTask::getNumber() const
{   return number;
}

inline void SimObjectTask::setNumber(long n)
{   number = n;
}

inline double SimObjectTask::getTime() const
{   return time;
}

inline long SimObjectTask::getHashId() const
{   return (long)taskId;
}

inline int SimObjectTask::getClassId() const
{   return simObjectTaskClass;
}

inline char *SimObjectTask::getClassName() const
{   return "SimObjectTask";
}

inline int SimObjectTask::getRunStatus()
{   return runStatus;
}

inline int SimObjectTask::isEqual(const WmmObject &o) const
{   return theSimObject.getDatId() ==
      ((SimObjectTask &)o).theSimObject.getDatId() &&
      taskId == ((SimObjectTask &)o).taskId;
}

inline int SimObjectTask::isSmaller(const WmmObject &o) const
{   if (time != ((SimObjectTask &)o).time)
    {   return time < ((SimObjectTask &)o).time;
    }
    else
    {   if (theSimObject.getDatId() !=
          ((SimObjectTask &)o).theSimObject.getDatId())
        {   return theSimObject.getDatId() <
              ((SimObjectTask &)o).theSimObject.getDatId();
        }
        else
        {   return taskId < ((SimObjectTask &)o).taskId;
        }
    }
}

inline SimObjectTask::operator long() const
{   return taskId;
}

inline SimObjectTask &SimObjectTask::operator=(const SimObjectTask &t)
{   theOutObject.printError("Assignment operator of SimObjectTask not yet implemented");
    t;
    return *this;
}

inline void SimObjectTask::setRunStatus(int s)
{   runStatus = s;
}

inline SimObjectTask &SimObjectTask::setStopTask()
{   setRunStatus(SIM_NOT_ACTIVE);
    return *this;
}

inline SimObjectTask &SimObjectTask::setStopSimulation()
{   setRunStatus(SIM_READY);
    return *this;
}

inline SimObjectTask &SimObjectTask::setTime(double t)
{   time = t;
    return *this;
}

inline void SimObjectTask::setTo(ostream &o) const
{   o << "Task " << taskId
      << " of " << theSimObject
      << " with next time " << time << endl;
}

#endif


