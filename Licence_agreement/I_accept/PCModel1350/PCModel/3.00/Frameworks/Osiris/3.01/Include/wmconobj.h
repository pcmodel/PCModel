/*------------------------------------------------------------------------*/
/*  Wmconobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMCONOBJ_H
#define __WMCONOBJ_H

#include "wmwmmobj.h"

class WmmConObject;
class WmmConObjectLoop;
class SngConObject;
class SngConObjectLoop;
class SngConObjectLink;
class DblConObject;
class DblConObjectLoop;
class DblConObjectLink;
class VecConObject;
class VecConObjectLoop;
class HshConObject;
class HshConObjectLoop;
class BtrConObject;
class BtrConObjectLink;
class BtrConObjectLoop;

class WmmConObject : public WmmObject
{ public:
                           WmmConObject() : number(0), owner(1) {}
    virtual               ~WmmConObject() {}
    virtual WmmObject     &get() const = 0;
    virtual WmmObject     &get(WmmObject &o) const = 0;
    virtual WmmObject     &get(long l) const = 0;
    virtual int            getClassId() const = 0;
    virtual char          *getClassName() const = 0;
    virtual LnkObject     &getLnk(const WmmObject &o) const;
    virtual LnkSrtObject  &getLnkSrt(const WmmObject &o) const;
    virtual WmmObject     &getFirst() const = 0;
    virtual WmmObject     &getFirst(WmmObject &o) const = 0;
    virtual long           getHashId() const {return 0;}
    virtual long           getIndex(const WmmObject &o) const = 0;
    virtual WmmObject     &getLast() const = 0;
    virtual WmmObject     &getLast(WmmObject &o) const = 0;
    virtual long           getNumber() const {return number;}
    virtual long           getSize() const = 0;
    virtual int            isConObject() const {return 1;}
    virtual int            isEmpty() const {return number == 0;}
    virtual int            isEqual(const WmmObject &o) const;
    virtual int            isMember(WmmObject &o) const {return get(o) != getNullObject();}
    virtual int            isOwner() const {return owner;}
    virtual WmmObject     &operator [](long l) const = 0;
    virtual WmmObject     &reset() = 0;
    virtual WmmObject     &reset(WmmObject &o) = 0;
    virtual WmmObject     &reset(long l) = 0;
    virtual WmmConObject  &resetAll() = 0;
    virtual WmmObject     &resetFirst() = 0;
    virtual WmmObject     &resetFirst(WmmObject &o) = 0;
    virtual WmmObject     &resetLast() = 0;
    virtual WmmObject     &resetLast(WmmObject &o) = 0;
    virtual WmmConObject  &set(WmmObject &o) = 0;
    virtual WmmConObject  &set(WmmObject &o, WmmObject &p) = 0;
    virtual WmmConObject  &set(WmmObject &o, long &l) = 0;
    virtual void           setDeleteLoop(WmmConObjectLoop &l);
    virtual WmmObject     &setLnk(WmmObject &o);
    virtual WmmObject     &setLnkSrt(WmmObject &o);
    virtual WmmConObject  &setFirst(WmmObject &o) = 0;
    virtual WmmConObject  &setFirst(WmmObject &o, WmmObject &p) = 0;
    virtual WmmConObject  &setLast(WmmObject &o) = 0;
    virtual WmmConObject  &setLast(WmmObject &o, WmmObject &p) = 0;
    virtual WmmConObject  &setSrt(WmmObject &o) = 0;
    virtual WmmConObject  &setSrt(WmmObject &o, WmmObject &p) = 0;
    virtual WmmConObjectLoop &setNewLoop() const = 0;
    virtual void           setTo(ostream &o) const;
    virtual WmmConObject  &setOwner(int i) {owner = i; return *this;}
  protected:
            long           number;
            int            owner;
    friend  class          WmmConObjectLoop;
};

class WmmConObjectLoop : public WmmObject
{ public:
    virtual               ~WmmConObjectLoop() {}
    virtual WmmObject     &get() = 0;
    virtual int            getClassId() const {return 0;}
    virtual char          *getClassName() const {return "ConObjectLoop";}
    virtual long           getHashId() const {return 0;}
    virtual int            isEqual(const WmmObject &o) const {return 0;}
    virtual                operator int() = 0;
    virtual WmmObject     &operator ++() = 0;
    virtual WmmObject     &operator ++(int) = 0;
    virtual WmmConObjectLoop &reset() = 0;
    virtual void           setTo(ostream &o) const {o;}
};

class SngObjectLink
{ public:
    SngObjectLink(WmmObject *o, SngObjectLink *p = 0) {theObjectPtr = o; thePrevObjectPtr = p;}
    WmmObject* getObjectPtr() const {return theObjectPtr;}
    SngObjectLink* getPrevObjectPtr() const {return thePrevObjectPtr;}
  private:
    WmmObject *theObjectPtr;
    SngObjectLink *thePrevObjectPtr;
    friend class SngConObject;
    friend class SngConObjectLoop;
};

class SngConObject : public WmmConObject
{ public:
                           SngConObject() : last(theNullPtr, &first), first(theNullPtr, &first) {}
    virtual               ~SngConObject() {resetAll();}
    virtual WmmObject     &get() const {return getLast();}
    virtual WmmObject     &get(WmmObject &o) const {return getLast(o);}
    virtual WmmObject     &get(long l) const {return getNullObject();}
    virtual int            getClassId() const {return sngConObjectClass;}
    virtual char          *getClassName() const {return "SngConObject";}
    virtual WmmObject     &getFirst() const {return getNullObject();}
    virtual WmmObject     &getFirst(WmmObject &o) const {return getNullObject();}
    virtual long           getIndex(const WmmObject &o) const {return 0;}
    virtual WmmObject     &getLast() const;
    virtual WmmObject     &getLast(WmmObject &o) const;
    virtual long           getSize() const {return 1;}
    virtual WmmObject     &operator [](long) const {return getNullObject();}
    virtual WmmObject     &reset() {return resetLast();}
    virtual WmmObject     &reset(WmmObject &o) {return resetLast(o);}
    virtual WmmObject     &reset(long l) {return getNullObject();}
    virtual WmmConObject  &resetAll();
    virtual WmmObject     &resetFirst() {return getNullObject();}
    virtual WmmObject     &resetFirst(WmmObject &o) {return getNullObject();}
    virtual WmmObject     &resetLast();
    virtual WmmObject     &resetLast(WmmObject &o);
    virtual WmmConObject  &set(WmmObject &o) {setLast(o); return *this;}
    virtual WmmConObject  &set(WmmObject &o, WmmObject &p) {setLast(o, p); return *this;}
    virtual WmmConObject  &set(WmmObject &o, long &l) {theOutObject.printError("SngConObject::set(WmmObject &o, long &l) not implemented"); return *this;}
    virtual WmmConObject  &setFirst(WmmObject &o) {theOutObject.printError("SngConObject::setFirst(WmmObject &o) not implemented"); return *this;}
    virtual WmmConObject  &setFirst(WmmObject &o, WmmObject &p) {if (getLast(p) == getNullObject()) setLast(o); return *this;}
    virtual WmmConObject  &setLast(WmmObject &o);
    virtual WmmConObject  &setLast(WmmObject &o, WmmObject &p) {if (getLast(p) == getNullObject()) setLast(o); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o) {theOutObject.printError("SngConObject::setSrt(WmmObject &o) not implemented"); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) setSrt(o); return *this;}
    virtual WmmConObjectLoop &setNewLoop() const;
  private:
            SngObjectLink  first;
            SngObjectLink  last;
    friend  class          SngConObjectLoop;
};

class SngConObjectLoop : public WmmConObjectLoop
{ public:
                           SngConObjectLoop(const SngConObject &l);
    virtual               ~SngConObjectLoop();
    virtual WmmObject     &get();
    virtual                operator int();
    virtual WmmObject     &operator ++ ();
    virtual WmmObject     &operator ++ (int);
    virtual WmmConObjectLoop &reset();
  private:
            SngObjectLink *firstPtr;
            SngObjectLink *getPtr;
};

class DblObjectLink
{ public:
    DblObjectLink(WmmObject *o, DblObjectLink *p = 0, DblObjectLink *n = 0) {thePrevObjectPtr = p; theObjectPtr = o; theNextObjectPtr = n;}
    DblObjectLink* getPrevObjectPtr() const {return thePrevObjectPtr;}
    WmmObject* getObjectPtr() const {return theObjectPtr;}
    DblObjectLink* getNextObjectPtr() const {return theNextObjectPtr;}
  private:
    DblObjectLink *thePrevObjectPtr;
    WmmObject *theObjectPtr;
    DblObjectLink *theNextObjectPtr;
    friend class DblConObject;
    friend class DblConObjectLoop;
};

class DblConObject : public WmmConObject
{ public:
                           DblConObject() : last(theNullPtr, &first, &last), first(theNullPtr, &first, &last) {}
    virtual               ~DblConObject() {resetAll();}
    virtual WmmObject     &get() const {return getFirst();}
    virtual WmmObject     &get(WmmObject &o) const {return getFirst(o);}
    virtual WmmObject     &get(long l) const {return getNullObject();}
    virtual int            getClassId() const {return dblConObjectClass;}
    virtual char          *getClassName() const {return "DblConObject";}
    virtual WmmObject     &getFirst() const;
    virtual WmmObject     &getFirst(WmmObject &o) const;
    virtual long           getIndex(const WmmObject &o) const {return 0;}
    virtual WmmObject     &getLast() const;
    virtual WmmObject     &getLast(WmmObject &o) const;
    virtual long           getSize() const {return 0;}
    virtual WmmObject     &operator [](long) const {return getNullObject();}
    virtual WmmObject     &reset() {return resetFirst();}
    virtual WmmObject     &reset(WmmObject &o) {return resetFirst(o);}
    virtual WmmObject     &reset(long l) {return getNullObject();}
    virtual WmmConObject  &resetAll();
    virtual WmmObject     &resetFirst();
    virtual WmmObject     &resetFirst(WmmObject &o);
    virtual WmmObject     &resetLast();
    virtual WmmObject     &resetLast(WmmObject &o);
    virtual WmmConObject  &set(WmmObject &o) {setLast(o); return *this;}
    virtual WmmConObject  &set(WmmObject &o, WmmObject &p) {setFirst(o, p); return *this;}
    virtual WmmConObject  &set(WmmObject &o, long &l) {theOutObject.printError("DblConObject::set(WmmObject &o, long &l) not implemented"); return *this;}
    virtual WmmConObject  &setFirst(WmmObject &o);
    virtual WmmConObject  &setFirst(WmmObject &o, WmmObject &p) {if (getFirst(p) == getNullObject()) setFirst(o); return *this;}
    virtual WmmConObject  &setLast(WmmObject &o);
    virtual WmmConObject  &setLast(WmmObject &o, WmmObject &p) {if (getLast(p) == getNullObject()) setLast(o); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o) {theOutObject.printError("DblConObject::setSrt(WmmObject &o) not implemented"); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) setSrt(o); return *this;}
    virtual WmmConObjectLoop &setNewLoop() const;
  private:
            DblObjectLink  first;
            DblObjectLink  last;
    friend  class          DblConObjectLoop;
};

class DblConObjectLoop : public WmmConObjectLoop
{ public:
                           DblConObjectLoop(const DblConObject &l);
    virtual               ~DblConObjectLoop();
    virtual WmmObject     &get();
    virtual                operator int();
    virtual WmmObject     &operator ++();
    virtual WmmObject     &operator ++(int);
    virtual WmmConObjectLoop &reset();
  private:
            DblObjectLink *firstPtr;
            DblObjectLink *getPtr;
};

class VecConObject: public WmmConObject
{ public:
                           VecConObject(long s = 1, long d = 1);
    virtual               ~VecConObject();
    virtual WmmObject     &get() const {return getLast();}
    virtual WmmObject     &get(WmmObject &o) const {return getLast(o);}
    virtual WmmObject     &get(long l) const {return *(first[(size_t)l]);}
    virtual int            getClassId() const {return vecConObjectClass;}
    virtual char          *getClassName() const {return "VecConObject";}
    virtual WmmObject     &getFirst() const {return get(0);}
    virtual WmmObject     &getFirst(WmmObject &o) const;
    virtual long           getIndex(const WmmObject &o) const {return step;}
    virtual WmmObject     &getLast() const {return get(last);}
    virtual WmmObject     &getLast(WmmObject &o) const;
    virtual long           getSize() const {return size;}
    virtual WmmObject     &operator [](long l) const {return *(first[(size_t)l]);}
    virtual WmmObject     &reset() {return resetLast();}
    virtual WmmObject     &reset(WmmObject &o) {return resetLast(o);}
    virtual WmmObject     &reset(long l);
    virtual WmmConObject  &resetAll();
    virtual WmmObject     &resetFirst() {return reset(0);}
    virtual WmmObject     &resetFirst(WmmObject &o);
    virtual WmmObject     &resetLast() {return reset(last);}
    virtual WmmObject     &resetLast(WmmObject &o);
            WmmConObject  &resetMemory(size_t s);
    virtual WmmConObject  &set(WmmObject &o) {setLast(o); return *this;}
    virtual WmmConObject  &set(WmmObject &o, WmmObject &p) {setLast(o, p); return *this;}
    virtual WmmConObject  &set(WmmObject &o, long &l);
    virtual WmmConObject  &setFirst(WmmObject &o);
    virtual WmmConObject  &setFirst(WmmObject &o, WmmObject &p) {if (getFirst(p) == getNullObject()) setFirst(o); return *this;}
    virtual WmmConObject  &setLast(WmmObject &o);
    virtual WmmConObject  &setLast(WmmObject &o, WmmObject &p) {if (getLast(p) == getNullObject()) setLast(o); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o) {theOutObject.printError("VecConObject::setSrt(WmmObject &o) not implemented"); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) setSrt(o); return *this;}
    virtual WmmConObjectLoop &setNewLoop() const;
  private:
            WmmObject    **first;
            size_t         last;
            size_t         size;
            size_t         step;
    friend  class          VecConObjectLoop;
};

class VecConObjectLoop : public WmmConObjectLoop
{ public:
                           VecConObjectLoop(WmmObject **f, size_t s);
    virtual               ~VecConObjectLoop();
    virtual WmmObject     &get();
    virtual                operator int();
    virtual WmmObject     &operator ++();
    virtual WmmObject     &operator ++(int);
    virtual WmmConObjectLoop &reset();
  private:
            WmmObject    **firstPtr;
            size_t         sizePtr;
            size_t         getPtr;
};

class HshConObject: public WmmConObject
{ public:
                           HshConObject(long s = 111) : first(s) {}
    virtual               ~HshConObject() {resetAll();}
    virtual WmmObject     &get() const {return getNullObject();}
    virtual WmmObject     &get(WmmObject &o) const;
    virtual WmmObject     &get(long l) const {return getNullObject();}
    virtual int            getClassId() const {return hshConObjectClass;}
    virtual char          *getClassName() const {return "HshConObject";}
    virtual WmmObject     &getFirst() const {return getNullObject();}
    virtual WmmObject     &getFirst(WmmObject &o) const {return getFirst(o);}
    virtual long           getIndex(const WmmObject &o) const {return 0;}
    virtual WmmObject     &getLast() const {return getNullObject();}
    virtual WmmObject     &getLast(WmmObject &o) const {return getNullObject();}
    virtual long           getSize() const {return first.getSize();}
    virtual WmmObject     &operator [](long l) const {return getNullObject();}
    virtual WmmObject     &reset() {return getNullObject();}
    virtual WmmObject     &reset(WmmObject &o);
    virtual WmmObject     &reset(long l) {return getNullObject();}
    virtual WmmConObject  &resetAll();
    virtual WmmObject     &resetFirst() {return getNullObject();}
    virtual WmmObject     &resetFirst(WmmObject &o) {return getNullObject();}
    virtual WmmObject     &resetLast() {return getNullObject();}
    virtual WmmObject     &resetLast(WmmObject &o) {return getNullObject();}
    virtual WmmConObject  &set(WmmObject &o);
    virtual WmmConObject  &set(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) set(o); return *this;}
    virtual WmmConObject  &set(WmmObject &o, long &l) {return *this;}
    virtual WmmConObject  &setFirst(WmmObject &o) {return *this;}
    virtual WmmConObject  &setFirst(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) set(o); return *this;}
    virtual WmmConObject  &setLast(WmmObject &o) {return *this;}
    virtual WmmConObject  &setLast(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) set(o); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o) {theOutObject.printError("HshConObject::setSrt(WmmObject &o) not implemented"); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) setSrt(o); return *this;}
    virtual WmmConObjectLoop &setNewLoop() const;
    virtual WmmConObject  &setOwner(int i);
  private:
            VecConObject   first;
    friend  class          HshConObjectLoop;
};

class HshConObjectLoop : public WmmConObjectLoop
{ public:
                           HshConObjectLoop(const HshConObject &h);
                          ~HshConObjectLoop();
    virtual WmmObject     &get();
    virtual                operator int();
    virtual WmmObject     &operator ++();
    virtual WmmObject     &operator ++(int);
    virtual WmmConObjectLoop &reset();
  private:
            WmmConObjectLoop *vecLoopPtr;
            WmmConObjectLoop *sngLoopPtr;
};

#ifdef __WMM_BOR

class BtrObjectLink
{ public:
                           BtrObjectLink(int i, BtrObjectLink *p, BtrConObject *b = 0);
                          ~BtrObjectLink();
            void           add(SrtObject *s, int i);
            void           addElt(int i, SrtObject *s, BtrObjectLink *n);
            void           appendFrom(BtrObjectLink *n, int i, int j);
            void           balanceWith(BtrObjectLink *n, int i);
            void           balanceWithLeft(BtrObjectLink *n, int i);
            void           balanceWithRight(BtrObjectLink *n, int i);
            void           decrNofKeys(BtrObjectLink *n);
            long           findIndex(SrtObject *s) const;
            long           findIndexRev(const BtrObjectLink *n) const;
            WmmObject     &get(SrtObject *s, BtrObjectLink **n, int *i);
            void           incrNofKeys(BtrObjectLink *n);
            int            indexOf(const BtrObjectLink *n) const;
            void           isFull(BtrObjectLink *n);
            void           isLow(BtrObjectLink *n);
            void           mergeWithRight(BtrObjectLink *n, int i);
            long           nofKeys() const;
            WmmObject     &operator [](long l) const;
            void           pushLeft(int i, BtrObjectLink *n, int j);
            void           pushRight(int i, BtrObjectLink *n, int j);
            void           remove(int i);
            void           removeItem(int i);
            void           setTo(ostream &o) const;
            void           shiftLeft(int i);
            void           split();
            void           splitWith(BtrObjectLink *n, int i);

            long           decNofKeys(int i, long n = 1) {return binNumberPtr[i] -= n;}
            BtrObjectLink *firstBtrObjectLinkOfObj() {return theNextObjectPtr ? theNextObjectPtr[0]->firstBtrObjectLinkOfObj() : this;}
            long           incNofKeys(int i, long n = 1) {return binNumberPtr[i] += n;}
            int            isAlmostFull() const {return theNextObjectPtr ? last >= elmMax - 1 : last >= objMax - 1;}
            int            isFull() const {return theNextObjectPtr ? last == elmMax : last == objMax;}
            int            isLow() const {return theNextObjectPtr ? last < elmLow : last < objLow;}
            BtrObjectLink *lastBtrObjectLinkOfObj() {return theNextObjectPtr ? theNextObjectPtr[last]->lastBtrObjectLinkOfObj() : this;}
            int            maxPsize() const {return theNextObjectPtr ? elmMax : objMax + 1;}
            int            Psize() const {return theNextObjectPtr ? last : last + 1;}
            int            Vsize() const {return Psize() + 1;}
  protected :
    static  int            elmLow;
    static  int            elmMax;
    static  int            objLow;
    static  int            objMax;
            int            isElm;
            int            last;
            BtrConObject  *theBtrConObject;
            BtrObjectLink *thePrevObjectPtr;
            SrtObject    **theObjectPtr;
            BtrObjectLink **theNextObjectPtr;
            long          *binNumberPtr;
    friend  ostream       &operator <<(ostream &o, const BtrObjectLink &n);
    friend  class          BtrConObject;
};

inline ostream &operator <<(ostream &o, const BtrObjectLink &n) {n.setTo(o); return o;}

class BtrConObject : public WmmConObject
{ public:
                           BtrConObject(int s = 3);
    virtual               ~BtrConObject() {resetAll();}
    virtual WmmObject     &get() const {return getNullObject();}
    virtual WmmObject     &get(WmmObject &o) const;
    virtual WmmObject     &get(long l) const {return (*first)[l];}
    virtual int            getClassId() const {return binConObjectClass;}
    virtual char          *getClassName() const {return "BtrConObjectClass";}
    virtual WmmObject     &getFirst() const {return getNullObject();}
    virtual WmmObject     &getFirst(WmmObject &o) const {o; return getNullObject();}
    virtual long           getIndex(const WmmObject &o) const;
    virtual WmmObject     &getLast() const {return getNullObject();}
    virtual WmmObject     &getLast(WmmObject &o) const {o; return getNullObject();}
    virtual long           getSize() const {return size;}
//    virtual int            isMember(WmmObject &o) const;
    virtual WmmObject     &operator [](long l) const {return (*first)[l];}
    virtual WmmObject     &reset() {return getNullObject();}
    virtual WmmObject     &reset(WmmObject &o);
    virtual WmmObject     &reset(long l) {return reset((*first)[l]);}
    virtual WmmConObject  &resetAll();
    virtual WmmObject     &resetFirst() {return getNullObject();}
    virtual WmmObject     &resetFirst(WmmObject &o) {o; return getNullObject();}
    virtual WmmObject     &resetLast() {return getNullObject();}
    virtual WmmObject     &resetLast(WmmObject &o) {o; return getNullObject();}
    virtual WmmConObject  &set(WmmObject &o);
    virtual WmmConObject  &set(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) set(o); return *this;}
    virtual WmmConObject  &set(WmmObject &o, long &l);
    virtual WmmConObject  &setFirst(WmmObject &o) {o; return *this;}
    virtual WmmConObject  &setFirst(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) set(o); return *this;}
    virtual WmmConObject  &setLast(WmmObject &o) {o; return *this;}
    virtual WmmConObject  &setLast(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) set(o); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o) {theOutObject.printError("BtrConObject::setSrt(WmmObject &o) not implemented"); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) setSrt(o); return *this;}
    virtual WmmConObjectLoop &setNewLoop() const;
    virtual void           setTo(ostream &o) const;
  private :
            BtrObjectLink *first;
            int            size;
    friend  class          BtrObjectLink;
    friend  class          BtrConObjectLoop;
};

class BtrConObjectLoop : public WmmConObjectLoop
{ public:
                           BtrConObjectLoop(const BtrConObject &b);
    virtual               ~BtrConObjectLoop();
    virtual WmmObject     &get();
    virtual                operator int();
    virtual WmmObject     &operator ++();
    virtual WmmObject     &operator ++(int);
    virtual WmmConObjectLoop &reset();
  private:
    const   BtrConObject  &firstPtr;
            long           getPtr;
};

#else

class BtrObjectLink
{ public:
    BtrObjectLink(WmmObject *o, BtrObjectLink *p = 0, BtrObjectLink *n = 0) {thePrevObjectPtr = p; theObjectPtr = o; theNextObjectPtr = n;}
    BtrObjectLink* getPrevObjectPtr() const {return thePrevObjectPtr;}
    WmmObject* getObjectPtr() const {return theObjectPtr;}
    BtrObjectLink* getNextObjectPtr() const {return theNextObjectPtr;}
  private:
    BtrObjectLink *thePrevObjectPtr;
    WmmObject *theObjectPtr;
    BtrObjectLink *theNextObjectPtr;
    friend class BtrConObject;
    friend class BtrConObjectLoop;
};

class BtrConObject : public WmmConObject
{ public:
                           BtrConObject() : last(theNullPtr, &first, &last), first(theNullPtr, &first, &last) {}
    virtual               ~BtrConObject() {resetAll();}
    virtual WmmObject     &get() const {return getFirst();}
    virtual WmmObject     &get(WmmObject &o) const {return getFirst(o);}
    virtual WmmObject     &get(long l) const {return getNullObject();}
    virtual int            getClassId() const {return BINConObjectClass;}
    virtual char          *getClassName() const {return "BINConObject";}
    virtual WmmObject     &getFirst() const;
    virtual WmmObject     &getFirst(WmmObject &o) const;
    virtual long           getIndex(const WmmObject &o) const {return 0;}
    virtual WmmObject     &getLast() const;
    virtual WmmObject     &getLast(WmmObject &o) const;
    virtual long           getSize() const {return 0;}
    virtual WmmObject     &operator [](long) const; // {return getFirst();}
    virtual WmmObject     &reset() {return resetFirst();}
    virtual WmmObject     &reset(WmmObject &o) {return resetFirst(o);}
    virtual WmmObject     &reset(long l) {return getNullObject();}
    virtual WmmConObject  &resetAll();
    virtual WmmObject     &resetFirst();
    virtual WmmObject     &resetFirst(WmmObject &o);
    virtual WmmObject     &resetLast();
    virtual WmmObject     &resetLast(WmmObject &o);
    virtual WmmConObject  &set(WmmObject &o) {setLast(o); return *this;}
    virtual WmmConObject  &set(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) set(o); return *this;}
    virtual WmmConObject  &set(WmmObject &o, long &l) {return *this;}
    virtual WmmConObject  &setFirst(WmmObject &o);
    virtual WmmConObject  &setFirst(WmmObject &o, WmmObject &p) {if (getFirst(p) == getNullObject()) setFirst(o); return *this;}
    virtual WmmConObject  &setLast(WmmObject &o);
    virtual WmmConObject  &setLast(WmmObject &o, WmmObject &p) {if (getLast(p) == getNullObject()) setLast(o); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o) {theOutObject.printError("BtrConObject::setSrt(WmmObject &o) not implemented"); return *this;}
    virtual WmmConObject  &setSrt(WmmObject &o, WmmObject &p) {if (get(p) == getNullObject()) setSrt(o); return *this;}
    virtual WmmConObjectLoop &setNewLoop() const;
  private:
            BtrObjectLink  first;
            BtrObjectLink  last;
    friend  class          BtrConObjectLoop;
};

class BtrConObjectLoop : public WmmConObjectLoop
{ public:
                           BtrConObjectLoop(const BtrConObject &l);
    virtual               ~BtrConObjectLoop();
    virtual WmmObject     &get();
    virtual                operator int();
    virtual WmmObject     &operator ++();
    virtual WmmObject     &operator ++(int);
    virtual WmmConObjectLoop &reset();
  private:
            BtrObjectLink *firstPtr;
            BtrObjectLink *getPtr;
};

#endif

#endif


