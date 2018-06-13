/*------------------------------------------------------------------------*/
/*  Wmwmmobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMWMMOBJ_H
#define __WMWMMOBJ_H

#include "wmoutobj.h"

enum
{   nulObjectClass,
    lnkObjectClass,
    lnkSrtObjectClass,
    numObjectClass,
    intObjectClass,
    intArrayObjectClass,
    fltObjectClass,
    fltArrayObjectClass,
    basicDateClass,
    basicTimeClass,
    basicMomentClass,
    strObjectClass,
    sumObjectClass,
    datObjectClass,
    simObjectClass,
    simObjectLinkClass,
    simObjectStltClass,
    simObjectTaskClass,
    conditionItemLinkClass,
    resourceItemLinkClass,
    individualLinkClass,
    sngConObjectClass,
    dblConObjectClass,
    hshConObjectClass,
    vecConObjectClass,
    binConObjectClass,
    BINConObjectClass,
};

class WmmObject
{ public:
    virtual           ~WmmObject() {}
    virtual int        getClassId() const = 0;
    virtual char      *getClassName() const = 0;
    virtual long       getHashId() const = 0;
    static  WmmObject &getNullObject() {return *theNullPtr;}
    virtual int        isConObject() const {return 0;}
    virtual int        isLnkObject() const {return 0;}
    virtual int        isEqual(const WmmObject &o) const = 0;
    virtual int        isSrtObject() const {return 0;}
    virtual void       setTo(ostream &o) const = 0;
    static  WmmObject *theNullPtr;
    friend  ostream   &operator <<(ostream &os, const WmmObject &ob);
};

inline ostream &operator <<(ostream &os, const WmmObject &ob) {ob.setTo(os); return os;}
inline int operator ==(const WmmObject &o1, const WmmObject &o2) {return o1.getClassId() == o2.getClassId() && o1.isEqual(o2);}
inline int operator !=(const WmmObject &o1, const WmmObject &o2) {return !(o1 == o2);}

class NulObject : public WmmObject
{ public:
    virtual int getClassId() const {return nulObjectClass;}
    virtual char *getClassName() const {return "NulObject";}
    virtual long getHashId() const {return 0;}
    virtual int isEqual(const WmmObject &) const {return 1;}
    void operator delete(void *) {theOutObject.printError("Objects of class NulObject cannot be deleted");}
    virtual void setTo(ostream &o) const {o << getClassName();}
};

class SrtObject : public WmmObject
{ public:
    virtual int getClassId() const = 0;
    virtual char *getClassName() const = 0;
    virtual long getHashId() const = 0;
    virtual int isEqual(const WmmObject &) const = 0;
    virtual int isSmaller(const WmmObject &) const = 0;
    virtual int isSrtObject() const {return 1;}
    virtual void setTo(ostream&) const = 0;
};

inline int operator <(const SrtObject &s1, const SrtObject &s2) {return s1.getClassId() == s2.getClassId() && s1.isSmaller(s2);}
inline int operator >(const SrtObject &s1, const SrtObject &s2) {return !(s1 < s2) && s1 != s2;}
inline int operator >=(const SrtObject &s1, const SrtObject &s2) {return !(s1 < s2);}
inline int operator <=(const SrtObject &s1, const SrtObject &s2) {return s1 < s2 || s1 == s2;}

class LnkObject : public WmmObject
{ public:
    LnkObject(WmmObject &k, WmmObject &v) : theKey(k), theValue(v), owner(1) {}
    LnkObject(const LnkObject &d) : theKey(d.theKey), theValue(d.theValue), owner(d.owner) {}
    virtual ~LnkObject() {if(owner) {if(theKey != getNullObject()) delete &theKey; if(theValue != getNullObject()) delete &theValue;}}
    virtual int getClassId() const {return lnkObjectClass;}
    virtual char *getClassName() const {return "LnkObject";}
    virtual long getHashId() const {return theKey.getHashId();}
    virtual int isLnkObject() const {return 1;}
    virtual int isEqual(const WmmObject &o) const {return theKey == ((LnkObject &)o).theKey;}
    virtual int isOwner() {return owner;}
    virtual void setTo(ostream &o) const {o << getClassName() << " {" << theKey << ", " << theValue << "}";}
    virtual void setOwner(int i) {owner = i;}

    WmmObject &getKey() const {return theKey;}
    WmmObject &getValue() const {return theValue;}
  private:
    WmmObject &theKey;
    WmmObject &theValue;
    int owner;
};

class LnkSrtObject : public SrtObject
{ public:
    LnkSrtObject(SrtObject &k, WmmObject &v) : theKey(k), theValue(v), owner(1) {}
    LnkSrtObject(const LnkSrtObject &d) : theKey(d.theKey), theValue(d.theValue), owner(d.owner) {}
    virtual ~LnkSrtObject() {if(owner) {if(theKey != getNullObject()) delete &theKey; if(theValue != getNullObject()) delete &theValue;}}
    virtual int getClassId() const {return lnkSrtObjectClass;}
    virtual char *getClassName() const {return "LnkSrtObject";}
    virtual long getHashId() const {return theKey.getHashId();}
    virtual int isLnkObject() const {return 1;}
    virtual int isEqual(const WmmObject &o) const {return theKey == ((LnkSrtObject &)o).theKey;}
    virtual int isSmaller(const WmmObject &o) const {return theKey < ((LnkSrtObject &)o).theKey;}
    virtual int isOwner() {return owner;}
    virtual void setTo(ostream &o) const {o << getClassName() << " {" << theKey << ", " << theValue << "}";}
    virtual void setOwner(int i) {owner = i;}

    SrtObject &getKey() const {return theKey;}
    WmmObject &getValue() const {return theValue;}
  private:
    SrtObject &theKey;
    WmmObject &theValue;
    int owner;
};

#endif


