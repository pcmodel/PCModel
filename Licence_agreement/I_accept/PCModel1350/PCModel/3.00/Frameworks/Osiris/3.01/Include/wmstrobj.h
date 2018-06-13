/*------------------------------------------------------------------------*/
/*  Wmstrobj.h                                                            */
/*  Copyright (c) 1991                                                    */
/*  W.M. Mooij                                                            */
/*  Kluijskenslaan 21                                                     */
/*  2082 GT  Santpoort Zuid                                               */
/*  The Netherlands                                                       */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#ifndef __WMSTROBJ_H
#define __WMSTROBJ_H

#include "wmwmmobj.h"

// Class definitions

class StrObject : public SrtObject
{ public :
                           StrObject(char c, int l = 1);
                           StrObject(const char *cPtr = "");
                           StrObject(const StrObject &bs);
                           StrObject(const StrObject &bs1,
                                       const StrObject &bs2);
                           StrObject(const StrObject &bs,
                                       int l, int s = 0);
    virtual               ~StrObject();
    virtual int            getClassId() const;
    virtual char          *getClassName() const;
            StrObject      getField();
    virtual long           getHashId() const;
            StrObject      getLCase() const;
            StrObject      getLeft(int l) const;
            unsigned       getLength() const;
            StrObject      getLTrim() const;
            StrObject      getMid(int l, int s) const;
            StrObject      getReverse() const;
            StrObject      getRight(int l) const;
            StrObject      getRTrim() const;
            StrObject      getSpaceToUnderScore() const;
            StrObject      getTrim() const;
            StrObject      getUCase() const;
            StrObject      getUnderScoreToSpace() const;
            int            isAlphaAtBegin() const;
            int            isAlphaAtEnd() const;
            int            isCharAtEnd(char c) const;
            int            isDigitAtBegin() const;
            int            isDigitAtEnd() const;
            int            isEmpty() const;
    virtual int            isEqual(const WmmObject &o) const;
            int            isInStr(const StrObject &s);
            int            isInStrReverse(const StrObject &s);
    virtual int            isSmaller(const WmmObject& o) const;
            int            isPageBreak() const;
            int            isText() const;
                           operator char() const;
                           operator char *() const;
                           operator const char *() const;
                           operator double() const;
                           operator float() const;
                           operator int() const;
                           operator long() const;
            char          &operator [](int i);
            StrObject     &operator =(char c);
            StrObject     &operator =(const char *cPtr);
            StrObject     &operator =(const StrObject &s);
            StrObject     &operator +=(char c);
            StrObject     &operator +=(const char *cPtr);
            StrObject     &operator +=(const StrObject &s);
            StrObject      operator +(char c);
            StrObject      operator +(const char *cPtr);
            StrObject      operator +(const StrObject &s);
    friend  StrObject      operator +(char c, const StrObject &s);
    friend  StrObject      operator +(const char *cPtr, const StrObject &s);
    friend  istream       &operator >>(istream& is, StrObject &s);
            StrObject     &setDeleteCharAtBegin();
            StrObject     &setDeleteCharAtBegin(char c);
            StrObject     &setDeleteCharAtEnd();
            StrObject     &setDeleteCharAtEnd(char c);
            StrObject     &setDeleteQuotes();
            StrObject     &setEmpty();
            StrObject     &setLCase();
            StrObject     &setLeft(int l);
            StrObject     &setLine(istream &is);
            StrObject     &setLTrim();
            StrObject     &setMergeAtEnd(const StrObject &b);
            StrObject     &setMid(int l, int s);
            StrObject     &setReverse();
            StrObject     &setRight(int l);
            StrObject     &setRTrim();
            StrObject     &setSpaceToUnderScore();
            StrObject     &setCommaToTab();
            StrObject     &setSplitAtBegin(StrObject &b, const char *cPtr);
            StrObject     &setSplitAtBegin(const char *cPtr, StrObject &b);
            StrObject     &setSplitAtBegin(int i, StrObject &b);
    virtual void           setTo(ostream &os) const;
            StrObject     &setTrim();
            StrObject     &setUCase();
            StrObject     &setUnderScoreToSpace();
            const char    *getInputFileName() const;
            const char    *getOutputFileName() const;
            int            isOutputFileName() const;
            int            isInputFileName() const;
  protected:
            int            stringLength;
            char          *stringPtr;
};

// Inline member functions

inline int StrObject::getClassId() const
{   return strObjectClass;
}

inline char *StrObject::getClassName() const
{   return "StrObject";
}

inline StrObject StrObject::getLeft(int l) const
{   return StrObject(*this, l);
}

inline unsigned StrObject::getLength() const
{   return stringLength;
}

inline StrObject StrObject::getMid(int l, int s) const
{   return StrObject(*this, l, s);
}

inline StrObject StrObject::getRight(int l) const
{   return StrObject(*this, l, stringLength - l);
}

inline int StrObject::isAlphaAtBegin() const
{   return isalpha(stringPtr[0]);
}

inline int StrObject::isAlphaAtEnd() const
{   return isalpha(stringPtr[stringLength - 1]);
}

inline int StrObject::isCharAtEnd(char c) const
{   return (stringLength > 0 && stringPtr[stringLength - 1] == c) ?
            1 : 0;
}

inline int StrObject::isDigitAtBegin() const
{   return isdigit(stringPtr[0]);
}

inline int StrObject::isDigitAtEnd() const
{   return isdigit(stringPtr[stringLength - 1]);
}

inline int StrObject::isEmpty() const
{   return stringLength ? 0 : 1;
}

inline int StrObject::isEqual(const WmmObject &o) const
{   return stringLength == ((StrObject &)o).stringLength &&
           !strcmp(stringPtr, ((StrObject &)o).stringPtr);
}

inline int StrObject::isSmaller(const WmmObject& o) const
{   return strcmp(stringPtr, ((StrObject &)o).stringPtr) < 0;
}

inline int StrObject::isPageBreak() const
{   return (stringLength == 1 && stringPtr[0] == 12) ? 1 : 0;
}

inline StrObject::operator char() const
{   return stringPtr[0];
}

inline StrObject::operator char *() const
{   return stringPtr;
}

inline StrObject::operator const char *() const
{   return stringPtr;
}

inline StrObject::operator double() const
{   return atof(stringPtr);
}

inline StrObject::operator float() const
{   return atof(stringPtr);
}

inline StrObject::operator int() const
{   return atoi(stringPtr);
}

inline StrObject::operator long() const
{   return atol(stringPtr);
}

inline char &StrObject::operator [](int i)
{   return stringPtr[i];
}

inline StrObject &StrObject::operator=(char c)
{   *this = (StrObject)c;
    return *this;
}

inline StrObject &StrObject::operator=(const char *cPtr)
{   *this = (StrObject)cPtr;
    return *this;
}

inline StrObject StrObject::operator +(const StrObject &s)
{   return StrObject(*this, s);
}

inline StrObject &StrObject::operator +=(char c)
{   *this = *this + c;
    return *this;
}

inline StrObject &StrObject::operator +=(const char *cPtr)
{   *this = *this + cPtr;
    return *this;
}

inline StrObject &StrObject::operator +=(const StrObject &s)
{   *this = *this + s;
    return *this;
}

inline istream &operator >>(istream &is, StrObject &s)
{   is >> theOutObject.getBuffer();
    s = theOutObject.getBuffer();
    return is;
}

inline void StrObject::setTo(ostream &o) const
{   o << stringPtr;
}

inline StrObject &StrObject::setRight(int l)
{   *this = this->getRight(l);
    return *this;
}

inline StrObject &StrObject::setDeleteCharAtBegin()
{   setRight(stringLength - 1);
    return *this;
}

inline StrObject &StrObject::setDeleteCharAtBegin(char c)
{   if (stringPtr[0] == c)
    {   setRight(stringLength - 1);
    }
    return *this;
}

inline StrObject &StrObject::setLeft(int l)
{   *this = this->getLeft(l);
    return *this;
}

inline StrObject &StrObject::setDeleteCharAtEnd()
{   setLeft(stringLength - 1);
    return *this;
}

inline StrObject &StrObject::setDeleteCharAtEnd(char c)
{   if (stringPtr[stringLength - 1] == c)
    {   setLeft(stringLength - 1);
    }
    return *this;
}

inline StrObject &StrObject::setDeleteQuotes()
{   setDeleteCharAtBegin('\"');
    setDeleteCharAtEnd('\"');
    return *this;
}

inline StrObject &StrObject::setEmpty()
{   return *this = "";
}

inline StrObject &StrObject::setLine(istream &is)
{   is.getline(theOutObject.getBuffer(), theOutObject.getBufferSize());
    *this = theOutObject.getBuffer();
    return *this;
}

inline StrObject &StrObject::setLTrim()
{   *this = this->getLTrim();
    return *this;
}

inline StrObject &StrObject::setMergeAtEnd(const StrObject &s)
{   if (stringLength == 0)
    {   *this = s;
    }
    else
    {   if (isCharAtEnd('-'))
        {   if (isalpha(stringPtr[stringLength - 2]))
            {   setDeleteCharAtEnd();
            }
        }
        else
        {   *this += " ";
        }
        *this += s;
    }
    return *this;
}

inline StrObject &StrObject::setMid(int l, int s)
{   *this = this->getMid(l, s);
    return *this;
}

inline StrObject &StrObject::setRTrim()
{   *this = this->getRTrim();
    return *this;
}

inline StrObject &StrObject::setSplitAtBegin(StrObject &b,
                                                 const char *cPtr)
{   if (strlen(cPtr) == 0)
    {   b = *this;
        *this = "";
    }
    else
    {   int i = isInStr(cPtr);
        if (i)
        {   b = getLeft(i - 1);
            setRight(stringLength - i + 1);
        }
        else
        {   b = "";
        }
    }
    return *this;
}

inline StrObject &StrObject::setSplitAtBegin(const char *cPtr,
                                                 StrObject &b)
{   if (strlen(cPtr) == 0)
    {   b = *this;
        *this = "";
    }
    else
    {   int i = isInStr(cPtr);
        if (i)
        {   b = getRight(stringLength - i + 1);
            setLeft(i - 1);
        }
        else
        {   b = "";
        }
    }
    return *this;
}

inline StrObject &StrObject::setSplitAtBegin(int i, StrObject &b)
{   if (i <= stringLength)
    {   b = getLeft(i);
        setRight(stringLength - i);
    }
    else
    {   b = "";
    }
    return *this;
}

inline StrObject &StrObject::setTrim()
{   *this = this->getTrim();
    return *this;
}

#endif


