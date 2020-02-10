/*------------------------------------------------------------------------*/
/*  File Pclake in Osiris 2.00                                            */
/*  Framework Osiris, version 3.01                                        */
/*  Copyright (c) 1993, 2002                                              */
/*  W.M. Mooij                                                            */
/*  Netherlands Institute of Ecology                                      */
/*  Centre for Limnology                                                  */
/*  Rijksstraatweg 6                                                      */
/*  3631 AC  Nieuwersluis                                                 */
/*  The Netherlands                                                       */
/*  tel: (31)-(0)294 239352                                               */
/*  fax: (31)-(0)294 232224                                               */
/*  e-mail: mooij@cl.nioo.knaw.nl                                         */
/*  www: www.nioo.knaw.nl                                                 */
/*  ftp: ftp.nioo.knaw.nl                                                 */
/*  All Rights Reserved                                                   */
/*------------------------------------------------------------------------*/

#include "osbioobj.h"
// Osiris header file

int main(int argc, char *argv[])
// C++ main function
{
#ifdef __GNUG__
    cout << "Compiled with GNUG" << endl;
#endif
#ifdef __BORLANDC__
    cout << "Compiled with BORLAND C++" << endl;
#endif
    if (argc!= 3) theOutObject.printError("Syntax OSIRIS <excel_filename analyser_filename>");
    // check if there are three arguments provided at the osiris command line: 1) path and file name of the executable, 2) excel filename, 3) analyser inputfilename
    else if (StrObject(argv[1]).getLCase().isInStr("p") && StrObject(argv[1]).isInStr("61316")) {void PCLake61316Define(); return Analyser::main(PCLake61316Define, argv[2]);}
    // call version 6.13.16 of pclake_plus and provide it with the analyser inputfilename
    else theOutObject.printError("Unable to identify model from input filename");
    // unable identify model from excel filename
}
