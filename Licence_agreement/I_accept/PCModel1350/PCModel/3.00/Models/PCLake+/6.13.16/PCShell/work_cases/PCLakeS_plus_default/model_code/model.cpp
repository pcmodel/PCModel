#include <R.h>

// define functions
#define EQ ==
#define OR ||
#define AND &&
#define FLOOR floor
#define SIN sin
#define COS cos
#define TAN tan
#define ACOS acos
#define ASIN asin
#define ATAN atan
#define EXP exp
#define MIN min
#define MAX max
#define LN log
#define SQRT sqrt
#define POW pow
#define IF
#define THEN ?
#define ELSEIF :
#define ELSE :
#define ENDIF 
#define FALSE 0.0
#define TRUE 1.0
#define LT <
#define LE <=
#define GT >
#define GE >=


inline double max(double a, double b) {double m = a; return b > m ? b : m;}
inline double max(double a, double b, double c) {double m = a; m = b > m ? b : m; return c > m ? c : m;}
inline double max(double a, double b, double c, double d) {double m = a; m = b > m ? b : m; m = c > m ? c : m; return d > m ? d : m;}
inline double min(double a, double b) {double m = a; return b < m ? b : m;}
inline double min(double a, double b, double c) {double m = a; m = b < m ? b : m; return c < m ? c : m;}
inline double min(double a, double b, double c, double d) {double m = a; m = b < m ? b : m; m = c < m ? c : m; return d < m ? d : m;}

// define external forcings
static double forc[16];
double &time = forc[0];
double &mPLoadEpiN0 = forc[1];
double &mPLoadEpiMultN0 = forc[2];
double &mPLoadEpiN1 = forc[3];
double &mPLoadEpiMultN1 = forc[4];
double &mPLoadEpiN2 = forc[5];
double &mPLoadEpiMultN2 = forc[6];
double &mPLoadEpiN3 = forc[7];
double &mPLoadEpiMultN3 = forc[8];
double &mPLoadEpiN4 = forc[9];
double &mPLoadEpiMultN4 = forc[10];
double &mFracN0N1 = forc[11];
double &mFracN0N2 = forc[12];
double &mFracN1N3 = forc[13];
double &mFracN2N3 = forc[14];
double &mFracN3N4 = forc[15];
#define MAXFORC 16

#include "../source_cpp_adjusted/arrays.cpp"   // define arrays (parameters, auxiliaries, state variables and initial auxiliaries) and their length
#include "../source_cpp_adjusted/pl61316ra.cpp" // declare auxiliaries
#include "../source_cpp_adjusted/pl61316rp.cpp" // declare parameters
#include "../source_cpp_adjusted/pl61316rs.cpp" // declare state variables
#include "../source_cpp_adjusted/pl61316ri.cpp" // declare initial auxiliaries

// Initialize the model (calculate state variables at t=0)
extern "C" {
void InitializeModel (double *param, double *initState, double *state)
{   
  #include "../source_cpp_adjusted/pl61316rp2.cpp" // declare parameters  
  #include "../source_cpp_adjusted/pl61316rc.cpp" // declare initial states for initialisation  
  #include "../source_cpp_adjusted/pl61316ri.cpp" // declare initial auxiliaries
  #include "../source_cpp_adjusted/pl61316rs.cpp" // declare initial state variables     
  #include "../source_cpp_adjusted/pl61316si.cpp" // calculate state initials
  #include "../source_cpp_adjusted/pl61316ss.cpp" // transform into initial state variables  
}
}

// initialize parameter values
extern "C" {
void initmod(void (* odeparms)(int *, double *))
{
int N=MAXPARAM;
odeparms(&N, param);
}
}

// initialize forcing functions
extern "C" {
void forcc(void (* odeforcs)(int *, double *))
{
int N=MAXFORC;
odeforcs(&N, forc);
}
}

// calculate derivatives and other output variables
extern "C" {
void CalculateDerivatives (int *neq, double *t, double *state, double *deriv,
double *yout, int *ip)
{
if (ip[0] <1) error("nout should be at least 1");
  #include "../source_cpp_adjusted/pl61316rs.cpp"  // declare state variables  
  #include "../source_cpp_adjusted/pl61316rd.cpp"  // declare derivatives
  #include "../source_cpp_adjusted/pl61316sa.cpp"  // calculate auxiliaries
  #include "../source_cpp_adjusted/pl61316sd.cpp"  // calculate derivatives
  yout[0] = oO2WHypN0;
  yout[1] = aDVegN0;
  yout[2] = oO2WEpiN0;
  yout[3] = oPTotWHypN0;
  yout[4] = oChlaHypN0;
  yout[5] = aSecchiTN0;
  yout[6] = oPTotWEpiN0;
  yout[7] = oChlaEpiN0;
  yout[8] = aDErrorN0;
  yout[9] = aNErrorN0;
  yout[10] = aPErrorN0;
  yout[11] = aSiErrorN0;
  yout[12] = aO2ErrorN0;
  yout[13] = aDepthErrorN0;
  yout[14] = oO2WHypN1;
  yout[15] = aDVegN1;
  yout[16] = oO2WEpiN1;
  yout[17] = oPTotWHypN1;
  yout[18] = oChlaHypN1;
  yout[19] = aSecchiTN1;
  yout[20] = oPTotWEpiN1;
  yout[21] = oChlaEpiN1;
  yout[22] = aDErrorN1;
  yout[23] = aNErrorN1;
  yout[24] = aPErrorN1;
  yout[25] = aSiErrorN1;
  yout[26] = aO2ErrorN1;
  yout[27] = aDepthErrorN1;
  yout[28] = oO2WHypN2;
  yout[29] = aDVegN2;
  yout[30] = oO2WEpiN2;
  yout[31] = oPTotWHypN2;
  yout[32] = oChlaHypN2;
  yout[33] = aSecchiTN2;
  yout[34] = oPTotWEpiN2;
  yout[35] = oChlaEpiN2;
  yout[36] = aDErrorN2;
  yout[37] = aNErrorN2;
  yout[38] = aPErrorN2;
  yout[39] = aSiErrorN2;
  yout[40] = aO2ErrorN2;
  yout[41] = aDepthErrorN2;
  yout[42] = oO2WHypN3;
  yout[43] = aDVegN3;
  yout[44] = oO2WEpiN3;
  yout[45] = oPTotWHypN3;
  yout[46] = oChlaHypN3;
  yout[47] = aSecchiTN3;
  yout[48] = oPTotWEpiN3;
  yout[49] = oChlaEpiN3;
  yout[50] = aDErrorN3;
  yout[51] = aNErrorN3;
  yout[52] = aPErrorN3;
  yout[53] = aSiErrorN3;
  yout[54] = aO2ErrorN3;
  yout[55] = aDepthErrorN3;
  yout[56] = oO2WHypN4;
  yout[57] = aDVegN4;
  yout[58] = oO2WEpiN4;
  yout[59] = oPTotWHypN4;
  yout[60] = oChlaHypN4;
  yout[61] = aSecchiTN4;
  yout[62] = oPTotWEpiN4;
  yout[63] = oChlaEpiN4;
  yout[64] = aDErrorN4;
  yout[65] = aNErrorN4;
  yout[66] = aPErrorN4;
  yout[67] = aSiErrorN4;
  yout[68] = aO2ErrorN4;
  yout[69] = aDepthErrorN4;
  yout[70] = wPTranPO4WEpiUpstN1;
  yout[71] = wPTranAIMWEpiUpstN1;
  yout[72] = wPTranGrenEpiUpstN1;
  yout[73] = wPTranDiatEpiUpstN1;
  yout[74] = wPTranBlueEpiUpstN1;
  yout[75] = wPTranDetWEpiUpstN1;
  yout[76] = wPTranZooEpiUpstN1;
  yout[77] = wNTranNH4WEpiUpstN1;
  yout[78] = wNTranNO3WEpiUpstN1;
  yout[79] = wNTranGrenEpiUpstN1;
  yout[80] = wNTranDiatEpiUpstN1;
  yout[81] = wNTranBlueEpiUpstN1;
  yout[82] = wNTranDetWEpiUpstN1;
  yout[83] = wNTranZooEpiUpstN1;
  yout[84] = wDTranDetWEpiUpstN1;
  yout[85] = wDTranIMWEpiUpstN1;
  yout[86] = wDTranGrenEpiUpstN1;
  yout[87] = wDTranDiatEpiUpstN1;
  yout[88] = wDTranBlueEpiUpstN1;
  yout[89] = wDTranZooEpiUpstN1;
  yout[90] = wSiTranSiO2EpiUpstN1;
  yout[91] = wSiTranDetWEpiUpstN1;
  yout[92] = wO2TranWEpiUpstN1;
  yout[93] = wPTranPO4WHypUpstN1;
  yout[94] = wPTranAIMWHypUpstN1;
  yout[95] = wPTranGrenHypUpstN1;
  yout[96] = wPTranDiatHypUpstN1;
  yout[97] = wPTranBlueHypUpstN1;
  yout[98] = wPTranDetWHypUpstN1;
  yout[99] = wPTranZooHypUpstN1;
  yout[100] = wNTranNH4WHypUpstN1;
  yout[101] = wNTranNO3WHypUpstN1;
  yout[102] = wNTranGrenHypUpstN1;
  yout[103] = wNTranDiatHypUpstN1;
  yout[104] = wNTranBlueHypUpstN1;
  yout[105] = wNTranDetWHypUpstN1;
  yout[106] = wNTranZooHypUpstN1;
  yout[107] = wDTranDetWHypUpstN1;
  yout[108] = wDTranIMWHypUpstN1;
  yout[109] = wDTranGrenHypUpstN1;
  yout[110] = wDTranDiatHypUpstN1;
  yout[111] = wDTranBlueHypUpstN1;
  yout[112] = wDTranZooHypUpstN1;
  yout[113] = wSiTranSiO2HypUpstN1;
  yout[114] = wSiTranDetWHypUpstN1;
  yout[115] = wO2TranWHypUpstN1;
  yout[116] = wPTranPO4WEpiUpstN2;
  yout[117] = wPTranAIMWEpiUpstN2;
  yout[118] = wPTranGrenEpiUpstN2;
  yout[119] = wPTranDiatEpiUpstN2;
  yout[120] = wPTranBlueEpiUpstN2;
  yout[121] = wPTranDetWEpiUpstN2;
  yout[122] = wPTranZooEpiUpstN2;
  yout[123] = wNTranNH4WEpiUpstN2;
  yout[124] = wNTranNO3WEpiUpstN2;
  yout[125] = wNTranGrenEpiUpstN2;
  yout[126] = wNTranDiatEpiUpstN2;
  yout[127] = wNTranBlueEpiUpstN2;
  yout[128] = wNTranDetWEpiUpstN2;
  yout[129] = wNTranZooEpiUpstN2;
  yout[130] = wDTranDetWEpiUpstN2;
  yout[131] = wDTranIMWEpiUpstN2;
  yout[132] = wDTranGrenEpiUpstN2;
  yout[133] = wDTranDiatEpiUpstN2;
  yout[134] = wDTranBlueEpiUpstN2;
  yout[135] = wDTranZooEpiUpstN2;
  yout[136] = wSiTranSiO2EpiUpstN2;
  yout[137] = wSiTranDetWEpiUpstN2;
  yout[138] = wO2TranWEpiUpstN2;
  yout[139] = wPTranPO4WHypUpstN2;
  yout[140] = wPTranAIMWHypUpstN2;
  yout[141] = wPTranGrenHypUpstN2;
  yout[142] = wPTranDiatHypUpstN2;
  yout[143] = wPTranBlueHypUpstN2;
  yout[144] = wPTranDetWHypUpstN2;
  yout[145] = wPTranZooHypUpstN2;
  yout[146] = wNTranNH4WHypUpstN2;
  yout[147] = wNTranNO3WHypUpstN2;
  yout[148] = wNTranGrenHypUpstN2;
  yout[149] = wNTranDiatHypUpstN2;
  yout[150] = wNTranBlueHypUpstN2;
  yout[151] = wNTranDetWHypUpstN2;
  yout[152] = wNTranZooHypUpstN2;
  yout[153] = wDTranDetWHypUpstN2;
  yout[154] = wDTranIMWHypUpstN2;
  yout[155] = wDTranGrenHypUpstN2;
  yout[156] = wDTranDiatHypUpstN2;
  yout[157] = wDTranBlueHypUpstN2;
  yout[158] = wDTranZooHypUpstN2;
  yout[159] = wSiTranSiO2HypUpstN2;
  yout[160] = wSiTranDetWHypUpstN2;
  yout[161] = wO2TranWHypUpstN2;
  yout[162] = wPTranPO4WEpiUpstN3;
  yout[163] = wPTranAIMWEpiUpstN3;
  yout[164] = wPTranGrenEpiUpstN3;
  yout[165] = wPTranDiatEpiUpstN3;
  yout[166] = wPTranBlueEpiUpstN3;
  yout[167] = wPTranDetWEpiUpstN3;
  yout[168] = wPTranZooEpiUpstN3;
  yout[169] = wNTranNH4WEpiUpstN3;
  yout[170] = wNTranNO3WEpiUpstN3;
  yout[171] = wNTranGrenEpiUpstN3;
  yout[172] = wNTranDiatEpiUpstN3;
  yout[173] = wNTranBlueEpiUpstN3;
  yout[174] = wNTranDetWEpiUpstN3;
  yout[175] = wNTranZooEpiUpstN3;
  yout[176] = wDTranDetWEpiUpstN3;
  yout[177] = wDTranIMWEpiUpstN3;
  yout[178] = wDTranGrenEpiUpstN3;
  yout[179] = wDTranDiatEpiUpstN3;
  yout[180] = wDTranBlueEpiUpstN3;
  yout[181] = wDTranZooEpiUpstN3;
  yout[182] = wSiTranSiO2EpiUpstN3;
  yout[183] = wSiTranDetWEpiUpstN3;
  yout[184] = wO2TranWEpiUpstN3;
  yout[185] = wPTranPO4WHypUpstN3;
  yout[186] = wPTranAIMWHypUpstN3;
  yout[187] = wPTranGrenHypUpstN3;
  yout[188] = wPTranDiatHypUpstN3;
  yout[189] = wPTranBlueHypUpstN3;
  yout[190] = wPTranDetWHypUpstN3;
  yout[191] = wPTranZooHypUpstN3;
  yout[192] = wNTranNH4WHypUpstN3;
  yout[193] = wNTranNO3WHypUpstN3;
  yout[194] = wNTranGrenHypUpstN3;
  yout[195] = wNTranDiatHypUpstN3;
  yout[196] = wNTranBlueHypUpstN3;
  yout[197] = wNTranDetWHypUpstN3;
  yout[198] = wNTranZooHypUpstN3;
  yout[199] = wDTranDetWHypUpstN3;
  yout[200] = wDTranIMWHypUpstN3;
  yout[201] = wDTranGrenHypUpstN3;
  yout[202] = wDTranDiatHypUpstN3;
  yout[203] = wDTranBlueHypUpstN3;
  yout[204] = wDTranZooHypUpstN3;
  yout[205] = wSiTranSiO2HypUpstN3;
  yout[206] = wSiTranDetWHypUpstN3;
  yout[207] = wO2TranWHypUpstN3;
  yout[208] = wPTranPO4WEpiUpstN4;
  yout[209] = wPTranAIMWEpiUpstN4;
  yout[210] = wPTranGrenEpiUpstN4;
  yout[211] = wPTranDiatEpiUpstN4;
  yout[212] = wPTranBlueEpiUpstN4;
  yout[213] = wPTranDetWEpiUpstN4;
  yout[214] = wPTranZooEpiUpstN4;
  yout[215] = wNTranNH4WEpiUpstN4;
  yout[216] = wNTranNO3WEpiUpstN4;
  yout[217] = wNTranGrenEpiUpstN4;
  yout[218] = wNTranDiatEpiUpstN4;
  yout[219] = wNTranBlueEpiUpstN4;
  yout[220] = wNTranDetWEpiUpstN4;
  yout[221] = wNTranZooEpiUpstN4;
  yout[222] = wDTranDetWEpiUpstN4;
  yout[223] = wDTranIMWEpiUpstN4;
  yout[224] = wDTranGrenEpiUpstN4;
  yout[225] = wDTranDiatEpiUpstN4;
  yout[226] = wDTranBlueEpiUpstN4;
  yout[227] = wDTranZooEpiUpstN4;
  yout[228] = wSiTranSiO2EpiUpstN4;
  yout[229] = wSiTranDetWEpiUpstN4;
  yout[230] = wO2TranWEpiUpstN4;
  yout[231] = wPTranPO4WHypUpstN4;
  yout[232] = wPTranAIMWHypUpstN4;
  yout[233] = wPTranGrenHypUpstN4;
  yout[234] = wPTranDiatHypUpstN4;
  yout[235] = wPTranBlueHypUpstN4;
  yout[236] = wPTranDetWHypUpstN4;
  yout[237] = wPTranZooHypUpstN4;
  yout[238] = wNTranNH4WHypUpstN4;
  yout[239] = wNTranNO3WHypUpstN4;
  yout[240] = wNTranGrenHypUpstN4;
  yout[241] = wNTranDiatHypUpstN4;
  yout[242] = wNTranBlueHypUpstN4;
  yout[243] = wNTranDetWHypUpstN4;
  yout[244] = wNTranZooHypUpstN4;
  yout[245] = wDTranDetWHypUpstN4;
  yout[246] = wDTranIMWHypUpstN4;
  yout[247] = wDTranGrenHypUpstN4;
  yout[248] = wDTranDiatHypUpstN4;
  yout[249] = wDTranBlueHypUpstN4;
  yout[250] = wDTranZooHypUpstN4;
  yout[251] = wSiTranSiO2HypUpstN4;
  yout[252] = wSiTranDetWHypUpstN4;
  yout[253] = wO2TranWHypUpstN4;
  yout[254] = wDTranWEpiUpstTotN1;
  yout[255] = wNTranWEpiUpstTotN1;
  yout[256] = wO2TranWEpiUpstTotN1;
  yout[257] = wPTranWEpiUpstTotN1;
  yout[258] = wSiTranWEpiUpstTotN1;
  yout[259] = wDTranWHypUpstTotN1;
  yout[260] = wNTranWHypUpstTotN1;
  yout[261] = wO2TranWHypUpstTotN1;
  yout[262] = wPTranWHypUpstTotN1;
  yout[263] = wSiTranWHypUpstTotN1;
  yout[264] = wDTranWEpiUpstTotN2;
  yout[265] = wNTranWEpiUpstTotN2;
  yout[266] = wO2TranWEpiUpstTotN2;
  yout[267] = wPTranWEpiUpstTotN2;
  yout[268] = wSiTranWEpiUpstTotN2;
  yout[269] = wDTranWHypUpstTotN2;
  yout[270] = wNTranWHypUpstTotN2;
  yout[271] = wO2TranWHypUpstTotN2;
  yout[272] = wPTranWHypUpstTotN2;
  yout[273] = wSiTranWHypUpstTotN2;
  yout[274] = wDTranWEpiUpstTotN3;
  yout[275] = wNTranWEpiUpstTotN3;
  yout[276] = wO2TranWEpiUpstTotN3;
  yout[277] = wPTranWEpiUpstTotN3;
  yout[278] = wSiTranWEpiUpstTotN3;
  yout[279] = wDTranWHypUpstTotN3;
  yout[280] = wNTranWHypUpstTotN3;
  yout[281] = wO2TranWHypUpstTotN3;
  yout[282] = wPTranWHypUpstTotN3;
  yout[283] = wSiTranWHypUpstTotN3;
  yout[284] = wDTranWEpiUpstTotN4;
  yout[285] = wNTranWEpiUpstTotN4;
  yout[286] = wO2TranWEpiUpstTotN4;
  yout[287] = wPTranWEpiUpstTotN4;
  yout[288] = wSiTranWEpiUpstTotN4;
  yout[289] = wDTranWHypUpstTotN4;
  yout[290] = wNTranWHypUpstTotN4;
  yout[291] = wO2TranWHypUpstTotN4;
  yout[292] = wPTranWHypUpstTotN4;
  yout[293] = wSiTranWHypUpstTotN4;
}
}
