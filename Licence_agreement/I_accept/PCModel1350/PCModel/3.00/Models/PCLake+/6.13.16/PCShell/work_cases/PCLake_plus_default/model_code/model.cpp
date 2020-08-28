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
static double forc[4];
double &time = forc[0];
double &mMixDepth = forc[1];
double &mPLoadEpi = forc[2];
double &mPLoadEpiMult = forc[3];
#define MAXFORC 4

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
  yout[0] = oO2WHyp;
  yout[1] = aDVeg;
  yout[2] = oO2WEpi;
  yout[3] = oPTotWHyp;
  yout[4] = oChlaHyp;
  yout[5] = aSecchiT;
  yout[6] = oPTotWEpi;
  yout[7] = oChlaEpi;
  yout[8] = aDError;
  yout[9] = aNError;
  yout[10] = aPError;
  yout[11] = aSiError;
  yout[12] = aO2Error;
  yout[13] = aDepthError;
}
}
