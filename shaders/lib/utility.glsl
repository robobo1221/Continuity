#ifdef fsh
  #define varying in
#endif

#ifdef vsh
  #define varying out
  #define attribute in
#endif


const vec3 lumaCoeff = vec3(0.2125, 0.7154, 0.0721);

#define PI 3.14159265
#define TAU 6.2831853071795864769252867665590
#define GOLDEN_ANGLE 2.39996322972865332


vec4 nvec4(vec3 pos) { return vec4(pos.xyz, 1.0); }
vec3 nvec3(vec4 pos) { return pos.xyz / pos.w; }

vec4 powf(vec4 a, float b) { return pow(a, vec4(b)); }
vec3 powf(vec3 a, float b) { return pow(a, vec3(b)); }
vec2 powf(vec2 a, float b) { return pow(a, vec2(b)); }

mat2 rotate(float rad) {
	return mat2(
	vec2(cos(rad), -sin(rad)),
	vec2(sin(rad), cos(rad))
	);
}

#define fsqrt(x) intBitsToFloat(0x1FBD1DF5 + (floatBitsToInt(x) >> 1)) // Error of 1.42%
#define finversesqrt(x) intBitsToFloat(0x5F33E79F - (floatBitsToInt(x) >> 1)) // Error of 1.62%

float facos(float x) { // Under 3% error
  float ax = abs(x);
  float res = -0.156583 * ax + PI * 0.5;
  res *= fsqrt(1.0 - ax);
  return x >= 0 ? res : PI - res;
}

float flength(vec2 x) {
	return fsqrt(dot(x, x));
}

float flength(vec3 x) {
	return fsqrt(dot(x, x));
}

float flength(vec4 x) {
	return fsqrt(dot(x, x));
}

#define toLinear(x) powf(x, 2.2)
#define toGamma(x) powf(x, 1.0 / 2.2)

#define saturate(x) clamp(x, 0.0, 1.0)
#define mDot(x, y) max(dot(x, y), 0.0)

#define max3(x,y,z) max(x,max(y,z))
#define max4(x,y,z,w) max(x,max(y,max(z,w)))
#define max5(a,b,c,d,e) max4(a,b,c,max(d,e))
#define max6(a,b,c,d,e,f) max5(a,b,c,d,max(e,f))

#define min3(a,b,c)       min(min(a,b),c)
#define min4(a,b,c,d)     min(min3(a,b,c),d)
#define min5(a,b,c,d,e)   min(min4(a,b,c,d),e)
#define min6(a,b,c,d,e,f) min(min5(a,b,c,d,e),f)

#define isnan(x) (x != x)
