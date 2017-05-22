#version 410 compatibility
#define final
#define fsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

const bool    colortex0MipmapEnabled = true;
const bool    colortex5MipmapEnabled = true;

varying vec4 texcoord;

uniform sampler2D colortex0;
uniform sampler2D colortex1;
uniform sampler2D colortex5;

uniform sampler2D depthtex0;
uniform sampler2D depthtex1;

uniform sampler2D noisetex;

uniform mat4 gbufferProjectionInverse;

uniform vec3 upPosition;
uniform vec3 sunPosition;
uniform ivec2 eyeBrightnessSmooth;
uniform float centerDepthSmooth;
uniform float aspectRatio;
uniform float viewWidth;
uniform float viewHeight;
uniform float frameTimeCounter;
uniform float rainStrength;
uniform int isEyeInWater;

#include "lib/noise.glsl"
#include "lib/dither.glsl"

vec4 cubic(float x) {
  float x2 = x * x;
  float x3 = x2 * x;
  vec4 w;
  w.x =   -x3 + 3*x2 - 3*x + 1;
  w.y =  3*x3 - 6*x2       + 4;
  w.z = -3*x3 + 3*x2 + 3*x + 1;
  w.w =  x3;
  return w / 6.f;
}

vec4 BicubicTexture(in sampler2D tex, in vec2 coord) {
  vec2 resolution = vec2(viewWidth, viewHeight);

  coord *= resolution;

  float fx = fract(coord.x);
  float fy = fract(coord.y);
  coord.x -= fx;
  coord.y -= fy;

  vec4 xcubic = cubic(fx);
  vec4 ycubic = cubic(fy);

  vec4 c = vec4(coord.x - 0.5, coord.x + 1.5, coord.y - 0.5, coord.y + 1.5);
  vec4 s = vec4(xcubic.x + xcubic.y, xcubic.z + xcubic.w, ycubic.x + ycubic.y, ycubic.z + ycubic.w);
  vec4 offset = c + vec4(xcubic.y, xcubic.w, ycubic.y, ycubic.w) / s;

  vec4 sample0 = texture2D(tex, vec2(offset.x, offset.z) / resolution);
  vec4 sample1 = texture2D(tex, vec2(offset.y, offset.z) / resolution);
  vec4 sample2 = texture2D(tex, vec2(offset.x, offset.w) / resolution);
  vec4 sample3 = texture2D(tex, vec2(offset.y, offset.w) / resolution);

  float sx = s.x / (s.x + s.y);
  float sy = s.z / (s.z + s.w);

  return mix( mix(sample3, sample2, sx), mix(sample1, sample0, sx), sy);
}

vec2 scaleTex(vec2 pos, float scale) {
	vec2 ghostPos = -pos + vec2(1.0);
	vec2 ghost1 = (vec2(0.5) - ghostPos)*scale+vec2(0.5);
	return ghost1;
}

vec2 distortTexcoord() {
  vec2 newTC = texcoord.st;

  #ifdef LENS_DISTORTION
    float lensDistortion = length(newTC.xy * vec2(aspectRatio, 1.0) - vec2(0.5) * vec2(aspectRatio, 1.0));
          lensDistortion = mix(sin((1.0 - lensDistortion) * PI * 0.5), 1.0, LDIST_AMOUNT);

    newTC = scaleTex(newTC, (1.0 / lensDistortion) * LDIST_AMOUNT);
  #endif

  if (newTC.x < 0.0 || newTC.y < 0.0 || newTC.x > 1.0 || newTC.y > 1.0)
    newTC = texcoord.st;

  return newTC;
}

float exposure() {
  float moonFade = smoothstep(-0.1, 0.1, dot(normalize(-sunPosition), normalize(upPosition)));
  return 0.0;//mix(saturate((-eyeBrightnessSmooth.y + 230) / 100.0), 1.0, moonFade);
}

#ifdef FILM_GRAIN
  void filmGrain(inout vec3 color) {
    vec2 coord = texcoord.st * 2.6 * vec2(aspectRatio, 1.0) + frameTimeCounter * 2.0;
    color *= mix(0.5 + texture2D(noisetex, coord).r, 1.0, 1.0 - FILM_GRAIN_STRENGTH);
  }
#endif

#ifdef EDGE_SHARPENING
  void sharpen(inout vec3 color, in vec2 coord) {
    vec3 highlight = toLinear(texture2DLod(colortex0, coord.st + vec2(0.002, 0.0), 2.0).rgb);
    color = max(mix(color, min(highlight, 1.0), -1.0), 0.0);
  }
#endif

vec3 curve(vec3 x, vec3 a, vec3 b, vec3 c, vec3 d, vec3 e) {
  x *= max(vec3(0.0), a * (1.0 + exposure() * 5.0));
	x  = ((x * (c * x + 0.5)) / (x * (c * x + 1.7) + b)) + e;
  x  = pow(x, d);

  return x;
}

vec3 burgess(vec3 x) {
  vec3 a, b, c, d, e, f;
  float g;

//Basic tonemapping, swap between them as you see fit
  a = vec3(1.0, 1.0, 1.0);	//Exposure
  b = vec3(0.6, 0.6, 0.6);	//Contrast
  c = vec3(12.0, 12.0, 12.0);	//Vibrance
  d = vec3(0.76, 0.8, 0.78);	//Gamma
  e = vec3(0.08, 0.08, 0.08);	//Lift
  f = vec3(1.0, 1.0, 1.0);    //Highlights
  g = 1.0;                    //Saturation

#if TONEMAP_PRESET == 1
  //Realer
    a = vec3(0.95, 0.85, 0.75);    //Exposure
    b = vec3(0.19, 0.15, 0.11);    //Contrast
    c = vec3(19.0, 15.0, 11.0);    //Vibrance
    d = vec3(0.5, 0.60, 0.7);    //Gamma
    e = vec3(0.03, 0.03, 0.01);    //Lift
    f = vec3(1.0, 1.0, 1.0);    //Highlights
    g = 0.6;                    //Saturation
#endif

#if TONEMAP_PRESET == 2
  //Warm and dusty
    a = vec3(1.0);    //Exposure
    b = vec3(0.2);    //Contrast
    c = vec3(15.0);    //Vibrance
    d = vec3(0.45);    //Gamma
    e = vec3(0.0);    //Lift
    f = vec3(1.0, 1.0, 1.0);    //Highlights
    g = 0.6;                    //Saturation
#endif

#if TONEMAP_PRESET == 3
  //Warmish and colorful
    a = vec3(0.8, 0.7, 0.9);	//Exposure
    b = vec3(0.1, 0.17, 0.3);	//Contrast
    c = vec3(23.0, 18.0, 14.0);	//Vibrance
    d = vec3(0.8, 0.85, 0.6);	//Gamma
    e = vec3(0.02, 0.03, 0.05);	//Lift
    f = vec3(1.0, 1.0, 1.0);    //Highlights
    g = 1.0;                    //Saturation
#endif

#if TONEMAP_PRESET == 4
  //Tilt
    a = vec3(2.0, 1.7, 1.4);    //Exposure
    b = vec3(0.5, 0.4, 0.4);    //Contrast
    c = vec3(15.0, 15.0, 15.0);    //Vibrance
    d = vec3(0.4, 0.5, 0.6);    //Gamma
    e = vec3(0.00, 0.00, 0.00);    //Lift
    f = vec3(1.0, 1.0, 1.0);    //Highlights
    g = 0.8;                    //Saturation
#endif

#if TONEMAP_PRESET == 5
  //Silvia's Preference (Also default)
    a = vec3(1.5, 1.6, 1.6);	//Exposure
    b = vec3(0.6, 0.6, 0.6);	//Contrast
    c = vec3(12.0, 12.0, 12.0);	//Vibrance
    d = vec3(0.33, 0.36, 0.36);	//Gamma
    e = vec3(0.000, 0.000, 0.000);	//Lift
    f = vec3(1.05, 1.02, 1.0);    //Highlights
    g = 1.09;                    //Saturation
#endif

#if TONEMAP_PRESET == 6
  //Drugs1
    a = vec3(2.0, 1.7, 1.4);    //Exposure
    b = vec3(0.5, 0.4, 0.4);    //Contrast
    c = vec3(1.0, 1.0, 150.0);    //Vibrance
    d = vec3(0.4, 0.5, 0.35);    //Gamma
    e = vec3(0.00, 0.0, 0.02);    //Lift
    f = vec3(1.0, 1.0, 1.0);    //Highlights
    g = 0.5;                    //Saturation
#endif

  e *= smoothstep(0.1, -0.1, dot(normalize(-sunPosition), normalize(upPosition)));
  g *= 1.0 - rainStrength * 0.5;

  vec3 retColor = curve(x, a, b, c, d, e);

  float luma = dot(retColor, lumaCoeff);
  retColor = mix(vec3(luma), retColor, g) / curve(vec3(1.0), a, b, c, d, e);

	return retColor * f;
}

#ifdef DOF
  vec3 toScreenSpace(vec2 p, float depth) {
		vec4 fragposition = gbufferProjectionInverse * vec4(vec3(p, depth) * 2. - 1., 1.);
		return fragposition.xyz /= fragposition.w;
  }

  vec2 GetDistOffset(vec2 uv, vec2 pxoffset, float barrel, float anamorphic) {
      vec2 tocenter = uv.xy - 0.5;
      vec3 prep = normalize(vec3(tocenter.y, -tocenter.x, 0.0));

      float angle = length(tocenter.xy) * 2.221 * barrel;
      vec3 oldoffset = vec3(pxoffset, 0.0);
      oldoffset.xy *= vec2(anamorphic, 1.0 / anamorphic);

      vec3 rotated = oldoffset * cos(angle) + cross(prep, oldoffset) * sin(angle) + prep * dot(prep, oldoffset) * (1.0 - cos(angle));

      return rotated.xy;
  }

  void DoF(inout vec3 color, in vec2 tc) {
    vec3 dof = vec3(0.0);
  	vec3 weight = vec3(0.0);
    float r = 1.0;
    mat2 rot = rotate(GOLDEN_ANGLE);

    // Lens specs referenced from Sigma 35mm F1.4 Art.
		// Focal length of 35mm (Assuming lens does not zoom), with a diaphram size of 25mm at f1.4. For more accurate to lens settings, set blades to 9.
        //focal length pre-calculated from 35/1000 - Strum355
		const float focalLength = 0.035;
		float aperture = (35.0 / FSTOP) / 1000.0;

    float z = -toScreenSpace(tc.st, texture2D(depthtex1, tc.st).x).z;
    float focus = -toScreenSpace(tc.st, centerDepthSmooth).z;

		float pcoc = (aperture) * ((focalLength) * (focus - z)) / (focus * (z - (focalLength)));

    #ifdef TILT_SHIFT
      pcoc = (tc.y - 0.5) * 0.001;
    #endif

    vec2 pcocAngle   = vec2(0.0, pcoc); // Start angle
    vec2 sampleAngle = vec2(0.0, 1.0);
    //pcocAngle = vec2(0.0, 1.0) * 0.002;

    float sizeCorrect = sqrt(ITERATIONS) * 1.35914091423;

    for (int i = 0; i < ITERATIONS; i++) {
      r += 1. / r;
      pcocAngle = rot * pcocAngle;
      vec2 pos;
      vec3 bokeh = vec3(1.0);

      #ifdef HQ_DOF
        sampleAngle = rot * sampleAngle;
        pos = GetDistOffset(tc.st, (r - 1.0) * sampleAngle, 0.0, 1.0)
              / (sizeCorrect) * 0.5 + 0.5;

        bokeh = texture2D(colortex5, pos / pow(2.0, 2.0) + vec2(0.5, 0.0)).rgb;
      #endif

      if (bokeh.r > 0.0 || bokeh.g > 0.0 || bokeh.b > 0.0) {
        pos = GetDistOffset(tc.st, (r - 1.0) * pcocAngle, DISTORTION_BARREL, DISTORTION_ANAMORPHIC)
              / (sizeCorrect) * 0.5 * vec2(1.0, aspectRatio) * aperture * 1000.0;
        dof += toLinear(texture2DLod(colortex0, tc.st + pos, 0.0).rgb) * bokeh;
        weight += bokeh;
      }
    }
    color = dof / weight;
  }
#endif

#ifdef BLOOM
	void doBloom(inout vec3 color, in vec2 tc){
		vec3 blur = vec3(0.0);
    blur += toLinear(BicubicTexture(colortex5, tc.st / pow(2.0, 2.0) + vec2(0.0, 0.0)).rgb * 10.0) * 7.0;
		blur += toLinear(BicubicTexture(colortex5, tc.st / pow(2.0, 3.0) + vec2(0.3, 0.0)).rgb * 12.0) * 6.0;
		blur += toLinear(BicubicTexture(colortex5, tc.st / pow(2.0, 4.0) + vec2(0.0, 0.3)).rgb * 16.0) * 5.0;
		blur += toLinear(BicubicTexture(colortex5, tc.st / pow(2.0, 5.0) + vec2(0.1, 0.3)).rgb * 24.0) * 4.0;
    blur += toLinear(BicubicTexture(colortex5, tc.st / pow(2.0, 6.0) + vec2(0.2, 0.3)).rgb * 30.0) * 3.0;
		blur += toLinear(BicubicTexture(colortex5, tc.st / pow(2.0, 7.0) + vec2(0.3, 0.3)).rgb * 38.0) * 2.0;

    if (isEyeInWater > 0.5) blur *= 16.0;

		color.rgb = mix(color.rgb, blur.rgb, 0.0025);
	}
#endif

#ifdef DYN_LENS
	void doDynLens(inout vec3 color, in vec2 tc){

		vec3 lens = vec3(0.0);

    const vec3 color1 = vec3(1.0,0.6,0.4);
    const vec3 color2 = vec3(1.0,0.4,0.3);

		lens += (BicubicTexture(colortex5, tc.st / pow(2.0, 4.0) + vec2(0.0, 0.5)).rgb * 10.0) * color1;
		lens += (BicubicTexture(colortex5, tc.st / pow(2.0, 4.0) + vec2(0.2, 0.5)).rgb * 10.0) * color2;
		lens += (BicubicTexture(colortex5, tc.st / pow(2.0, 4.0) + vec2(0.4, 0.5)).rgb * 10.0) * color2;
		lens += (BicubicTexture(colortex5, tc.st / pow(2.0, 4.0) + vec2(0.6, 0.5)).rgb * 10.0);

    //FOR DYNAMIC LENS DEBUGGING
    //color *= 0.0;
    //APPLY DYNAMIC LENS

    color += pow(lens, vec3(2.2)) * 0.0002;
	}
#endif


void main() {

  vec2 newTC = distortTexcoord();

	vec3 color = toLinear(texture2D(colortex0, newTC).rgb);


  #ifdef DOF
    DoF(color.rgb, newTC);
  #endif

	#ifdef BLOOM
		doBloom(color.rgb, newTC);
	#endif

  #ifdef DYN_LENS
		doDynLens(color.rgb, newTC);
	#endif

  #ifdef FILM_GRAIN
    filmGrain(color.rgb);
  #endif

  #ifdef EDGE_SHARPENING
    sharpen(color.rgb, newTC);
  #endif

	//robobo1221sTonemap(color);
	color = burgess(color);

  float noise = texture2D(noisetex, texcoord.st * 10.0).r;
  color += mix(-0.5/255.0, 0.5/255.0, noise);

	//color += bayer(gl_FragCoord.st) / 255.0;

  gl_FragColor = vec4(color, 1.0);
}
