#version 410 compatibility
#define composite0
#define fsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

/* Buffer Formats
const int colortex0Format = RGBA32F;
const int colortex1Format = RGBA16;
const int colortex2Format = RGBA16;
const int colortex3Format = RGBA16;
const int colortex5Format = RGBA16;
const int shadowcolor0Format = RGBA32F;
*/

// Half Lives
const float 	wetnessHalflife 		    = 70;
const float 	drynessHalflife 		    = 70;
const float 	centerDepthHalflife 	  = 4;
const float 	eyeBrightnessHalflife 	= 7;

// Prebaked Settings
const float		sunPathRotation			    = -40;
const float		ambientOcclusionLevel	  = 0.5;
const int 		noiseTextureResolution  = 512;

// Shadow Settings
const bool 		shadowHardwareFiltering = false;
const int		  shadowMapResolution		  = 2048;
const float 	shadowDistance 			    = 240.0;

varying vec4 texcoord;

uniform sampler2D colortex0;
uniform sampler2D colortex1;
uniform sampler2D colortex2;

uniform sampler2D depthtex0;
uniform sampler2D depthtex1;

uniform sampler2D shadowtex0;
uniform sampler2D shadowtex1;
uniform sampler2D shadowcolor0;
uniform sampler2D shadowcolor1;

uniform sampler2D noisetex;

uniform mat4 gbufferProjection;
uniform mat4 gbufferModelView;
uniform mat4 gbufferModelViewInverse;
uniform mat4 gbufferProjectionInverse;
uniform mat4 shadowModelView;
uniform mat4 shadowProjection;

uniform vec3 shadowLightPosition;
uniform vec3 upPosition;
uniform vec3 sunPosition;
uniform vec3 cameraPosition;
uniform float aspectRatio;
uniform float viewWidth;
uniform float viewHeight;
uniform int worldTime;
uniform int isEyeInWater;

#include "lib/encoding.glsl"
#include "lib/entity_ids.glsl"

#include "lib/basics.glsl"
#include "lib/time.glsl"

#include "lib/noise.glsl"
#include "lib/dither.glsl"

#include "lib/shadow_positions.glsl"

#include "lib/space_conversions.glsl"

const vec2 shadowOffsets[5] = vec2[5](
									vec2(0.0, 1.0),
	vec2(1.0, 0.0), vec2(0.0, 0.0), vec2(-1.0, 0.0),
									vec2(0.0, -1.0)
);

float distanceStep(vec4 sPos) {
  float dither = bayer(gl_FragCoord.xy) * PI * 2.0;
  float size = 10.0 * (getDistortFactor(sPos));

  vec3 depthSample0 = vec3(sPos.xy + shadowOffsets[0] * rotate(dither) * size / shadowMapResolution, sPos.z);
  vec3 depthSample1 = vec3(sPos.xy + shadowOffsets[1] * rotate(dither) * size / shadowMapResolution, sPos.z);
  vec3 depthSample2 = vec3(sPos.xy + shadowOffsets[2] * rotate(dither) * size / shadowMapResolution, sPos.z);
  vec3 depthSample3 = vec3(sPos.xy + shadowOffsets[3] * rotate(dither) * size / shadowMapResolution, sPos.z);
  vec3 depthSample4 = vec3(sPos.xy + shadowOffsets[4] * rotate(dither) * size / shadowMapResolution, sPos.z);

  float occluderDistance = max6(
        depthSample0.z-texture2DLod(shadowtex0, depthSample0.xy, 5.0).x,
        depthSample1.z-texture2DLod(shadowtex0, depthSample1.xy, 5.0).x,
        depthSample2.z-texture2DLod(shadowtex0, depthSample2.xy, 5.0).x,
        depthSample3.z-texture2DLod(shadowtex0, depthSample3.xy, 5.0).x,
        depthSample4.z-texture2DLod(shadowtex0, depthSample4.xy, 5.0).x,
        0.
    );
  return occluderDistance * shadowMapResolution * 0.1;
}

float shadowStep(sampler2D shadow, vec3 sPos, float mult) {
	return clamp(1.0 - max(sPos.z - texture2D(shadow, sPos.xy).x, 0.0) * float(shadowMapResolution) / mult, 0.0, 1.0);
}

float getShadows(in vec3 fpos) {
	vec4 sPos = biasedShadows(getShadowSpace(depth2, texcoord.st));

  float shadow0 	 = 0.0;

  float size = 2.0;

  float dither = bayer(gl_FragCoord.xy) * PI * 2.0;

  #ifdef VPSS
    size *= distanceStep(sPos);
  #endif

  size *= getDistortFactor(sPos);

  int samples = 32;

	float distance   = length(fpos);
	float filterDist = 70.0;

	float offsetShadow = 1.0 + saturate(-dot(normal1, lightVec)) * 0.8;

  if (land > 0.5) {
    for(int i = 0; i < samples; i++) {
      vec2 offset = vec2(0.0, float(i) + 1.0) * size * rotate(dither) / shadowMapResolution / samples;
      shadow0    += shadowStep(shadowtex0, vec3(sPos.xy + offset, sPos.z), offsetShadow) / samples;
    }
  }

	shadow0 = mix(shadow0,
						shadowStep(shadowtex0, sPos.xyz, 1.0),
						smoothstep(filterDist * 0.8, filterDist, distance));

	return shadow0;
}

#ifdef SSAO
	#if SSAO_METHOD == 1

	  #define hammersley(i, N) vec2( float(i) / float(N), float( bitfieldReverse(i) ) * 2.3283064365386963e-10 )
	  #define tau 6.2831853071795864769252867665590
	  #define circlemap(p) (vec2(cos((p).y*tau), sin((p).y*tau)) * p.x)

	  float jaao(vec2 p) {
			float r = 2.0;
    	int steps = 4;

	    int x = int(p.x*viewWidth)  % 4;
	    int y = int(p.y*viewHeight) % 4;
	    int index = (x<<2) + y;

	    vec3 p3 = toScreenSpace(p);
	    vec2 clipRadius = r * vec2(viewHeight/viewWidth,1.) / length(p3);

	    vec3 v = normalize(-p3);

	    float nvisibility = 0.;
	    float vvisibility = 0.;

	    for (int i = 0; i < steps; i++) {
	        vec2 circlePoint = circlemap(
	            hammersley(i*15+index+1, 16*steps)
	        )*clipRadius;

	        vec3 o  = toScreenSpace(circlePoint    +p) - p3;
	        vec3 o2 = toScreenSpace(circlePoint*.25+p) - p3;
	        float l  = length(o );
	        float l2 = length(o2);
	        o /=l ;
	        o2/=l2;

	        nvisibility += clamp(1.-max(
	            dot(o , normal1) - clamp((l -r)/r,0.,1.),
	            dot(o2, normal1) - clamp((l2-r)/r,0.,1.)
	        ), 0., 1.);

	        vvisibility += clamp(1.-max(
	            dot(o , v) - clamp((l -r)/r,0.,1.),
	            dot(o2, v) - clamp((l2-r)/r,0.,1.)
	        ), 0., 1.);
	    }

	    return min(vvisibility*2., nvisibility) / float(steps);
	  }
	#endif

	#if SSAO_METHOD == 2
		float getSSAO(vec3 fragpos) {
	  	const int ndirs = 3;
	  	const int num_samples = 4;
	  	vec3 center_pos = fragpos.rgb;
	  	const float samplingRadius = 3.0;
	  	float radius = 0.2 / (-center_pos.z);
	  	float angle_thresh = 0.05;

	    float occlusion = 0.0;

	  	//setup 4x4 noise pattern on two direction per pixel
	  	vec2 nTC = mod(floor(texcoord.st), 4.0);
	  	float dither = bayer(gl_FragCoord.st) * PI * 2.0;
	  	mat2 noiseM = rotate(dither);

	  	vec2 rd = vec2(radius, radius * aspectRatio) / num_samples;
	  	//pre-rotate direction
	  	vec2 dir = noiseM * vec2(0.0, -1.0);

	  	for (int i = 0; i < ndirs; i++) {

	  		vec2 dir = noiseM * (vec2(cos(PI * 2.0 / ndirs * i), sin(PI * 2.0 / ndirs * i)) * rd);	//jitter the directions

	  		for (int j = 0; j < num_samples; j++) {
	  		  //Marching Time
	  			vec2 sampleOffset = float(j + dither / (PI * 2.0)) * dir * vec2(1.0, aspectRatio);	//jitter start position to get better space coverage
	    		vec2 offset = floor((texcoord.st + sampleOffset) * vec2(viewWidth, viewHeight))
	                      / vec2(viewWidth, viewHeight) + 0.5 / vec2(viewWidth, viewHeight);  //perspective-correct coordinates

	    		if (abs(offset.x - 0.5) < 0.5 &&
	            abs(offset.y-0.5)<0.5 &&
	            abs(offset.x-texcoord.x) > 0.999999/viewWidth &&
	            abs(offset.y-texcoord.y) > 0.999999/viewHeight) {		//discard if sampling original texel or out of screen

	      		vec4 t0 = gbufferProjectionInverse * vec4(vec3(offset, texture2D(depthtex1, offset).x) * 2.0 - 1.0, 1.0);
	      		t0 /= t0.w;

	    			vec3 vec = t0.xyz - fragpos;
	    			float NdotV = dot(normalize(vec), normal1);
	    			float l2 = dot(vec, vec);
	    			occlusion += clamp(NdotV - angle_thresh, 0.0, 1.0) * clamp(1.0 - l2 / samplingRadius, 0.0, 1.0) / (1.0 - angle_thresh);
	    		}
	  		}
	  	}
	  	occlusion = 1.0-pow(occlusion/num_samples/ndirs,1/2.2)*1.1;

	    return mix(pow(occlusion, SSAO_STRENGTH), 1.0, emitter);
	  }
	#endif

#endif

void main() {

	vec3 fpos2 = toScreenSpace(texcoord.st, depth2);

	vec3 color = toLinear(texture2D(colortex0, texcoord.st).rgb);


	float ao = 1.0;
	vec3 gi = vec3(0.0);

	#ifdef SSAO
		#if SSAO_METHOD == 1
			ao = jaao(texcoord.st);
		#endif

		#if SSAO_METHOD == 2
			ao = getSSAO(fpos2);
		#endif

		if (isnan(ao)) ao = 0.0;
		if (isinf(ao)) ao = 1.0;
	#endif

	#ifdef GI
    if (land > 0.5) gi = getSSGI();
  #endif

  if (isnan(color)) color = vec3(0.0);


/* DRAWBUFFERS:05 */

  gl_FragData[0] = vec4(toGamma(color.rgb), getShadows(fpos2));
  gl_FragData[1] = vec4(toGamma(gi), ao);
}
