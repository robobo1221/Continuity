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
const int colortex4Format = RGBA16;
const int colortex5Format = RGBA16;
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
const int		  shadowMapResolution		  = 2048; //[512 1024 2048 4092 8192 16368]
const float 	shadowDistance 			    = 240.0; //[120 180 240 300 360]

const bool 		shadowcolor0Mipmap = true;
const bool 		shadowcolor0Nearest = false;
const bool 		shadowcolor1Mipmap = true;
const bool 		shadowcolor1Nearest = false;

varying vec4 texcoord;

uniform sampler2D colortex0;
uniform sampler2D colortex1;
uniform sampler2D colortex2;
uniform sampler2D colortex4;

uniform sampler2D depthtex0;
uniform sampler2D depthtex1;

uniform sampler2D shadowtex0;
uniform sampler2D shadowtex1;
uniform sampler2DShadow shadowcolor0;
uniform sampler2D shadowcolor1;

uniform sampler2D noisetex;

uniform mat4 gbufferProjection;
uniform mat4 gbufferModelView;
uniform mat4 gbufferModelViewInverse;
uniform mat4 gbufferProjectionInverse;
uniform mat4 shadowModelView;
uniform mat4 shadowModelViewInverse;
uniform mat4 shadowProjection;

uniform vec3 shadowLightPosition;
uniform vec3 upPosition;
uniform vec3 sunPosition;
uniform vec3 cameraPosition;
uniform float rainStrength;
uniform float aspectRatio;
uniform float viewWidth;
uniform float viewHeight;
uniform float far;
uniform int worldTime;
uniform int isEyeInWater;

#include "lib/encoding.glsl"
#include "lib/entity_ids.glsl"

#include "lib/basics.glsl"
#include "lib/colors.glsl"
#include "lib/time.glsl"

#include "lib/noise.glsl"
#include "lib/dither.glsl"

#include "lib/shadow_positions.glsl"

#include "lib/space_conversions.glsl"

#include "lib/sky.glsl"
#include "lib/PBR/diffuse.glsl"

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

  const int samples = 32;

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


#define hammersley(i, N) vec2( float(i) / float(N), float( bitfieldReverse(i) ) * 2.3283064365386963e-10 )
#define tau 6.2831853071795864769252867665590
#define circlemap(p) (vec2(cos((p).y*tau), sin((p).y*tau)) * p.x)
#define semicirclemap(p) (vec2(cos((p).y*PI), sin((p).y*PI)) * sqrt(p.x) )

#ifdef SSAO
	#if SSAO_METHOD == 1

	  float jaao(vec2 p) {
		const float r = 1.3;
    	const int steps = 3;

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

#if   AMBIENT_LIGHTING == 1
  vec4 getAmbientLighting(vec3 fpos) {
    vec3 indirect = js_totalScatter();
    return vec4(indirect, 1.0);
  }
#elif AMBIENT_LIGHTING == 2
  vec4 getAmbientLighting(vec3 fpos) {
    vec3 indirect = js_totalScatter() * 0.4;
		float ao = 1.0;
		#ifdef SSAO
			#if SSAO_METHOD == 1
				ao = jaao(texcoord.st);
			#endif

			#if SSAO_METHOD == 2
				ao = getSSAO(fpos);
			#endif
		#endif
    return vec4(indirect, ao);
  }
#elif AMBIENT_LIGHTING == 3
  vec2 lattice(int i, int N){
  	float sn = fsqrt(float(N));
  	return vec2(   mod( float(i) * PI, sn )  / sn, float(i) / float(N) );
  }

  vec3 getSkyIllumination(vec3 refNormal, vec3 normal, vec3 fpos, float horizon){

  	const int ditherSize = 64;

  	int index = bayer64x64(ivec2(texcoord.st*vec2(viewWidth,viewHeight)));

  	vec3 tangentX = normalize(cross(vec3(0,0,1), normal));
  	vec3 tangentY = cross(normal, tangentX);

  	vec3 sky=vec3(0);

    float skyDistance = far * getAtmosphere();

  	int samples = 16;

  	for (int i = 0; i < samples; i++) {

  		vec2 a=//hammersley( i * 15 + index + 1 , 16 * samples );
  			lattice(i * (ditherSize*ditherSize) + index, (ditherSize*ditherSize) * samples);
  		float angle = a.y * TAU;

  	    float cosx = sqrt(1.0 - a.x);
  	    float sinx = sqrt(a.x)*horizon;

  	    vec3 vector = tangentX * cos(angle) * sinx +
             tangentY * sin(angle) * sinx + normal * cosx;

        vector = normalize(vector);

        vec3 ambientColor = js_getAmbient(vector) + (js_totalScatter() * (skyDistance / (1.0 + skyDistance)));

  	    sky += Burley(-fpos, vector, refNormal, 0.9)*ambientColor;
  	}
  	return sky/float(samples);
  }

  vec4 getAmbientLighting(vec3 fpos) {

		int steps = 4;
		float radius = 8.0;

  	const int ditherSize = 64;

  	int index = bayer64x64(ivec2(texcoord.st*vec2(viewWidth,viewHeight)));

  	vec3 p3 = toScreenSpace(texcoord.st);
  	//vec3 normal = normalize( cross(dFdx(p3), dFdy(p3)) );
  	vec2 clipRadius = vec2(viewHeight / viewWidth, 1.) / length(p3);

  	float totalAmbient = 0.;

  	float skyAngle = 0.;
  	vec3 skyDirection = vec3(0.);

  	float PdotN = dot(p3,normal1);

  	//large scale accurate ao
  	for (int i = 0; i < steps; i++) {
  		vec2 semicirclePoint = semicirclemap(
  			//hammersley( i * 15 + index, 16 * steps )
  			lattice(i * (ditherSize*ditherSize) + index , (ditherSize*ditherSize) * steps)
  		)*clipRadius;

  		vec3 o1 = toScreenSpace( texcoord.st + semicirclePoint * radius ); // right sample
  		vec3 o2 = toScreenSpace( texcoord.st - semicirclePoint * radius ); // left sample

  		vec3 o3 = toScreenSpace( texcoord.st + semicirclePoint * radius / 8.); // right sample
  		vec3 o4 = toScreenSpace( texcoord.st - semicirclePoint * radius / 8.); // left sample

  		vec3 o5 = toScreenSpace( texcoord.st + semicirclePoint * radius / 8. / 8.); // right sample
  		vec3 o6 = toScreenSpace( texcoord.st - semicirclePoint * radius / 8. / 8.); // left sample

  		if(dot(normalize(o3-p3),normal1)>dot(normalize(o1-p3),normal1))
  			o1=o3;

  		if(dot(normalize(o4-p3),normal1)>dot(normalize(o2-p3),normal1))
  			o2=o4;

  		if(dot(normalize(o5-p3),normal1)>dot(normalize(o1-p3),normal1))
  			o1=o5;

  		if(dot(normalize(o6-p3),normal1)>dot(normalize(o2-p3),normal1))
  			o2=o6;


  		vec3 oo1 = o1-p3;
  		float l1 = length(o1);
  		vec3 r1 = o1/l1;
  		o1 = min(l1, PdotN /
  			-clamp(-dot(r1, normal1),0.,1.)
  		) * r1;// tangent plane intersection;
  		o1 = normalize(o1-p3);

  		vec3 oo2 = o2-p3;
  		float l2 = length(o2);
  		vec3 r2 = o2/l2;
  		o2 = min(l2, PdotN /
  			-clamp(-dot(r2, normal1),0.,1.)
  		) * r2;// tangent plane intersection;
  		o2 = normalize(o2-p3);

  		float doto = dot(o1, o2);

  		doto = clamp(doto,-1.,1.); //fixes NaNing


  		float litNormalOcclusion = facos(doto)/PI;
  		skyAngle += facos(doto);
  		skyDirection += normalize(o2+o1+normal1*.001);

  		float distMul = max(1., max(oo1.z, oo2.z) / radius );
  		litNormalOcclusion = 1. - (1. - litNormalOcclusion) / distMul;


  		totalAmbient += litNormalOcclusion;
  	}

  	skyAngle /= float(steps)*PI;
  	skyDirection = normalize(skyDirection);
  	vec3 sky = getSkyIllumination(normal1, skyDirection, fpos, skyAngle);

  	totalAmbient /= float(steps);

    totalAmbient = pow(totalAmbient, mix(2.0, 6.0, 1.0 - pow(lightmaps1.y, 3.0)));

  	return vec4(totalAmbient * sky, 1.0);
  }
#endif

#ifdef GI
	vec3 getGI(vec3 viewSpace){

			float weight = 0.0;
			vec3 indirectLight = vec3(0.0);

			float dither = bayer(gl_FragCoord.st);

			float rotateMult = dither * PI * 2.0;   //Make sure the offset rotates 360 degrees.

			mat2 rotationMatrix = rotate(rotateMult);

			vec4 shadowSpaceNormal = gbufferModelViewInverse * vec4(normal1.xyz, 0.0f);
					 shadowSpaceNormal = shadowModelView * shadowSpaceNormal;
					 shadowSpaceNormal.xyz = normalize(shadowSpaceNormal.xyz);

			vec4 shadowPosition = getShadowSpace(depth2, texcoord.st);
			vec4 biasedShadowPosition = biasedShadows(shadowPosition);

			const float GIsteps = 1.0 / 8.0;

			vec2 circleDistribution = rotationMatrix * vec2(1.0 + dither) / 32.0;

			for (float i = 1.0; i < 2.0; i += GIsteps){

				vec2 offset  = circleDistribution * i;
						 offset *= 1.0 + flength(offset);

				vec4 offsetPosition = vec4(shadowPosition.rg + offset, 0.0, 0.0);
				vec3 biasedPosition = biasedShadows(offsetPosition).xyz;

				vec3 normalSample = texture2D(shadowcolor1, biasedPosition.xy).rgb * 2.0 - 1.0;
						 normalSample.xy = -normalSample.xy;

				float shadow = texture2D(shadowtex1, biasedPosition.xy).x;
				shadow = -2.5 + 5.0 * (shadow - 0.0);

				vec3 samplePos = vec3(offsetPosition.xy, shadow);

				vec3 halfVector = samplePos.xyz - shadowPosition.xyz;

				vec3 lPos = normalize(halfVector);
				float distFromX = length(halfVector);
				float offsetDistFromX = length(vec3(halfVector.xy, (halfVector.z - 0.01) * 5.0));

				float diffuse = Burley(-normalize(viewSpace), vec3(lPos.xy, -(lPos.z + 0.5)), normalize(shadowSpaceNormal.xyz), 0.9);
				if (transparent > 0.5) diffuse = 1.0;
				float sampleWeight = saturate(dot(lPos, normalSample));

				float distanceWeight  = 1.0 / (pow(offsetDistFromX * 20000.0, 1.7) + 5000.0);
				//      distanceWeight *= pow(length(offset), 2.0);

				indirectLight += toLinear(shadow2DLod(shadowcolor0, biasedPosition, 2.0).rgb) * sampleWeight * diffuse * distanceWeight;

				weight++;
			}
			indirectLight /= weight;
			indirectLight *= 4000000.0;

			return indirectLight * pow(lightmaps1.y, 4.0);
	}
#endif

void main() {

	vec3 fpos2 = toScreenSpace(texcoord.st, depth2);

	vec3 color = toLinear(texture2D(colortex0, texcoord.st).rgb);

	vec3 specular = texture2D(colortex4, texcoord.st).rgb;


	float ao = 1.0;
	vec3 gi = vec3(0.0);
	vec4 ambient = getAmbientLighting(fpos2);

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
    if (land > 0.5) gi = getGI(fpos2);
  #endif

  if (isnan(color)) color = vec3(0.0);


/* DRAWBUFFERS:0145 */

  gl_FragData[0] = vec4(toGamma(color.rgb), getShadows(fpos2));
  gl_FragData[1] = vec4(aux1.rgb, encodeColor(specular));
	gl_FragData[2] = ambient;
  gl_FragData[3] = vec4(toGamma(gi), ao);
}
