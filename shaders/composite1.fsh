#version 410 compatibility
#define composite1
#define fsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

// Shadow Settings
const bool 		shadowHardwareFiltering 	= false;
const int		  shadowMapResolution		  	= 2048;
const float 	shadowDistance 			    	= 240.0;

// Buffer Mipmaps
const bool    colortex0MipmapEnabled		= true;
const bool    colortex5MipmapEnabled		= true;
const bool    shadowtex0MipmapEnabled	  = true;

varying vec4 texcoord;

uniform sampler2D colortex0;
uniform sampler2D colortex1;
uniform sampler2D colortex2;
uniform sampler2D colortex3;
uniform sampler2D colortex5;

uniform sampler2D depthtex0;
uniform sampler2D depthtex1;

uniform sampler2D shadowtex0;
uniform sampler2D shadowtex1;
uniform sampler2D shadowcolor0;

uniform sampler2D noisetex;

uniform mat4 gbufferProjection;
uniform mat4 gbufferModelView;
uniform mat4 gbufferModelViewInverse;
uniform mat4 gbufferProjectionInverse;
uniform mat4 shadowModelView;
uniform mat4 shadowProjection;
uniform mat4 shadowModelViewInverse;

uniform vec3 shadowLightPosition;
uniform vec3 upPosition;
uniform vec3 sunPosition;
uniform vec3 cameraPosition;
uniform float rainStrength;
uniform float viewWidth;
uniform float viewHeight;
uniform float aspectRatio;
uniform float far;
uniform float near;
uniform int isEyeInWater;
uniform int worldTime;

#include "lib/encoding.glsl"
#include "lib/entity_ids.glsl"

#include "lib/basics.glsl"
#include "lib/time.glsl"
#include "lib/colors.glsl"

#include "lib/noise.glsl"
#include "lib/dither.glsl"

#include "lib/shadow_positions.glsl"

#include "lib/space_conversions.glsl"

#include "lib/sky.glsl"

float distx(float dist){
	return (far * (dist - near)) / (dist * (far - near));
}

float getDepth(float depth) {
    return 2.0 * near * far / (far + near - (2.0 * depth - 1.0) * (far - near));
}

float ld(float depth) {
    return (2.0 * near) / (far + near - depth * (far - near));
}

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

vec4 BicubicTextureLod(in sampler2D tex, in vec2 coord, in float lod) {
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

  vec4 sample0 = texture2DLod(tex, vec2(offset.x, offset.z) / resolution, lod);
  vec4 sample1 = texture2DLod(tex, vec2(offset.y, offset.z) / resolution, lod);
  vec4 sample2 = texture2DLod(tex, vec2(offset.x, offset.w) / resolution, lod);
  vec4 sample3 = texture2DLod(tex, vec2(offset.y, offset.w) / resolution, lod);

  float sx = s.x / (s.x + s.y);
  float sy = s.z / (s.z + s.w);

  return mix( mix(sample3, sample2, sx), mix(sample1, sample0, sx), sy);
}

const vec2 shadowOffsets[5] = vec2[5](
									vec2(0.0, 1.0),
	vec2(1.0, 0.0), vec2(0.0, 0.0), vec2(-1.0, 0.0),
									vec2(0.0, -1.0)
);

float distanceStep(vec4 sPos) {
  float dither = bayer(gl_FragCoord.xy) * PI * 2.0;
  float size = 10.0 * getDistortFactor(sPos);

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

vec3 getShadows(vec3 fpos, bool SSS) {
	float shadowDepth;
	if (SSS) shadowDepth = depth1;
	else 		 shadowDepth = depth2;
	vec4 sPos = biasedShadows(getShadowSpace(shadowDepth, texcoord.st));

  vec4 lightColor  = vec4(0.0);
  float shadow0 	 = 0.0;
  float shadow1 	 = 0.0;
	vec3 directLight = vec3(0.0);

  float size;
	if (SSS) size = 100.0;
	else 		 size = 2.0;

  float dither = bayer(gl_FragCoord.xy) * PI * 2.0;

	float distance   = length(fpos);
	float filterDist = 40.0;

  #ifdef VPSS
    if (!SSS) size *= distanceStep(sPos);
  #endif
  size *= getDistortFactor(sPos);

  int samples = 2;

	if (SSS) {
		if (water > 0.5) {
			for(int i = 0; i < 10; i++) {
	      vec2 offset = vec2(0.0, float(i) + 1.0) * size * rotate(dither) / shadowMapResolution / 10.0;
	      shadow0    += (saturate((shadowStep(shadowtex0, vec3(sPos.xy + offset, sPos.z), 5.0) + 0.1) * 8.0) - 0.8) * 5.0 / 10.0;
	    }
		}
		directLight = vec3(shadow0);
	} else {
	  if (land > 0.5) {
			shadow0 = 0;
			for(int i = 0; i < 5; i ++) {
				shadow0 += texture2DLod(colortex0, texcoord.st + shadowOffsets[i] / vec2(1.0, aspectRatio) * 0.005 / (-fpos.z), 0.5 + (3.0 / (-fpos.z))).a / 5.0;
			}

	    lightColor = toLinear(texture2D(shadowcolor0, sPos.xy));
			if (water > 0.5) lightColor.rgb *= 4.5;
	    shadow1    = shadowStep(shadowtex1, sPos.xyz, 1.0);
	  }
		directLight = clamp(shadow0, 0.0, 1.0) + (lightColor.rgb * (1.0 - lightColor.a) * shadow1);
	}

	return directLight;
}

#include "lib/PBR/diffuse.glsl"
#include "lib/PBR/specular.glsl"

float filterAO() {
  float ao = texture2DLod(colortex5, texcoord.st, 1.5).a;
  return ao;
}

vec3 waterAbsorption(vec3 fpos1, vec3 fpos2) {

	float depthMap = distance(fpos1, fpos2);

	return powf(waterAbsorptionColor, pow(depthMap, 1.8)* 1.1);
}

vec3 waterScatter(vec3 fpos1, vec3 fpos2) {
	float metricDepth1 = -fpos1.z;
	float metricDepth2 = -fpos2.z;
	float depthMap = 0.1 * saturate(metricDepth2 - metricDepth1);

	vec3 castedSkylight = getSky(normal2, false);
	vec3 avgSkylight    = YxyToRGB(calculateZenithLuminanceYxy(turbidity, acos(mDot(sunVec, upVec)))) * lightColor * (1.0 - rainStrength * 0.5);
	castedSkylight 			= mix(castedSkylight, powf(avgSkylight, CONTRAST) * BRIGHTNESS, 0.1);

	vec3 skyLightmap 		= pow(lightmaps2.y, 4.0) * castedSkylight;
	vec3 shadowLightmap = getShadows(fpos2, true).r * lightColor * (1.0 - rainStrength);

	vec3 waterShading		= skyLightmap + shadowLightmap;

	return waterColor * waterShading * depthMap;
}

void doShading(inout vec3 color, vec3 fpos1, vec3 fpos2) {
	float diffuse = Burley(-normalize(fpos2), lightVec, normal1, 0.5);
	vec3 specular = BRDF(normal1, normalize(-fpos1), lightVec, 0.4, vec3(0.02));

  float ao = mix(filterAO(), 1.0, emitter);
	vec3 castedSkylight = getSky(normalize(normal1), false);
	vec3 avgSkylight    = YxyToRGB(calculateZenithLuminanceYxy(turbidity, acos(mDot(sunVec, upVec)))) * lightColor * (1.0 - rainStrength * 0.5);
	castedSkylight 			= mix(castedSkylight, powf(avgSkylight, CONTRAST) * BRIGHTNESS, 0.9);

  vec3 torchLightmap 	= ((1.0 / (1.0 - pow(mix(lightmaps1.x, 0.8, emitter), 6.0)) - 1.0)
											+ emitter * pow(length(color), 4.0) * 6.0) *
	 										torchColor * pow(ao, mix(0.0, 6.0, 1.0 - pow(lightmaps1.x, 3.0)));

  vec3 skyLightmap 		= pow(lightmaps1.y, 4.0) *
											castedSkylight * pow(ao, mix(1.0, 4.0, 1.0 - pow(lightmaps1.y, 3.0)));

	vec3 shadowLightmap = getShadows(fpos2, false) * lightColor * (1.0 - rainStrength);

	vec3 SSS 						= powf(color, 0.5) * saturate(shadowLightmap) * pow(1.0 - diffuse, 2.0) * translucent;

	shadowLightmap *= diffuse;

  vec3 finalShading = torchLightmap + skyLightmap + shadowLightmap + SSS;

  color *= finalShading;
	color += specular * shadowLightmap;

	if (water > 0.5 && transparent > 0.5 && isEyeInWater < 0.5) {
		color *= waterAbsorption(fpos1, fpos2);
		color += waterScatter(fpos1, fpos2);
	}
}

#ifdef VOLUMETRIC_LIGHT
	vec3 getVL() {
		float dither = bayer(gl_FragCoord.xy);
		vec3 step = vec3(0.0);
		float incriment = VL_DISTANCE / VL_STEPS;

		for (int i = 0; i < VL_STEPS; i ++) {
			float dist = 0.01 + i + dither * incriment;
			if (getDepth(depth2) < dist) break;
			vec4 sPos = biasedShadows(getShadowSpace(distx(dist), texcoord.st));

			float vl0 = floor(shadowStep(shadowtex0, sPos.xyz, 1.0));
			#ifdef COLORED_VL
				float vl1 = floor(shadowStep(shadowtex1, sPos.xyz, 1.0));
				vec4 vlColor = toLinear(texture2D(shadowcolor0, sPos.xy));

				if (isEyeInWater > 0.5) {
					vlColor.rgb *= waterColor * 2000.0;
				}

				step += saturate(vl0) + (vlColor.rgb * (1.0 - vlColor.a) * vl1);
			#else
				step += vec3(vl0);
			#endif
		}
		return step / VL_STEPS;
	}
#endif

void main() {

	vec3 color = toLinear(texture2D(colortex0, texcoord.st).rgb);

	vec3 albedo = toGamma(color);

	vec3 fpos1 = toScreenSpace(texcoord.st, depth1);
  vec3 fpos2 = toScreenSpace(texcoord.st, depth2);


  doShading(color.rgb, fpos1, fpos2);

	vec3 vl = vec3(0.0);

	#ifdef VOLUMETRIC_LIGHT
		vl = getVL();
	#endif

	if (isnan(color)) color = vec3(0.0);

	//color.rgb = toLinear(texture2D(colortex5, texcoord.st).rgb);

/* DRAWBUFFERS:05 */

	gl_FragData[0] = vec4(toGamma(color.rgb), 1.0);
  gl_FragData[1] = vec4(vl, encodeColor(albedo));
}
