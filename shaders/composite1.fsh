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
uniform float frameTimeCounter;
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

 const int samples = 2;

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

	return powf(waterAbsorptionColor, pow(depthMap, 1.3)* 1.1);
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


float luma(vec3 color) {
  return dot(color, vec3(0.299, 0.587, 0.114));
}

vec3 colorSaturate(in vec3 base, in float saturation) {
    return vec3(mix(base, vec3(luma(base)), -saturation));
}

vec3 nightDesaturation(vec3 inColor, vec2 lightmap){
		vec3 nightColor = vec3(0.25, 0.35, 0.7);
		vec3 desatColor = vec3(dot(inColor, vec3(1.0)));
		float mixAmount = clamp(lightmap.x, 0.0, 1.0);

	return mix(inColor,
				 mix(desatColor * nightColor,
				(mix(desatColor, inColor, 0.5) * colorSaturate(torchColor, 0.35) * 10) * 4, mixAmount), saturate(min(moonFade, 0.65)+pow(1 - lightmap.y, 1.4)));//mix(mix(inColor*torchcolor2*20, desatColor*nightColor, mixAmount), inColor, saturate(TimeNoon+TimeSunset+TimeSunrise-min(pow(rainx, 5.0), 0.7)));
}

void doShading(inout vec3 color, vec3 fpos1, vec3 fpos2) {
	float diffuse = Burley(-normalize(fpos2), lightVec, normal1, 0.5);
	vec3 specular = BRDF(normal1, normalize(-fpos1), lightVec, 0.4, vec3(0.02));

  float ao = mix(filterAO(), 1.0, emitter);
	vec3 castedSkylight = getSky(normalize(normal1), false);
	vec3 avgSkylight    = YxyToRGB(calculateZenithLuminanceYxy(turbidity, acos(mDot(sunVec, upVec)))) * lightColor * (1.0 - rainStrength * 0.5);
	castedSkylight 			= mix(castedSkylight, powf(avgSkylight, CONTRAST) * BRIGHTNESS, 0.9);

  vec3 torchLightmap 	= ((1.0 / (1.0 - pow(mix(lightmaps1.x, 0.8, emitter), 3.0)) - 1.0)
											+ emitter * pow(length(color), 7.0) * 7.0) *
	 										torchColor * pow(ao, mix(0.0, 9.0, 1.0 - pow(lightmaps1.x, 3.0)));

  vec3 skyLightmap 		= pow(lightmaps1.y, 4.0) *
											castedSkylight * pow(ao, mix(1.0, 4.0, 1.0 - pow(lightmaps1.y, 3.0)))+vec3(0.0);

	vec3 shadowLightmap = getShadows(fpos2, false) * lightColor * (1.0 - rainStrength);

	vec3 SSS 						= powf(color, 0.5) * saturate(shadowLightmap) * pow(1.0 - diffuse, 2.0) * translucent;

	shadowLightmap *= diffuse;

  vec3 finalShading = torchLightmap + skyLightmap + shadowLightmap + SSS;

  color *= finalShading;
  color = nightDesaturation(color,lightmaps1*vec2(2.0, 1.0));
	color += specular * shadowLightmap;
//	color = vec3(pow(lightmaps1.y, 5.0));

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

#ifdef VOLUMETRIC_CLOUDS
	vec4 mod289(vec4 x){return x - floor(x * 0.003460) * 289.0;}
	vec4 perm(vec4 x){return mod289(((x * 34.0) + 1.0) * x);}

	float noise3D(vec3 p){
    vec3 a = floor(p);
    vec3 d = p - a;
    d = d * d * (3.0 - 2.0 * d);

    vec4 b = a.xxyy + vec4(0.0, 1.0, 0.0, 1.0);
    vec4 k1 = perm(b.xyxy);
    vec4 k2 = perm(k1.xyxy + b.zzww);

    vec4 c = k2 + a.z;
    vec4 k3 = perm(c);
    vec4 k4 = perm(c + 1.0);

    vec4 o1 = fract(k3 * 0.02439024390243902439024390243902);
    vec4 o2 = fract(k4 * 0.02439024390243902439024390243902);

    vec4 o3 = (o2 * d.z) + (o1 * (1.0 - d.z));
    vec2 o4 = (o3.yw * d.x) + (o3.xz * (1.0 - d.x));

    return (o4.y * d.y) + (o4.x * (1.0 - d.y));
	}

	float cloudFBM3D(vec3 p){

		float wind = abs(frameTimeCounter - 0.5);

		p.xz += wind;

		p *= 0.02;

		float noise  = noise3D(vec3(p.x - wind * 0.01, p.y, p.z - wind * 0.015));
			  	noise += noise3D(p * 3.5) / 3.5;
			  	noise += abs(noise3D(p * 6.125) * 2.0 - 1.0) / 6.125;
			  	noise += abs(noise3D(p * 6.125 * 2.0) * 2.0 - 1.0) / 6.125 / 2.0;

			  	noise = noise * (1.0 - rainStrength * 0.5);
			  	noise = pow(max(1.0 - noise * 1.5 / VOLUMETRIC_CLOUDS_COVERAGE,0.),2.0) * 0.030303;

		return clamp(noise * 10.0, 0.0, 1.0);
	}

	float expDepth(float dist){
		return (far * (dist - near)) / (dist * (far - near));
	}

	vec4 getVolumetricCloudPosition(vec2 coord, float depth)
	{
		vec4 position = gbufferProjectionInverse * vec4(vec3(coord, expDepth(depth)) * 2.0 - 1.0, 1.0);
			 position /= position.w;

		     position.rgb = toWorldSpace(position.rgb);

		     position.rgb += cameraPosition;

		return position;
	}

	vec4 getVolumetricCloudsColor(vec3 wpos, vec3 fpos, vec3 ambient, float phase) {
		const float height = VOLUMETRIC_CLOUDS_HEIGHT;  	//Height of the clouds
		float distRatio = 100.0 * (1.0 + rainStrength * 0.5);  	//Distance between top and bottom of the cloud in block * 10.

		float maxHeight = (distRatio * 0.5) + height;
		float minHeight = height - (distRatio * 0.5);

		if (wpos.y < minHeight || wpos.y > maxHeight){
			return vec4(0.0);
		} else {

			float cloudAlpha = cloudFBM3D(wpos);
			float cloudTreshHold = pow(1.0f - clamp(distance(wpos.y, height) / (distRatio / 2.0f), 0.0f, 1.0f), 12.0);

			cloudAlpha *= cloudTreshHold;

			float absorption = clamp((-(minHeight - wpos.y) / distRatio), 0.0f, 1.0f);

			float sunLightAbsorption = pow(absorption, 5.0);

			vec3 cloudSunlight = lightColor * 10.0 * sunLightAbsorption * (1.0 + phase) * (1.0 - rainStrength * 1.0);

			vec3 cloudColor = mix(cloudSunlight, ambient * (0.25 + (rainStrength * 0.5)), pow(1.0 - absorption / 1.0, 4.0f));

				cloudColor /= 1.0 + cloudColor;

			return vec4(cloudColor, cloudAlpha);
		}
	}

	float phaseCloud(float x){
		x = facos(x);

		float xmpi2 = x - 3.14159265359;
		xmpi2 *= xmpi2;

		float x252 = x - 2.5;
		x252 *= x252;

		return
				 17.7 * exp(  -3.9 * x     )+
				1744. * exp(-1200. * x*x   )+
					.17 * exp(  -75. * x252  )+
					 .3 * exp(-4826. * xmpi2 )+
					 .2 * exp(  -50. * xmpi2 )+
					.15 * exp(   -1. * xmpi2 );
	}

	vec4 getClouds3D(vec3 color, vec3 p){

		vec4 clouds = vec4(0.1 * lightColor, 0.0);
		vec3 ambient = YxyToRGB(calculateZenithLuminanceYxy(turbidity, acos(mDot(sunVec, upVec)))) * lightColor * (1.0 - rainStrength * 0.9);
		float phase = phaseCloud(dot(normalize(p), lightVec));

		float nearPlane = 2.0;			//start to where the ray should march.
		float farPlane = far; 		//End from where the ray should march.

	    float increment = far / 10.0;

		float dither = bayer(gl_FragCoord.st);

		farPlane += dither * increment;

		vec3 worldPosition2 = toWorldSpace(p);

		while (farPlane > nearPlane){

			vec4 wpos = getVolumetricCloudPosition(texcoord.st, farPlane);

			vec4 result = getVolumetricCloudsColor(wpos.rgb, p, powf(ambient, CONTRAST) * BRIGHTNESS, phase);
				 result.a = clamp(result.a * VOLUMETRIC_CLOUDS_DENSITY, 0.0, 1.0);

			float volumetricDistance = length(wpos.xyz - cameraPosition.xyz);

			if (length(worldPosition2) < volumetricDistance ){
				result.a = 0.0;
			}

			//if (length(worldPosition) < volumetricDistance ){
				 //result.rgb = renderGaux2(result.rgb, normal2);
			//}

			clouds.rgb = mix(clouds.rgb, result.rgb, min(result.a * VOLUMETRIC_CLOUDS_DENSITY, 1.0));
			clouds.a += result.a * VOLUMETRIC_CLOUDS_DENSITY;

			farPlane -= increment;

		}

		return clouds;
	}
#endif

void main() {

	vec3 color = toLinear(texture2D(colortex0, texcoord.st).rgb);

	vec3 albedo = toGamma(color);

	vec3 fpos1 = toScreenSpace(texcoord.st, depth1);
  vec3 fpos2 = toScreenSpace(texcoord.st, depth2);


  doShading(color.rgb, fpos1, fpos2);

	vec3 vl = vec3(0.0);
	vec4 vc = vec4(0.0);

	#ifdef VOLUMETRIC_LIGHT
		vl = getVL();
	#endif

	#ifdef VOLUMETRIC_CLOUDS
		vc = getClouds3D(color, fpos2);
	#endif

	if (isnan(color)) color = vec3(0.0);

	//color.rgb = toLinear(texture2D(colortex5, texcoord.st).rgb);

/* DRAWBUFFERS:045 */

	gl_FragData[0] = vec4(toGamma(color.rgb), 1.0);
	gl_FragData[1] = vc;
  gl_FragData[2] = vec4(vl, encodeColor(albedo));
}
