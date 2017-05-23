#version 410 compatibility
#define composite2
#define fsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

const bool    colortex0MipmapEnabled = true;
const bool    colortex4MipmapEnabled = true;
const bool    colortex5MipmapEnabled = true;

varying vec4 texcoord;

uniform sampler2D colortex0;
uniform sampler2D colortex1;
uniform sampler2D colortex2;
uniform sampler2D colortex3;
uniform sampler2D colortex4;
uniform sampler2D colortex5;

uniform sampler2D depthtex0;
uniform sampler2D depthtex1;

uniform sampler2D noisetex;

uniform mat4 gbufferProjection;
uniform mat4 gbufferModelView;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelViewInverse;
uniform mat4 shadowModelViewInverse;

uniform vec3 cameraPosition;
uniform vec3 shadowLightPosition;
uniform vec3 upPosition;
uniform vec3 sunPosition;
uniform vec3 moonPosition;
uniform ivec2 eyeBrightnessSmooth;
uniform float frameTimeCounter;
uniform float rainStrength;
uniform float viewWidth;
uniform float viewHeight;
uniform float far;
uniform float near;
uniform int worldTime;
uniform int isEyeInWater;

#include "lib/encoding.glsl"
#include "lib/entity_ids.glsl"

#include "lib/basics.glsl"
#include "lib/time.glsl"
#include "lib/colors.glsl"

#include "lib/noise.glsl"
#include "lib/dither.glsl"

#include "lib/space_conversions.glsl"

#include "lib/sky.glsl"

float cdist(vec2 coord) {
	return max(abs(coord.s-0.5),abs(coord.t-0.5))*2.0;
}

float ld(float depth) {
    return (2.0 * near) / (far + near - depth * (far - near));
}

#define phaseMie(a) (.0348151 / pow(1.5625 - 1.5*a,1.5))
#define phaseRayleigh(a) (.059683104 * (1. + a * a))

void deferredRender(inout vec3 color) {
	vec2 bufferInfo = texture2D(colortex3, texcoord.st).rg;
	vec3 albedo = toLinear(decodeColor(bufferInfo.r));
	color = mix(color, color * albedo.rgb, pow(bufferInfo.y, 1.0 / 2.2));
}

void refractTexcoord(inout vec2 coord, in vec3 fpos1, in vec3 fpos2) {
	vec2 refractionNormals = decodeNormal(texture2D(colortex3, texcoord.st).b).xy;

	float metricDepth1 = -fpos1.z;
	float metricDepth2 = -fpos2.z;

	float depthFactor = (metricDepth2 - metricDepth1) * 0.1;
				depthFactor = depthFactor / (1.0 + depthFactor);
				depthFactor = depthFactor / (1.0 + metricDepth1) * 15.0;

	if (transparent > 0.5) coord.st += refractionNormals * depthFactor;
}

void doFog(inout vec3 color, in vec3 fpos) {
	float indoors = 1.0 - saturate((-eyeBrightnessSmooth.y + 230) / 100.0);
	float fogFactor  = length(fpos);
	float thickness  = getAtmosphere();
	if (isEyeInWater > 0.5) thickness = 0.00005;
				fogFactor *= thickness * indoors;
				fogFactor  = fogFactor / (1.0 + fogFactor * (1.0 + 100.0 * rainStrength));

	vec3 fogColor = js_totalScatter();

	if (isEyeInWater > 0.5) {
		fogColor *= waterColor;
		color		 *= powf(waterAbsorptionColor, fogFactor * 10000.0);
	}

	color += fogColor * fogFactor;
	//color = vec3(fogFactor);
}

vec3 getStars(vec3 color, vec3 fpos) {
	vec3 wpos			= normalize(toWorldSpace(fpos));
	float horizon = pow(mDot(normalize(fpos), upVec), 0.8);
	float sun			= smoothstep(0.04, 0.05, acos(mDot(moonVec, normalize(fpos))));

	vec3 starCoord  = wpos * (50.0 / (wpos.y));
			 starCoord *= mix(1.0, horizon, 1.0 - horizon);
	vec2 coord			= (starCoord.xz) * 0.006;

	float noise  = texture2D(noisetex, fract(coord.xy/2.0)).r;
				noise += texture2D(noisetex, fract(coord.xy)).r/2.0;

	float star = max(noise-1.3,0.0);

	return color + star * 9.8 * horizon * sun * moonFade;
}



float noise( in vec2 p ) {
    vec2 i = floor( p );
    vec2 f = fract( p );

    f = 1. - f*f;
    f = 1. - f*f;

    float a=hash12(i);
    float b=hash12(i + vec2(1.,0.) );
    float c=hash12(i + vec2(0.,1.) );
    float d=hash12(i + vec2(1.,1.) );

    return 2.*mix(
        mix( a, b, f.x),
        mix( c, d, f.x),
    f.y)-1.;
}

float wave(vec2 p) {
    p += noise2(p);
    return abs(noise(p));
}

float waveH(vec2 p) {
    float a = .04;
    float t = frameTimeCounter * .0;
    p = (p * rotate(1.0));

    float h = (wave(p+t)+wave(p-t)) * a;
    p *= mat2(3.2,1.2,-1.2,3.2);
    a *= .2;
    h += (wave(p+t)+wave(p-t)) * a;
    p *= mat2(3.2,1.2,-1.2,3.2);
    a *= .2;
    h += (wave(p+t)+wave(p-t)) * a;

    return (h) * 1.0;
}

vec3 getGalaxy(vec3 color, vec3 fpos) {
	vec3 wpos = normalize(toWorldSpace(fpos));
	vec3 axisVector = vec3(0.2,0.5,-0.8);

	float factor = (1.0 - (mDot(wpos, normalize(axisVector)) + max(-dot(wpos, normalize(axisVector)), 0.0)));
	float horizon = pow(mDot(normalize(fpos), upVec), 1.0);

	vec3 coord  = wpos * (50.0 / (wpos.y));
			 coord *= mix(1.0, horizon, 1.0 - horizon);

	vec3 noise  = vec3(pow(1.0 - waveH(coord.xz * 0.2), 5.0)) * 0.2;
			 noise += pow(waveH(coord.xz * 0.05), 1.0) * 2.0;
			 noise += pow(waveH(coord.xz * 0.01), 1.0) * 2.0;

	noise = max(noise * vec3(1.0, 0.2, 0.8) - 0.3,0.0);

	return color + noise * pow(factor, 10.0) * horizon * 0.05 * moonFade;
}

#ifdef CLOUDS
	vec4 normalNoise(vec2 x) {
		vec2 p = floor(x);
		vec2 f = fract(x);

		float a = hash12(p);
		float b = hash12(p + vec2(1., 0.));
		float c = hash12(p + vec2(0., 1.));
		float d = hash12(p + vec2(1., 1.));


		vec2 g = f - 1.;
		g *= g;

		vec2 v = 1. - g;
		v *= v;

		float noise = mix(mix(a, b, v.x), mix(c, d, v.x), v.y);

		vec2 z = 75.*g*f*f;

		vec2 derivatives = z * (vec2(b, c) + (a - b - c + d) * v.yx - a);

		vec3 normal = cross(vec3(15,0,derivatives.x),vec3(0,1,derivatives.y));

		return vec4(normal, noise);
	}

	float phaseCloud(float x){
		x = facos(x);

		float xmpi2 = x - 3.14159265359;
		xmpi2 *= xmpi2;

		float x252 = x - 14.5;
		x252 *= x252;

		return
				 17.7 * exp(  -3.9 * x     )+
				1744. * exp(-1200. * x*x   )+
					.17 * exp(  -75. * x252  )+
					 .3 * exp(-4826. * xmpi2 )+
					 .2 * exp(  -50. * xmpi2 )+
					.15 * exp(   -1. * xmpi2 );
	}

	vec3 getClouds(vec3 color, vec3 fpos) {
		float cloudHeight = 400.0 + bayer(gl_FragCoord.st) * 30.0;
		vec3 wpos = normalize(toWorldSpace(fpos));

		vec3 cloudPosition = wpos * ((cloudHeight) / wpos.y - wpos.y);
		vec2 coord = cloudPosition.xz * 0.001;

		float horizon  = pow(mDot(upVec, normalize(fpos)), 0.8);
		float phase    = phaseCloud(dot(lightVec, normalize(fpos)));
		float aBeer		 = pow(1.0 + dot(downVec, normalize(fpos)), 2.0);
		float movement = frameTimeCounter * 0.03;
		float coverage = mix(0.39 * (2.0 - pow(horizon, 0.1)), 0.0, rainStrength);

		vec4 noise;
		noise		= normalNoise(coord * vec2(2.3, 1.8) + movement);
		coord	 *= mat2(3.2,0.2,-0.2,2.0);
		noise  += normalNoise(coord * vec2(2.5, 2.3) + movement)*0.9;
		coord  *= mat2(1.0,0.2,-0.2,2.0);
		noise  += normalNoise(coord * vec2(0.3, 0.5) + movement)*0.5;
		coord  *= mat2(3.0,1.2,-1.2,3.0);
		noise  += normalNoise(coord * vec2(0.6, 1.0) + movement)*0.25;
		coord  *= mat2(1.2,0.7,-0.7,1.2)*1.1;
		noise  += normalNoise(coord * vec2(1.5, 2.0) + movement)*0.125;
		coord  *= mat2(3.0,0.5,-0.5,3.0);
		noise  += normalNoise(coord * vec2(1.5, 2.1) + movement)*.0625;
		coord  *= mat2(0.5,0.2,-0.2,0.5);
		noise  += normalNoise(coord * vec2(0.1, 0.2) + movement)*.13125;
		coord  *= mat2(4.0,0.3,-0.3,4.0);
		noise  += normalNoise(coord * vec2(3.4, 3.0) + movement)*.015625;
	  coord  *= mat2(0.2,4.2,-4.2,0.2);
		noise  += normalNoise(coord * vec2(0.2, 0.1) + movement)*.038125;
		noise  += normalNoise(coord * vec2(3.4, 0.8) + movement)*.031125;


		noise.a  *= 0.25;
		noise.xyz = normalize(noise.xyz);

		noise.a = saturate(noise.a - coverage);

		vec4 sunPos = normalize(gbufferModelViewInverse * vec4(shadowLightPosition, 0.0));

		float cloudsSun	 = mDot(noise.rgb, sunPos.rgb);

		vec3 cloudColor = lightColor * 11.0 * (0.8 + phase * 0.6);

		vec3 clouds = mix(vec3(0.007, 0.01, 0.012) * (lightColor) * 6.6, cloudColor, pow(cloudsSun, 0.8) * pow(1.0 - noise.a, 30.0));

		return mix(color, clouds, saturate(smoothstep(0.,1.25,noise.a*4.) * pow(cloudsSun, 0.8) * horizon * 3.8));
	}
#endif

#ifdef REFLECTIONS
	float G(float NoV, float NoL, float alpha) {
		float a=alpha;
		float GV = (2.*NoV)/(NoV+sqrt(NoV*NoV+a*a*(1.-NoV*NoV)   ));
		float GL = (2.*NoL)/(NoL+sqrt(NoL*NoL+a*a*(1.-NoL*NoL)   ));
		return GV*GL;
	}

	vec3 MakeSample(vec2 p, float alpha, float randomValue) {
		float x = (alpha*p.x) / (1.-p.x);
		float y = (p.y+randomValue) * tau;
		vec2 c=vec2(
			inversesqrt(x+1.),
			cos(y)
		);
		vec2 s=vec2(
			sqrt(x) * inversesqrt(x+1.),
			sin(y)
		);
		return vec3(c.y*s.x, s.y*s.x, c.x);
	}

	vec3 rayTrace(vec3 dir, vec3 position, vec3 clipPosition, float skyLightmap){
    vec3 direction = normalize(fromScreenSpace(position+dir)-clipPosition);  //convert to clip space
    direction.xy = normalize(direction.xy);
    //get at which length the ray intersects with the edge of the screen
    vec3 maxLengths = (step(0.,direction)-clipPosition) / direction;
    float mult = min(min(maxLengths.x,maxLengths.y),maxLengths.z);
    vec3 rayStep = direction * mult / R_QUALITY;

    vec3 position0 = clipPosition;
    vec3 position1;
    float depth0 = clipPosition.z;
    float depth1;

		float dither = hash12(gl_FragCoord.st) * 0.0;

    for (int i = 1; i < int(R_QUALITY+1); i++) {
      position1 = position0;
      position0 = clipPosition + rayStep * ( float(i) + dither);
      depth1 = depth0;
      depth0 = texture2D(depthtex0,position0.xy).x;

      if( depth0 < position0.z&&i>1)
          break;
    }

    float delta0 = depth0 - position0.z;
    float delta1 = depth1 - position1.z;
    float weight = delta0 / (sign(direction.z)*(delta0 - delta1));

    vec3 position2 = mix(position0,position1,weight);
    float rejectSample = texture2D(depthtex0,position2.xy).x;

    if(
        	 abs(position2.z-rejectSample) < abs(rayStep.z) + .002 / R_QUALITY //not backface
        && abs(position0.z-depth0) < abs(rayStep.z*5.) + .002 / R_QUALITY //not backface
        && rejectSample < 1.//not sky
        && abs(position2.x-0.5)<0.5&&abs(position2.y-0.5)<0.5//not outside viewport
        && depth0 < position0.z
    )
        return toLinear(texture2DLod(colortex0, position2.xy, 0.0).rgb);

		vec3 color = vec3(0.0);

		if (isEyeInWater < 0.5) {
			#if SKY_MODEL == 1
			color = js_getScatter(color, dir);
			#endif

			#if SKY_MODEL == 2
				color = CalculateAtmosphericSky(toWorldSpace(dir), land) * 0.1;
			#endif

			//color = getStars(color, dir);
			#ifdef CLOUDS
				color = getClouds(color, dir);
			#endif
		}

		return color * skyLightmap;
	}

	vec3 jssr(vec2 p, vec3 n, float skyLightmap, float roughness) {
		vec3 clip3 = vec3(p, depth1);
		vec3 p3 = toScreenSpace(p, depth1);
		vec3 v = -normalize(p3);

		#ifdef ROUGH_REFLECTIONS
			float alpha = roughness * roughness;
			float randomValue = hash12(texcoord.st * vec2(viewWidth, viewHeight));


			int x = int(p.x*viewWidth)  % 4;
			int y = int(p.y*viewHeight) % 4;
			int index = x*4+y;
			vec3 tanX = normalize(cross(upVec, n)); //normalize((gbufferModelView*vec4(0,1,0,0)).xyz)
			vec3 tanY = cross(n, tanX);

			float NoV = clamp(dot(n, v),0.,1.);

			vec3 color = vec3(0.0);
			const int steps = R_STEPS;
			for (int i = 0; i < steps; i++){
					vec3 hSample = MakeSample(hammersley(i*15+index, 16*steps), alpha, randomValue);
					vec3 h  = normalize(hSample.x * tanX + hSample.y * tanY + hSample.z * n);
					vec3 l  = normalize(-reflect(v, h));

					float NoL = clamp(dot(n, l),0.,1.);
					float NoH = clamp(dot(n, h),0.,1.);
					float VoH = clamp(dot(v, h),0.,1.);

					float F0 = mix(0.02, 0.05, water);
					float fresnel = F0 + (1. - F0) * pow(1.-VoH,5.);

					color +=  (fresnel) * G(NoV, NoL, alpha) * rayTrace(l, p3, clip3, skyLightmap)  * VoH / (NoH * NoV);
			}

			return color / float(steps);
		#else
			vec3 r = reflect(normalize(p3), n);
			vec3 h = normalize(r + v);
			float VoH = clamp(dot(v, h),0.,1.);

			float F0 = mix(0.02, 0.05, water);
			float fresnel = F0 + (1. - F0) * pow(1.-VoH,5.);

			return rayTrace(r, p3, skyLightmap) * fresnel;
		#endif
	}

	#include "lib/PBR/fresnel.glsl"

	void reflections(inout vec3 color, in vec3 fpos) {
		if (land > 0.5 || transparent > 0.5) {
			vec3 combinedNormals = normal1;
			if(transparent > 0.5) combinedNormals = normal2;

			vec3 specular = decodeColor(aux1.a);
			float glossy    = mix(specular.r * 0.1 * (1.0 - emitter), 0.8, transparent);
			float metal   = specular.g;
			float roughness = specular.r * (1.0 - transparent) * 0.1;

			vec3 V = normalize(-fpos);
			vec3 R = reflect(normalize(fpos), combinedNormals);
			vec3 H = normalize(R + V);

			float LoH	= mDot(R, H);

			float fresnel = Schlick(0.05, LoH);
			float skyLightmap = smoothstep(0.0, 0.5, pow(mix(lightmaps1.y, lightmaps2.y, transparent), 8.0));

			float reflectedLand = 0.0;
			vec3 reflections = jssr(texcoord.st, combinedNormals, skyLightmap, roughness);

			if (isnan(reflections))		reflections		= vec3(0.0);
			if (isinf(reflections.r)) reflections.r = 1.0;
			if (isinf(reflections.g)) reflections.g = 1.0;
			if (isinf(reflections.b)) reflections.b = 1.0;

			vec3 reflectionsG = reflections * glossy;
			vec3 reflectionsM = reflections * metal * toLinear(decodeColor(texture2D(colortex5, texcoord.st).a));

			color += reflectionsG;
		}
	}
#endif

#ifdef VOLUMETRIC_CLOUDS
	void getVC(inout vec3 color, in vec3 fpos){

		vec4 clouds = vec4(0.0);
		float totalWeight = 0.0;

		float radius = 4.0;

		for (float i = -1.0; i < 1.0; i++){
			for (float j = -1.0; j < 1.0; j++){

				vec2 offset = (vec2(i,j) + 0.5) / vec2(viewWidth, viewHeight) * radius;

				float vcDepth = texture2D(depthtex1, texcoord.st + offset).x;

				float weight = pow(1.0 - abs(depth2 - vcDepth) * 10.0, 32.0);
					weight = max(0.1e-8, weight);

				clouds += texture2DLod(colortex4, texcoord.xy + offset, 1.0) * weight;

				totalWeight += weight;
			}
		}

		clouds /= totalWeight;

		clouds = texture2DLod(colortex4, texcoord.xy, 1.75);

		color = mix(color, clouds.rgb, saturate(clouds.a * 1.0));
	}
#endif

#ifdef VOLUMETRIC_LIGHT
	void getVL(inout vec3 color, in vec3 fpos){

    vec3 volumetricLightSample = vec3(0.0);
		float volumetricFogSample = 0.0;
		float totalWeight = 0.0;

		const float radius = 4.0;

		for (float i = -1.0; i < 1.0; i++){
			for (float j = -1.0; j < 1.0; j++){

				vec2 offset = (vec2(i,j) + 0.5) / vec2(viewWidth, viewHeight) * radius;

				float vlDepth = texture2D(depthtex1, texcoord.st + offset).x;

				float weight = pow(1.0 - abs(depth1 - vlDepth) * 10.0, 32.0);
					weight = max(0.1e-8, weight);

        volumetricLightSample += texture2DLod(colortex5, texcoord.xy + offset, 2.0).rgb * 100.0 * weight;

				totalWeight += weight;
			}
		}

    volumetricLightSample /= totalWeight;

		float LdotF			= dot(lightVec, normalize(fpos));
		float phase			= phaseMie(LdotF) + phaseRayleigh(LdotF);
		float thickness = getAtmosphere() * 20.0 * (1.0 + rainStrength * 10.0);

	  color.rgb += volumetricLightSample * phase * lightColor * thickness;
	}
#endif

void main() {

	vec3 fpos1 = toScreenSpace(texcoord.st, depth1);
	vec3 fpos2 = toScreenSpace(texcoord.st, depth2);

	vec2 refractedTC = texcoord.st;
	refractTexcoord(refractedTC.st, fpos1, fpos2);

	depth2 = texture2D(depthtex1, refractedTC.st).x;
	fpos2  = toScreenSpace(refractedTC.st, depth2);
	land   = float(pow(depth2, 2.0) < pow(depth2, 1.0));


	vec3 color = toLinear(texture2D(colortex0, refractedTC.st).rgb);
	if (water > 0.5) {
		vec2 refraction = vec2(0.0);
		refractTexcoord(refraction, fpos1, fpos2);
		color.r = pow(texture2D(colortex0, texcoord.st + refraction * 1.0).r, 2.2);
		color.g = pow(texture2D(colortex0, texcoord.st + refraction * 1.2).g, 2.2);
		color.b = pow(texture2D(colortex0, texcoord.st + refraction * 1.4).b, 2.2);
	}
	if (isnan(color)) toLinear(texture2D(colortex0, texcoord.st).rgb);

	if (land < 0.5) {
		#if SKY_MODEL == 1
			color = getStars(color, fpos2);
			color = getGalaxy(color, fpos2);
			color = js_getScatter(color, normalize(fpos2));
		#endif

		#if SKY_MODEL == 2
			color = CalculateAtmosphericSky(toWorldSpace(fpos2), land) * 0.1;
			color = getStars(color, fpos2);
			color = getGalaxy(color, fpos2);
		#endif

		#ifdef CLOUDS
			color = getClouds(color, fpos2);
		#endif
	}

	deferredRender(color);

	#ifdef REFLECTIONS
		reflections(color.rgb, fpos1);
	#endif

	doFog(color.rgb, fpos1);

	#ifdef VOLUMETRIC_CLOUDS
    getVC(color.rgb, fpos2);
  #endif

  #ifdef VOLUMETRIC_LIGHT
    getVL(color.rgb, fpos2);
  #endif

	if (isnan(color)) color = vec3(0.0);

/* DRAWBUFFERS:0 */

  gl_FragData[0] = vec4(toGamma(color.rgb), 1.0);
}
