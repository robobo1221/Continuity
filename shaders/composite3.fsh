#version 410 compatibility
#define composite3
#define fsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

const bool    colortex0MipmapEnabled = true;

varying vec4 texcoord;

uniform sampler2D colortex0;

uniform sampler2D depthtex1;

uniform sampler2D noisetex;

uniform mat4 gbufferProjection;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelViewInverse;
uniform mat4 gbufferPreviousModelView;
uniform mat4 gbufferPreviousProjection;

uniform vec3 cameraPosition;
uniform vec3 previousCameraPosition;

uniform float aspectRatio;
uniform float viewWidth;
uniform float viewHeight;

uniform int isEyeInWater;

float pw = 1.0/ viewWidth;

vec2 scaleTex(vec2 pos, float scale) {
	vec2 ghostPos = -pos + vec2(1.0);
	vec2 ghost1 = (vec2(0.5) - ghostPos)*scale+vec2(0.5);
	return ghost1;
}

float distratio(vec2 pos, vec2 pos2) {
	float xvect = pos.x-pos2.x;
	float yvect = pos.y-pos2.y;

	return sqrt(xvect*xvect + yvect*yvect);
}

// TOTAL 5 trigonometric calls
// http://glslsandbox.com/e#35413.0
vec3 bokehImage(float lod, vec2 offset) {
	float r = 0.0;
	float a = 2.0 * PI * (1.0 / BLADES);
	float cs = cos(a);
	mat2 rmat = mat2(cs, -sin(a), sin(a), cs);

	vec2 coord = (texcoord.st - offset) * pow(2.0, lod);

	vec3 size 	 = vec3(0.4);
			 size.r *= (1.0 - 1.0 * LENS_SHIFT_AMOUNT);
			 size.g *= (1.0 - 0.5 * LENS_SHIFT_AMOUNT);
	float softness = 0.9;

	vec2 lp = vec2(0, -1); //swap to remove 2 additional sin cos and go with 3 trigs
	vec2 addv = lp * rotate(ROTATION);

	for(int i = 0; i < BLADES; i++){
		addv = rmat * addv;
		lp += addv;
		r = max(r, dot(coord - 0.5, normalize(addv)));
	}

	r = mix(r, distratio(coord, vec2(0.5)) * 0.8, ROUNDING);

	vec3 bokeh;
	bokeh = saturate(1.0 - smoothstep(size * softness, size, vec3(r)));
	bokeh = saturate(bokeh) * (1.0 - saturate(smoothstep(size * 0.99, size * softness * 0.5, vec3(r)) * BIAS));

	bokeh *= mix(1.0, 0.2 + (1.0 + texture2D(noisetex, coord * 0.5).x) / 2.0, GRUNGE);

	return bokeh;
}

#ifdef BLOOM
	vec3 bloomTile(float lod, vec2 offset) {
		vec3 bloom  = vec3(0.0);
		float total = 0.0;

		float scale = pow(2.0, lod);
		vec2 coord  = (texcoord.st - offset) * scale;

		if (coord.s > -0.1 && coord.t > -0.1 && coord.s < 1.1 && coord.t < 1.1){
			for (int i = -4; i < 4; i++) {
				for (int j = -4; j < 4; j++) {
					float weight = pow(max(1.0 - length(vec2(i, j)) / 4.0, 0.0), 5.0) * 10.0;
					vec2 bcoord = (texcoord.st - offset + vec2(i, j) * pw * vec2(4.0, aspectRatio)) * scale;

					if (weight > 0) bloom += toLinear(texture2D(colortex0, bcoord).rgb) * weight;
					total += weight;
				}
			}
			bloom /= total;
		}
		return bloom;
	}
#endif

#ifdef DYN_LENS
	vec3 lensTile(float lod, vec2 offset, float dist){
		vec3 lens   = vec3(0.0);
		float total = 0.0;

		float scale = pow(2.0, lod);
		vec2 coord  = scaleTex((texcoord.st - offset) * scale, dist);

		if (coord.x > -0.0 && coord.y > -0.0 && coord.x < 1.0 && coord.y < 1.0) {
			for (int i = -4; i < 4; i++) {
				for (int j = -4; j < 4; j++) {
					float weight = pow(max(1.0 - length(vec2(i, j)) / 2.0, 0.0), 0.3) * 5.0;
					vec2 bcoord  = scaleTex((texcoord.st - offset + vec2(i, j) * pw * vec2(1.0, aspectRatio)) * scale, dist);

					if (weight > 0) {
						lens  += toLinear(max(texture2D(colortex0, bcoord).rgb - 0.0, 0.0)) * weight;
						total += weight;
					}
				}
			}
			lens /= total;
		}
		return lens;
	}

	vec3 anaTile(float lod, vec2 offset, float dist) {
		vec3 streak = vec3(0.0);
		float total = 0.0;

		float scale = pow(2.0, lod);
		vec2 coord  = scaleTex((texcoord.st - offset) * scale, dist);
		const float res   = 0.001;

		if (coord.x > -0.1 && coord.y > -0.1 && coord.x < 1.1 && coord.y < 1.1) {
			for (int i = 0;  i < 80; i++) {
				float weight = pow(max(1.0 - length(vec2(i - 40, 0.0)) / 40.0, 0.0), 4.0) * 0.125;
				vec2 bcoord = scaleTex((texcoord.st - offset + vec2(i - 40, 0.0) * res) * scale, dist);
				streak += toLinear(max(texture2D(colortex0, bcoord).rgb - 0.0, 0.0)) * weight;
			}
		}
		return streak;
	}
#endif

#ifdef MOTIONBLUR
	vec3 getMotionblur(vec3 color, vec4 previousPosition, vec4 currentPosition) {

		vec4 aux2 = texture2D(depthtex1, texcoord.st);
		float hand = 0.0;

		if (isEyeInWater > 0.9) {
			} else if (hand > 0.9) {
				} else {
					#ifdef LQ_MOTIONBLUR
						vec2 velocity = (currentPosition.xyz - previousPosition.xyz).st * (0.05*MOTIONBLUR_AMOUNT);
					#endif
					#ifdef HQ_MOTIONBLUR
						vec2 velocity = (currentPosition - previousPosition).st * (0.01*MOTIONBLUR_AMOUNT);
					#endif

					int samples = 1;

					vec2 coord = texcoord.st + velocity;

					coord = clamp(coord, 1.0 / vec2(viewWidth, viewHeight), 1.0 - 1.0 / vec2(viewWidth, viewHeight));
					//float dither = find_closest(texcoord.st,1.0);
					if (coord.s < 1.0 || coord.t < 1.0 || coord.s > 0.0 || coord.t > 0.0) {
						#ifdef HQ_MOTIONBLUR
							for (int i = 0; i < 16; ++i, coord += velocity) {
								if (coord.s > 1.0 || coord.t > 1.0 || coord.s < 0.0 || coord.t < 0.0) {
									break;
								}
									color += toLinear(texture2D(colortex0, coord).rgb);
									++samples;
							}
							#endif
								#ifdef LQ_MOTIONBLUR
								for (int i = 0; i < 4; ++i, coord += velocity) {
								if (coord.s > 1.0 || coord.t > 1.0 || coord.s < 0.0 || coord.t < 0.0) {
									break;
								}
									color += toLinear(texture2D(colortex0, coord).rgb);
									++samples;
							}
						#endif
					}

					color = (color/1.0)/samples;
				}
		return color;
	}
#endif

void main() {

	vec4 currentPosition = vec4(texcoord.x * 2.0 - 1.0, texcoord.y * 2.0 - 1.0, 2.0 * texture2D(depthtex1, texcoord.st).x - 1.0, 1.0);

	vec4 fragposition = gbufferProjectionInverse * currentPosition;
		 fragposition = gbufferModelViewInverse * fragposition;
		 fragposition /= fragposition.w;
		 fragposition.xyz += cameraPosition;

	vec4 previousPosition = fragposition;
		 previousPosition.xyz -= previousCameraPosition;
		 previousPosition = gbufferPreviousModelView * previousPosition;
		 previousPosition = gbufferPreviousProjection * previousPosition;
		 previousPosition /= previousPosition.w;

	vec3 color = toLinear(texture2D(colortex0, texcoord.st).rgb);


	vec3 bokeh = bokehImage(2.0, vec2(0.5, 0.0));

	vec3 lens = vec3(0.0);
	#ifdef BLOOM
		lens += powf(bloomTile(2.0, vec2(0.0, 0.0)), 1.0) * 1.0;
		lens += powf(bloomTile(3.0, vec2(0.3, 0.0)), 1.0) * 1.0;
		lens += powf(bloomTile(4.0, vec2(0.0, 0.3)), 1.0) * 1.0;
		lens += powf(bloomTile(5.0, vec2(0.1, 0.3)), 1.0) * 1.0;
		lens += powf(bloomTile(6.0, vec2(0.2, 0.3)), 1.0) * 1.0;
		lens += powf(bloomTile(7.0, vec2(0.3, 0.3)), 1.0) * 1.0;
	#endif

	#ifdef DYN_LENS
		lens += lensTile(4.0, vec2(0.0,0.5), -2.0);
		lens += lensTile(4.0, vec2(0.2,0.5), -1.0);
		lens += anaTile(4.0, vec2(0.4,0.5), -0.7);
		lens += anaTile(4.0, vec2(0.6,0.5), 1.0);
	#endif

	lens += bokeh;

	#ifdef MOTIONBLUR
		color = getMotionblur(color, previousPosition, currentPosition);
	#endif


/* DRAWBUFFERS:05 */

	gl_FragData[0] = vec4(toGamma(color.rgb), 1.0);
	gl_FragData[1] = vec4(toGamma(lens.rgb) * 0.1, 1.0);
}
