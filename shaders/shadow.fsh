#version 410 compatibility
#define gbuffers_shadow
#define fsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

varying vec4 texcoord;
varying vec4 color;
varying vec3 normal;
varying vec3 worldPosition;
varying float water;

uniform sampler2D tex;

uniform float frameTimeCounter;

#include "lib/noise.glsl"
#include "lib/water.glsl"

void main() {

	vec4 fragcolor = texture2D(tex, texcoord.st) * color;

	if (water > 0.5) {
		fragcolor      = vec4(1.0, 1.0, 1.0, 0.5);
		fragcolor.rgb *= mix(pow(waveH(worldPosition.xz * 3.0, 1.0) * 20.0, 4.0) * 1.0, 1.6, 0.2);
	}

/* DRAWBUFFERS:01 */

	gl_FragData[0] = fragcolor;
	gl_FragData[1] = vec4(normal * 0.5 + 0.5, 1.0);
}
