#version 410 compatibility
#define gbuffers_water
#define fsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

uniform sampler2D texture;
uniform sampler2D normals;

varying vec4 texcoord;
varying vec4 lightmapCoord;
varying vec4 color;

varying vec4 verts;
varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;
varying mat3 tbnMatrix;

varying vec3 wpos;

varying float material;

uniform mat4 gbufferModelViewInverse;

uniform float frameTimeCounter;

#include "lib/encoding.glsl"
#include "lib/entity_ids.glsl"

#include "lib/noise.glsl"
#include "lib/water.glsl"

#include "lib/dither.glsl"

vec3 waterNormals(vec2 posxz, float material){

	float wavesCenter = waveH(posxz, material);
	float wavesLeft = waveH(posxz + vec2(0.3, 0.0f), material);
	float wavesUp   = waveH(posxz + vec2(0.0f, 0.3), material);

	vec3 wavesNormal;
	     wavesNormal.r = (wavesCenter - wavesLeft)*10;
       wavesNormal.g = (wavesCenter - wavesUp)*10;
       wavesNormal.b = 1.0;

	return normalize(wavesNormal);
}

vec2 calcParallax(vec2 pos, vec2 VertexModelView) {
  for (int i = 0; i < 8; i++) {
    pos += VertexModelView * waveH(pos, material) * 10.0;
  }
  return pos;
}

vec3 normalToWorldSpace(vec3 viewSpace) {
	vec4 fpos = vec4(viewSpace, 0.0);
	vec4 wpos = gbufferModelViewInverse * fpos;
	return wpos.xyz;
}

void main() {

  vec4 albedo = texture2D(texture, texcoord.st) * color;

	vec3 bump = texture2D(normals, texcoord.st).rgb * 2.0 - 1.0;

	vec4 modelView = normalize(gl_ModelViewMatrix * verts);
	vec3 tangentVector = normalize(tbnMatrix * modelView.xyz);

	vec2 posxz = wpos.xz - wpos.y;

	#ifdef WATER_PARALLAX
		posxz = calcParallax(posxz, tangentVector.xy);
	#endif

	if (material > 0.5) albedo = vec4(1.0, 3.0, 1.0, 1.0);

	bump = waterNormals(posxz, material);

	vec3 normalTangentSpace = normalToWorldSpace(normalize(bump * tbnMatrix));

/* DRAWBUFFERS:32 */

  gl_FragData[0] = vec4(encodeColor(albedo.rgb), albedo.a, encodeNormal(bump), 1.0);
  gl_FragData[1] = vec4(encodeNormal(normalTangentSpace), encodeLightMap(lightmapCoord.xy), encodeMatIDs(material), 1.0);

}
