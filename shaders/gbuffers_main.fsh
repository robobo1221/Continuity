#include "lib/utility.glsl"
#include "lib/settings.glsl"

uniform sampler2D texture;
uniform sampler2D normals;
uniform sampler2D specular;

varying vec4 texcoord;
varying vec4 lightmapCoord;
varying vec4 color;

varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;
varying mat3 tbnMatrix;

varying float material;

uniform mat4 gbufferProjection;

uniform int terrainIconSize;

#if defined gbuffers_terrain && defined POM
	varying vec3 modelView;
  varying vec3 p3;
	uniform ivec2 atlasSize;
#endif

#include "lib/encoding.glsl"
#include "lib/entity_ids.glsl"

float iconsize = 2.*float(terrainIconSize);
vec2 u = floor(texcoord.st*iconsize);

vec4 textureWrap(sampler2D t, vec2 v){
	v = (u+fract(v*iconsize))/iconsize;
	return texture2D(t,v);
}

#if defined gbuffers_terrain && defined POM
  #include "lib/dither.glsl"

	vec4 textureSmooth(sampler2D t, vec2 x) {
		x*=vec2(atlasSize);
		vec2 p = floor(x);
	  vec2 f = fract(x);

		vec4 a = textureWrap(t,  (p               )/vec2(atlasSize)  );
	  vec4 b = textureWrap(t,  (p + vec2(1., 0.))/vec2(atlasSize)  );
	  vec4 c = textureWrap(t,  (p + vec2(0., 1.))/vec2(atlasSize)  );
	  vec4 d = textureWrap(t,  (p + vec2(1., 1.))/vec2(atlasSize)  );

		return mix(mix(a, b, f.x), mix(c, d, f.x), f.y);
	}

  vec2 cheapPOM(vec2 p,vec3 dir) {
    vec2 stepW = dir.xy * .3 / POM_MAP_RES;
    p += stepW * ( 1. - textureWrap(normals,p).w );
    p += stepW * ( 1. - textureWrap(normals,p).w );
    p += stepW * ( 1. - textureWrap(normals,p).w );
    return p;
  }
#endif

vec3 toClipSpace(vec3 viewSpace) {
	vec4 screenspace = gbufferProjection * vec4(viewSpace, 1.0);
	screenspace.xyz /= screenspace.w;
	return screenspace.xyz * 0.5f + 0.5f;
}

void main() {

  vec2 coord = texcoord.st;
  #if defined gbuffers_terrain && defined POM
    vec3 viewVector = normalize(tbnMatrix * modelView);
    coord = cheapPOM(coord.st, viewVector);
  #endif

  vec4 albedo = textureWrap(texture, coord) * color;
	vec4 bump = textureWrap(normals, coord);
  vec3 specular = textureWrap(specular, coord).rgb;

	vec3 normalTangentSpace = normalize((bump.xyz * 2.0 - 1.0) * tbnMatrix);

  #if defined gbuffers_terrain && defined POM
    vec3 pompos = p3 - (vec3(0.0, 0.0, (1.0 - bump.a) * 0.3) * tbnMatrix);
		float depth = toClipSpace(pompos).z;
  #endif

/* DRAWBUFFERS:014 */

  gl_FragData[0] = albedo;
  gl_FragData[1] = vec4(encodeNormal(normalTangentSpace), encodeLightMap(lightmapCoord.xy), encodeMatIDs(material), 1.0);
  gl_FragData[2] = vec4(specular, 1.0);
  #if defined gbuffers_terrain && defined POM
    gl_FragDepth = depth;
  #endif

}
