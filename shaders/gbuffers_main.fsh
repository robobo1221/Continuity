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

#include "lib/encoding.glsl"
#include "lib/entity_ids.glsl"

void main() {

  vec4 albedo = texture2D(texture, texcoord.st) * color;

	vec3 bump = texture2D(normals, texcoord.st).rgb * 2.0 - 1.0;

  vec3 specular = texture2D(specular, texcoord.st).rgb;

	vec3 normalTangentSpace = normalize(bump * tbnMatrix);

/* DRAWBUFFERS:014 */

  gl_FragData[0] = albedo;
  gl_FragData[1] = vec4(encodeNormal(normalTangentSpace), encodeLightMap(lightmapCoord.xy), encodeMatIDs(material), 1.0);
  gl_FragData[2] = vec4(specular, 1.0);

}
