#version 410 compatibility
#define gbuffers_water
#define vsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

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
uniform vec3 cameraPosition;

attribute vec4 mc_Entity;
attribute vec4 at_tangent;

#include "lib/entity_ids.glsl"

void main() {

	material = 0.0;

	gl_Position = ftransform();

	texcoord = gl_MultiTexCoord0;

  lightmapCoord = gl_TextureMatrix[1] * gl_MultiTexCoord1;

  color = gl_Color;

	verts = gl_Vertex;

	vec4 position = gl_ModelViewMatrix * gl_Vertex;
	vec4 viewpos = gbufferModelViewInverse * position;
	wpos = viewpos.xyz + cameraPosition;

	tangent = vec3(0.0);
	binormal = vec3(0.0);
	normal = normalize(gl_NormalMatrix * gl_Normal);

		if (gl_Normal.x > 0.5) {
		tangent  = normalize(gl_NormalMatrix * vec3( 0.0,  0.0, -1.0));
		binormal = normalize(gl_NormalMatrix * vec3( 0.0, -1.0,  0.0));
	} else if (gl_Normal.x < -0.5) {
		tangent  = normalize(gl_NormalMatrix * vec3( 0.0,  0.0,  1.0));
		binormal = normalize(gl_NormalMatrix * vec3( 0.0, -1.0,  0.0));
	} else if (gl_Normal.y > 0.5) {
		//  0.0,  1.0,  0.0
		tangent  = normalize(gl_NormalMatrix * vec3( 1.0,  0.0,  0.0));
		binormal = normalize(gl_NormalMatrix * vec3( 0.0,  0.0,  1.0));
	} else if (gl_Normal.y < -0.5) {
		//  0.0, -1.0,  0.0
		tangent  = normalize(gl_NormalMatrix * vec3( 1.0,  0.0,  0.0));
		binormal = normalize(gl_NormalMatrix * vec3( 0.0,  0.0,  -1.0));
	} else if (gl_Normal.z > 0.5) {
		tangent  = normalize(gl_NormalMatrix * vec3( 1.0,  0.0,  0.0));
		binormal = normalize(gl_NormalMatrix * vec3( 0.0, -1.0,  0.0));
	} else if (gl_Normal.z < -0.5) {
		tangent  = normalize(gl_NormalMatrix * vec3( -1.0,  0.0,  0.0));
		binormal = normalize(gl_NormalMatrix * vec3( 0.0, -1.0,  0.0));
	}


	tbnMatrix = mat3(tangent.x, binormal.x, normal.x,
									  tangent.y, binormal.y, normal.y,
							     	tangent.z, binormal.z, normal.z);

  float id = mc_Entity.x;

  // water
  if (id == WATER_FLOWING || id == WATER_STILL) material = 1.0;
/*
  // translucent
  if (id == LEAVES || id == VINES || id == TALLGRASS || id == DANDELION || id == ROSE ||
      id == WHEAT || id == LILYPAD || id == LEAVES2 || id == NEWFLOWERS || id == NETHER_WART ||
      id == DEAD_BUSH || id == CARROT || id == POTATO || id == COBWEB || id == SUGAR_CANE ||
      id == BROWN_SHROOM || id == RED_SHROOM) material = 2.0;

  // emitter
  if (id == TORCH || id == FIRE || id == LAVAFLOWING || id == LAVASTILL || id == GLOWSTONE ||
      id == SEA_LANTERN || id == LAMP_ON || id == BEACON) material = 3.0;
*/
}
