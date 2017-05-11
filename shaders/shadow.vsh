#version 410 compatibility
#define gbuffers_shadow
#define vsh
#include "lib/utility.glsl"
#include "lib/settings.glsl"

varying vec4 texcoord;
varying vec4 color;
varying vec3 normal;
varying vec3 worldPosition;
varying float water;

uniform mat4 shadowModelView;
uniform mat4 shadowModelViewInverse;
uniform mat4 shadowProjection;
uniform mat4 shadowProjectionInverse;

uniform vec3 cameraPosition;

attribute vec4 mc_midTexCoord;
attribute vec4 mc_Entity;

#include "lib/entity_ids.glsl"

vec4 BiasShadowProjection(vec4 position) {

	vec2 pos = abs(position.xy * 1.165);
	float dist = pow(pow(pos.x, 8.) + pow(pos.y, 8.), 1.0 / 8.0);

	float distortFactor = (1.0 - SHADOW_BIAS) + dist * SHADOW_BIAS;

	position.xy /= distortFactor*0.97;

	position.z /= 2.5;


	return position;
}

void main(){

	water = 0.0;

	texcoord = gl_MultiTexCoord0;

	vec4 position = ftransform();

	position = shadowProjectionInverse * position;
	position = shadowModelViewInverse * position;

	worldPosition = position.xyz + cameraPosition;

	position = shadowModelView * position;
	position = shadowProjection * position;

	gl_Position = BiasShadowProjection(position);

	normal = normalize(gl_NormalMatrix * gl_Normal);

	float id = mc_Entity.x;

  // water
  if (id == WATER_FLOWING || id == WATER_STILL) water = 1.0;

	color = gl_Color;
}
