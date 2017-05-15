#include "lib/utility.glsl"
#include "lib/settings.glsl"

varying vec4 texcoord;
varying vec4 lightmapCoord;
varying vec4 color;

varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;
varying mat3 tbnMatrix;

varying float material;

#if defined gbuffers_terrain && defined WAVING_TERRAIN
	uniform mat4 gbufferModelView;
	uniform mat4 gbufferModelViewInverse;

	uniform vec3 cameraPosition;
	uniform float frameTimeCounter;
#endif

attribute vec4 mc_Entity;
attribute vec4 at_tangent;

#if defined gbuffers_terrain && defined WAVING_TERRAIN
	attribute vec4 mc_midTexCoord;
#endif

#include "lib/entity_ids.glsl"

#if defined gbuffers_terrain && defined WAVING_TERRAIN
	vec3 calcWave(in vec3 pos, in float fm, in float mm, in float ma, in float f0, in float f1, in float f2, in float f3, in float f4, in float f5) {
		float pi2wt = PI*2*(frameTimeCounter*24);

	  vec3 ret;
	  float magnitude, d0, d1, d2, d3;
	  magnitude = sin(pi2wt*fm + pos.x*0.5 + pos.z*0.5 + pos.y*0.5) * mm + ma;

	  d0 = sin(pi2wt*f0);
	  d1 = sin(pi2wt*f1);
	  d2 = sin(pi2wt*f2);

	  ret.x = sin(pi2wt*f3 + d0 + d1 - pos.x + pos.z + pos.y) * magnitude;
	  ret.z = sin(pi2wt*f4 + d1 + d2 + pos.x - pos.z + pos.y) * magnitude;
		ret.y = sin(pi2wt*f5 + d2 + d0 + pos.z + pos.y - pos.y) * magnitude;

	  return ret;
	}

	vec3 calcMove(in vec3 pos, in float f0, in float f1, in float f2, in float f3, in float f4, in float f5, in vec3 amp1, in vec3 amp2) {
	  vec3 move1 = calcWave(pos      , 0.0027, 0.0400, 0.0400, 0.0127, 0.0089, 0.0114, 0.0063, 0.0224, 0.0015) * amp1;
		vec3 move2 = calcWave(pos+move1, 0.0348, 0.0400, 0.0400, f0, f1, f2, f3, f4, f5) * amp2;

	  return move1+move2;
	}

	void displaceVertex(inout vec4 position, in float lightmap) {
		float istopv = gl_MultiTexCoord0.t < mc_midTexCoord.t ? 1.0 : 0.0;

		float underCover = clamp(pow(lightmap, 15.0) * 2.0,0.0,1.0);

		float wavyMult  = 1.0;

		vec3 worldpos = position.xyz + cameraPosition;

		// Waving leaves / tall flowers
		vec3 waving1 = calcMove(worldpos.xyz, 0.0030, 0.0054, 0.0033, 0.0025, 0.0017, 0.0031,vec3(0.75,0.15,0.75), vec3(0.375,0.075,0.375)) * underCover * wavyMult;

		// Waving vines / cobwebs / plants
		vec3 waving2 = calcMove(worldpos.xyz, 0.0040, 0.0064, 0.0043, 0.0035, 0.0037, 0.0041,vec3(1.0,0.2,1.0), vec3(0.5,0.1,0.5)) * underCover * wavyMult;

		// Waving leaves / tall flowers
		if ( mc_Entity.x == LEAVES || mc_Entity.x == LEAVES2 || mc_Entity.x == NEWFLOWERS )
			position.xyz += waving1;

		// Waving plants
		if ( mc_Entity.x == VINES )
			position.xyz += waving2;

		// Waving cobwebs
		if ( mc_Entity.x == COBWEB )
			position.xyz += waving2 * 0.1;

		// Waving plants
		if (istopv > 0.9) {
			if ( mc_Entity.x == TALLGRASS || mc_Entity.x == DANDELION || mc_Entity.x == ROSE || mc_Entity.x == WHEAT || mc_Entity.x == FIRE ||
			mc_Entity.x == NETHER_WART || mc_Entity.x == DEAD_BUSH || mc_Entity.x == CARROT || mc_Entity.x == POTATO)
				position.xyz += waving2;
		}
	}
#endif

void main() {

	material = 0.0;

  	lightmapCoord = vec4(0.0);
	#ifndef gbuffers_basic
  	lightmapCoord = gl_TextureMatrix[1] * gl_MultiTexCoord1;
	#endif

	texcoord = gl_MultiTexCoord0;

  	color = gl_Color;

	#if defined gbuffers_terrain && defined WAVING_TERRAIN
		vec4 position = gbufferModelViewInverse * gl_ModelViewMatrix * gl_Vertex;

		displaceVertex(position, lightmapCoord.y);

		gl_Position = gl_ProjectionMatrix * gbufferModelView * position;
	#else
		gl_Position = ftransform();
	#endif

	normal = normalize(gl_NormalMatrix * gl_Normal);
  tangent = normalize(gl_NormalMatrix*at_tangent.xyz);
  binormal = normalize(cross(tangent, normal));

  tbnMatrix = transpose(mat3(tangent, binormal, normal));

  float id = mc_Entity.x;

  // Check block IDs and assign the material ID of 0.2 to translucent blocks.
  if (id == LEAVES || id == VINES || id == TALLGRASS || id == DANDELION || id == ROSE ||
      id == WHEAT || id == LILYPAD || id == LEAVES2 || id == NEWFLOWERS || id == NETHER_WART ||
      id == DEAD_BUSH || id == CARROT || id == POTATO || id == COBWEB || id == SUGAR_CANE ||
      id == BROWN_SHROOM || id == RED_SHROOM) material = 2.0;

  // Check block IDs and assign the material ID of 0.3 to translucent blocks.
  if (id == TORCH || id == FIRE || id == LAVAFLOWING || id == LAVASTILL || id == GLOWSTONE ||
      id == SEA_LANTERN || id == LAMP_ON || id == BEACON) material = 3.0;

}
