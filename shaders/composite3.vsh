#version 410 compatibility
#define composite3
#define vsh
#include "lib/utility.glsl"

varying vec4 texcoord;

void main() {

	gl_Position = ftransform();

	texcoord = gl_MultiTexCoord0;
}
