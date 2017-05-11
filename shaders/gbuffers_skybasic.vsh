#version 410 compatibility
#define gbuffers_skybasic
#define vsh

void main() {

	gl_Position = ftransform();

}
