vec4 iProjDiag = vec4(gbufferProjectionInverse[0].x, gbufferProjectionInverse[1].y, gbufferProjectionInverse[2].zw);

vec3 toScreenSpace(vec2 p) {
  vec3 p3 = vec3(p, texture2D(depthtex1,p).x) * 2. - 1.;
  vec4 fragposition = iProjDiag * p3.xyzz + gbufferProjectionInverse[3];
  return fragposition.xyz / fragposition.w;
}

vec3 toScreenSpace(vec2 p, float depth) {
  vec3 p3 = vec3(p, depth) * 2. - 1.;
  vec4 fragposition = iProjDiag * p3.xyzz + gbufferProjectionInverse[3];
  return fragposition.xyz / fragposition.w;
}

vec3 fromScreenSpace(vec3 viewSpace) {
	vec4 screenspace = gbufferProjection * vec4(viewSpace, 1.0);
	screenspace.xyz /= screenspace.w;
	return screenspace.xyz * 0.5f + 0.5f;
}

vec3 toWorldSpace(vec3 viewSpace) {
	vec4 fpos = vec4(viewSpace, 0.0);
	vec4 wpos = gbufferModelViewInverse * fpos;
	return wpos.xyz;
}
