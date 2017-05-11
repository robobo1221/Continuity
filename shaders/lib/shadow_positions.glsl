vec3 convertScreenSpaceToWorldSpace(vec2 co, float depth) {
    vec4 fragposition = gbufferProjectionInverse * vec4(vec3(co, depth) * 2.0 - 1.0, 1.0);
    fragposition /= fragposition.w;
    return fragposition.xyz;
}

vec4 getShadowSpace(float shadowdepth, vec2 texcoord){

	vec4 fragpos = vec4(convertScreenSpaceToWorldSpace(texcoord.st,shadowdepth), 1.0);

	if (isEyeInWater > 0.9)
	fragpos.xy *= 0.817;

	vec4 wpos = vec4(0.0);
		wpos = gbufferModelViewInverse * fragpos;

		wpos = shadowModelView * wpos;
		wpos = shadowProjection * wpos;
		wpos /= wpos.w;

	return wpos;

}

float getDistortFactor(vec4 worldposition){
	vec2 pos1 = abs(worldposition.xy * 1.165);

	float distb = pow(pow(pos1.x, 8.) + pow(pos1.y, 8.), 1.0 / 8.0);
	return (1.0 - SHADOW_BIAS) + distb * SHADOW_BIAS;
}

vec4 biasedShadows(vec4 worldposition){

	float distortFactor = getDistortFactor(worldposition);

	worldposition.xy /= distortFactor*0.97;
	worldposition = worldposition * vec4(0.5,0.5,0.2,0.5) + vec4(0.5);

	return worldposition;
}
