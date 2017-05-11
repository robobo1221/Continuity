vec3 BRDF(vec3 N, vec3 V, vec3 L, float r, vec3 f0) {
    r *= r;

    vec3 H = normalize(V + L);

    float NoV = abs(dot(N, V));
    float VoH = clamp(dot(V, H),0.,1.);
    float NoH = clamp(dot(N, H),0.,1.);
    float NoL = clamp(dot(N, L),0.,1.);

    vec3 fresnel = f0 + (1.0 - f0) * pow(1.0 - VoH, 5.0);
    float Lambda_GGXV = NoL * pow((-NoV * r + NoV) * NoV + r,0.5);
    float Lambda = NoV * pow((-NoL * r + NoL) * NoL + r,0.5) + Lambda_GGXV;

    float Vis = 0.159155 / Lambda;

    float denom = (NoH * r - NoH) * NoH + 1.0;

    float distribution = r / (denom * denom);

    return (distribution * Vis * NoL) * fresnel;
}
