float noise( in vec2 p ) {
    vec2 i = floor( p );
    vec2 f = fract( p );

    f = 1. - f*f;
    f = 1. - f*f;

    float a=hash12(i);
    float b=hash12(i + vec2(1.,0.) );
    float c=hash12(i + vec2(0.,1.) );
    float d=hash12(i + vec2(1.,1.) );

    return 2.*mix(
        mix( a, b, f.x),
        mix( c, d, f.x),
    f.y)-1.;
}

float wave(vec2 p) {
    p += noise2(p);
    return 1.-abs(noise(p)) * 1.2;
}

float waveH(vec2 p, float material) {
    float a = .04;
    float t = frameTimeCounter * .3;

    t *= material;
    p = (p * rotate(1.0)) * vec2(1.3, 0.3) * mix(3.0, 0.45, material);

    float h = (wave(p+t)+wave(p-t)) * a;
    float roughness = 0.0;

    if (material > 0.5) {
      p *= mat2(3.2,1.2,-1.2,3.2);
      a *= .2;
      h += (wave(p+t)+wave(p-t)) * a;
      p *= mat2(5.2,1.2,-1.2,3.2);
      a *= .3;
      h += (wave(p+t)+wave(p-t)) * a;
    }
    
    #ifdef ROUGH_WATER
      roughness = noise((p + t * 50.0) * 100.0) * 0.001;
    #endif

    return (h) * mix(0.15, 0.5, material) + roughness;
}
