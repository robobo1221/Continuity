float encodeMatIDs(in float matIDs) {
  return matIDs / 255.0;
}

float decodeMatIDs(in float matIDs) {
  return floor(matIDs * 255.0);
}

float encodeColor(vec3 a){
  vec3 m = vec3(31,63,31);
  //a += (bayer16x16(ivec2(gl_FragCoord.xy))-.5) / m;
  a = clamp(a, 0., 1.);
  ivec3 b = ivec3(a*m);
  return float( b.r|(b.g<<5)|(b.b<<11) ) / 65535.;
}

vec3 decodeColor(float a){
  int bf = int(a*65535.);
  return vec3(bf%32, (bf>>5)%64, bf>>11) / vec3(31,63,31);
}

float encodeNormal(vec3 a){
  vec2 spheremap = a.xy / sqrt( a.z * 8. + 8. ) + .5;
  ivec2 bf = ivec2(spheremap*255.);
  return float( bf.x|(bf.y<<8) ) / 65535.;
}

vec3 decodeNormal(float a){
  int bf = int(a*65535.);
  vec2 b = vec2(bf%256, bf>>8) / 63.75 - 2.;
  float c = dot(b, b);
  return vec3( b * sqrt(1.-c*.25), 1. - c * .5 );
}

float encodeLightMap(vec2 a){
  ivec2 bf = ivec2(a*255.);
  return float( bf.x|(bf.y<<8) ) / 65535.;
}

vec2 decodeLightMap(float a){
  int bf = int(a*65535.);
  return vec2(bf%256, bf>>8) / 255.;
}
