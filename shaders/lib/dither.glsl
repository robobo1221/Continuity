#define hammersley(i, N) vec2( float(i) / float(N), float( bitfieldReverse(i) ) * 2.3283064365386963e-10 )
#define tau 6.2831853071795864769252867665590
#define circlemap(p) (vec2(cos((p).y*tau), sin((p).y*tau)) * p.x)

#define g(a) (4-(a).x-((a).y<<1))%4

float bayer(vec2 tc){
  ivec2 p = ivec2(tc);
  return float(
     g(p>>3&1)    +
    (g(p>>2&1)<<2)+
    (g(p>>1&1)<<4)+
    (g(p   &1)<<6)
  )/255.;
}
