vec3 skyAbsorbance = vec3(0.95, 0.55, 0.75);

float sunFade  = smoothstep(0.0, 0.7, mDot(sunVec, upVec));
float moonFade = smoothstep(0.0, 0.3, mDot(moonVec, upVec));

vec3 sunColor   = powf(skyAbsorbance, pow(1.0 + dot(sunVec, downVec), 1.5)) * sunFade;
vec3 moonColor  = vec3(0.08,0.11,0.15) * 0.14 * moonFade;
//vec3 lightColor = sunColor + moonColor;

vec3 torchColor = vec3(0.5,0.33,0.15) * 0.1;
vec3 waterColor = vec3(0.0,0.2,0.15);
vec3 waterAbsorptionColor = vec3(0.6, 0.7, 0.75);
