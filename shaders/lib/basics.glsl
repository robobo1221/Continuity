vec3 aux1 = texture2D(colortex1, texcoord.st).rgb;
vec3 aux2 = texture2D(colortex2, texcoord.st).rgb;

vec3 normalToViewSpace(vec3 worldSpace) {
  vec4 wpos = vec4(worldSpace, 0.0);
  vec4 fpos = gbufferModelView * wpos;
  return fpos.xyz;
}

vec3 normal1 = decodeNormal(aux1.r);
vec3 normal2 = normalToViewSpace(decodeNormal(aux2.r));

float matIDs1 = decodeMatIDs(aux1.b);
float matIDs2 = decodeMatIDs(aux2.b);

vec2 lightmaps1 = decodeLightMap(aux1.g);
vec2 lightmaps2 = decodeLightMap(aux2.g);

float depth1 = texture2D(depthtex0, texcoord.st).x;
float depth2 = texture2D(depthtex1, texcoord.st).x;

vec3 lightVec = normalize(shadowLightPosition);
vec3 sunVec   = normalize( sunPosition);
vec3 moonVec  = normalize(-sunPosition);
vec3 upVec    = normalize( upPosition);
vec3 downVec  = normalize(-upPosition);


float land = float(pow(depth2, 2.0) < pow(depth2, 1.0));
float transparent = float(depth1 < depth2);
float water = getMatID(1.0, matIDs2);
float translucent = getMatID(2.0, matIDs1);
float emitter = getMatID(3.0, matIDs1);
