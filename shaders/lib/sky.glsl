#define iSteps 12

const float expCoeff = 1.0;

const float     planetRadius = 6371.0e2;
const float atmosphereRadius = 6471.0e2;

const float atmosphereHeight = atmosphereRadius - planetRadius;

const vec2 radiiSquared = pow(vec2(planetRadius, atmosphereRadius), vec2(2.0));

const vec3  OZoneCoeff    = vec3(3.426, 8.298, 0.356) * 6e-7;
const vec3  rayleighCoeff = vec3(0.58, 1.35, 3.31) * 1e-5 * -expCoeff;
const vec3  rayleighOZone = (vec3(0.58, 1.35, 3.31) * 1e-5 + OZoneCoeff) * -expCoeff;
const float      mieCoeff = 7e-6 * -expCoeff;

const float rayleighHeight = 8.0e3 * 0.25;
const float      mieHeight = 1.2e3 * 2.0;

const vec2 invScatterHeight = (-1.0 / vec2(rayleighHeight, mieHeight) * expCoeff); // Optical step constant to save computations inside the loop

float dotNorm(vec3 v, vec3 normal) {
	float norm = inversesqrt(dot(v,v));
	return dot(v, normal) * norm;
}

vec2 AtmosphereDistances(vec3 worldPosition, vec3 worldDirection) {
	// Considers the planet's center as the coordinate origin, as per convention

	float b  = -dot(worldPosition, worldDirection);
	float bb = b * b;
	vec2  c  = dot(worldPosition, worldPosition) - radiiSquared;

	vec2 delta   = sqrt(max(bb - c, 0.0)); // .x is for planet distance, .y is for atmosphere distance
	     delta.x = -delta.x; // Invert delta.x so we don't have to subtract it later

	if (worldPosition.y < atmosphereRadius) { // Uniform condition
		if (bb < c.x || b < 0.0) return vec2(b + delta.y, 0.0); // If the earth is not visible to the ray, check against the atmosphere instead

		vec2 dist     = b + delta;
		vec3 hitPoint = worldPosition + worldDirection * dist.x;

		float horizonCoeff = dotNorm(hitPoint, worldDirection);
		      horizonCoeff = exp2(horizonCoeff * 5.0 * expCoeff);

		return vec2(mix(dist.x, dist.y, horizonCoeff), 0.0);
	} else {
		if (b < 0.0) return vec2(0.0);

		if (bb < c.x) return vec2(2.0 * delta.y, b - delta.y);

		return vec2((delta.y + delta.x) * 2.0, b - delta.y);
	}
}

vec3 ComputeAtmosphericSky(vec3 playerSpacePosition, vec3 worldPosition, vec3 pSun, float visibility, const float iSun) {
	vec3 worldDirection = normalize(playerSpacePosition);

	vec2 atmosphereDistances = AtmosphereDistances(worldPosition, worldDirection);

	if (atmosphereDistances.x <= 0.0) return vec3(0.0);

	float iStepSize = atmosphereDistances.x / float(iSteps); // Calculate the step size of the primary ray
	vec4 iStepSize2 = vec2(log2(iStepSize), 0.0).xxyy - planetRadius * invScatterHeight.rgrg;
	vec3  iStep     = worldDirection * iStepSize;

	vec3 rayleigh = vec3(0.0); // Initialize accumulators for Rayleigh and Mie scattering
	vec3 mie      = vec3(0.0);

	vec2 opticalDepth = vec2(0.0); // Initialize optical depth accumulators, .rg represents rayleigh and mie for the 'i' loop, .ba represent the same for the 'j' loop

	vec3 iPos = worldPosition + worldDirection * (iStepSize * 0.5 + atmosphereDistances.y); // Calculate the primary ray sample position


	vec3 c = vec3(dot(iPos, iPos), dot(iPos, iStep) * 2.0, dot(iStep, iStep));
	float pSunLen2 = dot(pSun, pSun) * 0.25;

	vec2 e = vec2(dot(iPos, pSun), dot(iStep, pSun));

    // Sample the primary ray
	for (float i = 0; i < iSteps; i++) {
		float iPosLength2 = fma(fma(c.z, i, c.y), i, c.x);

		float b = fma(e.y, i, e.x); // b = dot(iPos, pSun);
		float jStepSize = sqrt(fma(b, b, radiiSquared.y - iPosLength2)) - b; // jStepSize = sqrt(b*b + radiiSquared.y - dot(iPos, iPos)) - b;

		float jPosLength2 = fma(fma(pSunLen2, jStepSize, b), jStepSize, iPosLength2);

		vec4 opticalStep = exp2(sqrt(vec2(iPosLength2, jPosLength2)).xxyy * invScatterHeight.rgrg + iStepSize2); // Calculate the optical depth of the Rayleigh and Mie scattering for this step
		opticalDepth += opticalStep.rg; // Accumulate optical depth
		opticalStep.ba = opticalStep.ba * jStepSize + opticalDepth;

		vec3 attn = exp2(rayleighOZone * opticalStep.b + (mieCoeff * opticalStep.a));

		rayleigh += opticalStep.r * attn;
		mie      += opticalStep.g * attn;
    }

	// Calculate the Rayleigh and Mie phases
	float g = 0.9 * sqrt(visibility);
	float gg = g * g;
    float  mu = dot(worldDirection, pSun);
    float rayleighPhase = 1.5 * (1.0 + mu * mu);
    float      miePhase = rayleighPhase * (1.0 - gg) / (pow(1.0 + gg - 2.0 * mu * g, 1.5) * (2.0 + gg));

	mie = max(vec3(0.0), mie);

    // Calculate and return the final color
    return iSun * (rayleigh * rayleighPhase * rayleighCoeff + mie * miePhase * mieCoeff) / -expCoeff;
}

vec3 CalculateAtmosphericSky(vec3 worldSpacePosition, float visibility) {
	float isNight = float(dot(sunVec, upVec) < 0.0);
	vec3 worldPosition = vec3(0.0, planetRadius + 1.061e3 + max(0.0, cameraPosition.y - HORIZON_HEIGHT) * 40.0, 0.0);
	vec3 worldSunPosition = shadowModelViewInverse[2].xyz * (1.0 - isNight * 2.0);

	return ComputeAtmosphericSky(worldSpacePosition, worldPosition, worldSunPosition, visibility, 3.0);
}

float getAtmosphere() {
	float thickness = mix(pow(dot(sunVec, downVec) * 0.5 + 0.5, 3.0) * 0.001, 0.00008, rainStrength);
	return thickness;
}

float rainFix = (1.0 - rainStrength * 0.9);

//config
#define atmosphereHeight 8000. * (1.0 + getAtmosphere() * 500.0) // actual thickness of the atmosphere
#define earthRadius 6371000.    // actual radius of the earth
#define mieMultiplier 2.
#define ozoneMultiplier 1.      // 1. for physically based
#define rayleighDistribution 8. //physically based
#define mieDistribution 1.8 * (1.0 + getAtmosphere() * 5000.0)     //physically based
#define rayleighCoefficient vec3(5.8e-6,1.35e-5,3.31e-5) // Physically based (Bruneton, Neyret)
#define ozoneCoefficient (vec3(3.426,8.298,.356) * 6e-5 / 100.) // Physically based (Kutz)
#define mieCoefficient ( 3e-6 * mieMultiplier) //good default
#define up gbufferModelView[1].xyz

vec2 js_getThickness2(vec3 rd){
    vec2 sr = earthRadius + vec2(
        atmosphereHeight,
        atmosphereHeight * mieDistribution / rayleighDistribution
    );
    vec3 ro = -up * earthRadius;
    float b = dot(rd, ro);
    float t = b * b - dot(ro, ro);
    return b + sqrt( sr * sr + t );
}

#define getEarth(a) pow(smoothstep(-.1,.1,dot(up,a)),1.)
// Improved Rayleigh phase for single scattering (Elek)
#define js_phaseRayleigh(a) ( .4 * (a) + 1.12 )

float phaseMie(float x){
    const vec3 c = vec3(.256098,.132268,.010016);
    const vec3 d = vec3(-1.5,-1.74,-1.98);
    const vec3 e = vec3(1.5625,1.7569,1.9801);
    float b = x * x + 1.;
    vec3 f = b * c / pow( d * x + e, vec3(1.5));
    return dot(f,vec3(.33333333333));
}

float phaseMie2(float x){
    const vec3 c = vec3(.256098,.132268,.010016);
    const vec3 d = vec3(-1.5,-1.74,-1.98);
    const vec3 e = vec3(1.5625,1.7569,1.9801);
    float b = x * x + 1.;
    vec3 f = b * c / pow( d * x + e, vec3(1.5));
    return dot(f.xy,vec2(.33333333333));
}

#define js_absorb(a) exp( -(a).x * (  ozoneCoefficient * ozoneMultiplier + rayleighCoefficient) - 1.11 * (a).y * mieCoefficient)

const float js_steps = 8.;

vec2 js_sunThickness  = js_getThickness2(sunVec)  / js_steps;
vec2 js_moonThickness = js_getThickness2(moonVec) / js_steps;

vec3 js_sunAbsorb  = js_absorb(js_sunThickness)  * getEarth(sunVec)  * 0.9;
vec3 js_moonAbsorb = js_absorb(js_moonThickness) * getEarth(moonVec) * 0.01;
vec3 js_moonColor = vec3(0.9,0.6,0.3) * 0.015;

vec3 lightColor = (powf(js_sunAbsorb / 0.9, js_steps) * sunFade) + (powf(js_moonAbsorb / 0.01, js_steps) * js_moonColor * moonFade);


vec3 js_getScatter(vec3 color, vec3 V) {
    vec2 thickness = js_getThickness2(V) / js_steps;

		float dotVS = dot(V, sunVec);
    float dotVM = dot(V, moonVec);

    vec3 viewAbsorb = js_absorb(thickness);
    vec4 scatterCoeff = 1. - exp(-thickness.xxxy * vec4(rayleighCoefficient,mieCoefficient));

		float rayleighPhaseS = js_phaseRayleigh(dotVS);
    float rayleighPhaseM = js_phaseRayleigh(dotVM);

		float miePhaseS = phaseMie2(dotVS);
    float miePhaseM = phaseMie2(dotVM);

		vec3 scatterS = scatterCoeff.xyz * rayleighPhaseS + (scatterCoeff.w * miePhaseS);
    vec3 scatterM = scatterCoeff.xyz * rayleighPhaseM + (scatterCoeff.w * miePhaseM);

		vec3 sun = sin(max(dot(sunVec,   V) - 0.9985, 0.0) / 0.0015 * PI * 0.5) * js_sunAbsorb  * 100.0;
		vec3 moon = sin(max(dot(moonVec, V) - 0.9985, 0.0) / 0.0015 * PI * 0.5) * js_moonAbsorb * 100.0 * (js_moonColor * 100.0);
		vec3 skyColorS = color + sun;
    vec3 skyColorM = color + moon;

    for(int i=0;i<int(js_steps);i++){
				scatterS *= js_sunAbsorb;
        scatterM *= js_moonAbsorb * js_moonColor * 100.0;

				skyColorS = skyColorS * viewAbsorb + scatterS;
        skyColorM = skyColorM * viewAbsorb + scatterM;
    }

    return skyColorS + skyColorM;
}

vec3 js_getAmbient(vec3 V) {
	vec2 thickness = js_getThickness2(V) / js_steps;

	float dotVS = dot(V, sunVec);
	float dotVM = dot(V, moonVec);

	vec3 viewAbsorb = js_absorb(thickness);
	vec4 scatterCoeff = 1. - exp(-thickness.xxxy * vec4(rayleighCoefficient,mieCoefficient));

	float rayleighPhaseS = js_phaseRayleigh(dotVS);
	float rayleighPhaseM = js_phaseRayleigh(dotVM);

	float miePhaseS = phaseMie2(dotVS);
	float miePhaseM = phaseMie2(dotVM);

	vec3 scatterS = scatterCoeff.xyz * rayleighPhaseS + (scatterCoeff.w * miePhaseS);
	vec3 scatterM = scatterCoeff.xyz * rayleighPhaseM + (scatterCoeff.w * miePhaseM);

	vec3 skyColorS = vec3(0.0);
	vec3 skyColorM = vec3(0.0);

	float earth = pow(1.0 - mDot(downVec, V), 2.0);

	for(int i=0;i<int(js_steps);i++){
			scatterS *= js_sunAbsorb;
			scatterM *= js_moonAbsorb;

			skyColorS = skyColorS * viewAbsorb + scatterS;
			skyColorM = skyColorM * viewAbsorb + scatterM;
	}

	return (skyColorS + skyColorM);
}

vec3 js_totalScatter() {
	vec2 thickness = js_getThickness2(upVec);
	vec4 scatterCoeff = 1. - exp(-thickness.xxxy * vec4(rayleighCoefficient,mieCoefficient));
	vec3 approxScatter = (js_sunAbsorb + js_moonAbsorb) * phaseMie2(0.0);
	return (scatterCoeff.rgb * js_sunAbsorb) + (scatterCoeff.rgb * js_moonAbsorb) + approxScatter;
}

vec3 js_sunColor(vec3 V, vec3 L) {
    return js_absorb(js_getThickness2(L)) * getEarth(L);
}
