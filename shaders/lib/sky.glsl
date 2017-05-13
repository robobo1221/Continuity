
#define CONTRAST 1.4
#define BRIGHTNESS 0.005

float turbidity = mix(2.0, 10.0, rainStrength);

vec3 YxyToXYZ( in vec3 Yxy )
{
	float Y = Yxy.r;
	float x = Yxy.g;
	float y = Yxy.b;

	float X = x * ( Y / y );
	float Z = ( 1.0 - x - y ) * ( Y / y );

	return vec3(X,Y,Z);
}

vec3 XYZToRGB( in vec3 XYZ )
{
	// CIE/E
	mat3 M = mat3
	(
		 2.3706743, -0.9000405, -0.4706338,
		-0.5138850,  1.4253036,  0.0885814,
 		 0.0052982, -0.0146949,  1.0093968
	);

	return XYZ * M;
}


vec3 YxyToRGB( in vec3 Yxy )
{
	vec3 XYZ = YxyToXYZ( Yxy );
	vec3 RGB = XYZToRGB( XYZ );
	return RGB;
}

void calculatePerezDistribution( in float t, out vec3 A, out vec3 B, out vec3 C, out vec3 D, out vec3 E )
{
	A = vec3(  0.1787 * t - 1.4630, -0.0193 * t - 0.2592, -0.0167 * t - 0.2608 );
	B = vec3( -0.3554 * t + 0.4275, -0.0665 * t + 0.0008, -0.0950 * t + 0.0092 );
	C = vec3( -0.0227 * t + 5.3251, -0.0004 * t + 0.2125, -0.0079 * t + 0.2102 );
	D = vec3(  0.1206 * t - 2.5771, -0.0641 * t - 0.8989, -0.0441 * t - 1.6537 );
	E = vec3( -0.0670 * t + 0.3703, -0.0033 * t + 0.0452, -0.0109 * t + 0.0529 );
}

vec3 calculateZenithLuminanceYxy( in float t, in float thetaS )
{
	float chi  	 	= ( 4.0 / 9.0 - t / 120.0 ) * ( PI - 2.0 * thetaS );
	float Yz   	 	= ( 4.0453 * t - 4.9710 ) * tan( chi ) - 0.2155 * t + 2.4192;

	float theta2 	= thetaS * thetaS;
    float theta3 	= theta2 * thetaS;
    float T 	 	= t;
    float T2 	 	= t * t;

	float xz =
      ( 0.00165 * theta3 - 0.00375 * theta2 + 0.00209 * thetaS + 0.0)     * T2 +
      (-0.02903 * theta3 + 0.06377 * theta2 - 0.03202 * thetaS + 0.00394) * T +
      ( 0.11693 * theta3 - 0.21196 * theta2 + 0.06052 * thetaS + 0.25886);

    float yz =
      ( 0.00275 * theta3 - 0.00610 * theta2 + 0.00317 * thetaS + 0.0)     * T2 +
      (-0.04214 * theta3 + 0.08970 * theta2 - 0.04153 * thetaS + 0.00516) * T +
      ( 0.15346 * theta3 - 0.26756 * theta2 + 0.06670 * thetaS + 0.26688);

	return vec3( Yz, xz, yz );
}

vec3 calculatePerezLuminanceYxy( in float theta, in float gamma, in vec3 A, in vec3 B, in vec3 C, in vec3 D, in vec3 E )
{
	return ( 1.0 + A * exp( B / cos( theta ) ) ) * ( 1.0 + C * exp( D * gamma ) + E * cos( gamma ) * cos( gamma ) );
}

vec3 getSky(in vec3 viewPos, bool doSun)
{
	vec3 A, B, C, D, E;
	calculatePerezDistribution(turbidity, A, B, C, D, E);

	float thetaS  = facos(mDot(sunVec,  upVec));
	float thetaM  = facos(mDot(moonVec, upVec));
	float thetaE  = facos(mDot(viewPos, upVec));
	float gammaES = facos(mDot(sunVec,  viewPos));
	float gammaEM = facos(mDot(moonVec, viewPos));

	vec3 YzS = calculateZenithLuminanceYxy(turbidity, thetaS);
	vec3 YzM = calculateZenithLuminanceYxy(turbidity, thetaM);

	vec3 fThetaGammaS = calculatePerezLuminanceYxy(thetaE, gammaES, A, B, C, D, E );
	vec3 fZeroThetaS  = calculatePerezLuminanceYxy(0.0,    thetaS,  A, B, C, D, E );

	vec3 fThetaGammaM = calculatePerezLuminanceYxy(thetaE, gammaEM, A, B, C, D, E );
	vec3 fZeroThetaM  = calculatePerezLuminanceYxy(0.0,    thetaM,  A, B, C, D, E );

  float sun  = smoothstep(0.05, 0.04, facos(mDot(sunVec,  viewPos)));
  float moon = smoothstep(0.05, 0.04, facos(mDot(moonVec, viewPos)));

  float horizon = smoothstep(0.0, 0.3, mDot(viewPos, upVec));

	vec3 YpS = YzS * (fThetaGammaS / fZeroThetaS);
	vec3 YpM = YzM * (fThetaGammaM / fZeroThetaM);

  vec3 sky  = YxyToRGB(YpS) * (sunColor  + 0.005);
			 sky += YxyToRGB(YpM) * (moonColor + 0.002);

	if (doSun) {
  	sky = mix(sky, sunColor  * 1000.0, sun  * horizon);
  	sky = mix(sky, moonColor * 10000.0, moon * horizon);
	}

	return powf(sky, CONTRAST) * BRIGHTNESS * (1.0 - rainStrength * 0.7);
}



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
