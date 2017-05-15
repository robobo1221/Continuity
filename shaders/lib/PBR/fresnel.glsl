float Schlick(float F0, float LoH) {
	return F0 + (1.0 - F0) * pow(1.0 - LoH, 5.0);
}
