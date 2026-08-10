class_name RunRewards
extends RefCounted

const INFINITE_REWARD_MULTIPLIER := 2.5
const INFINITE_REWARD_CAP := 188

static func wave_reached(simulation: Variant) -> int:
	return maxi(0, int(simulation.wave_index) + 1)

static func completed_waves(simulation: Variant) -> int:
	var current_wave_cleared: int = 1 if simulation.pending_enemies.is_empty() and simulation.enemies.is_empty() else 0
	return maxi(0, int(simulation.wave_index) + current_wave_cleared)

static func infinite_gems(wave: int) -> int:
	var reached := maxi(0, wave)
	if reached == 0:
		return 0
	var early_reward := mini(reached, 5)
	var middle_waves := clampi(reached - 5, 0, 10)
	var middle_reward := floori(float(middle_waves) * 1.5)
	var endurance_waves := maxi(0, reached - 15)
	var endurance_reward := floori((sqrt(float(endurance_waves + 1)) - 1.0) * 6.0)
	var base_reward := early_reward + middle_reward + endurance_reward
	return mini(INFINITE_REWARD_CAP, roundi(float(base_reward) * INFINITE_REWARD_MULTIPLIER))
