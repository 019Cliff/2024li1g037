class_name InfiniteWaveGenerator
extends RefCounted

const EnemyScaling = preload("res://src/domain/enemy_scaling.gd")

static func generate(catalog: Variant, wave_number: int) -> Dictionary:
	var level := wave_number + 1
	var amount := 5 + wave_number
	var instances: Array[Dictionary] = []
	for class_id: String in composition(level, amount):
		instances.append(EnemyScaling.scaled_instance(catalog, class_id, level))
	if wave_number % 4 == 0:
		for instance in instances:
			instance["health"] = EnemyScaling.float32(EnemyScaling.float32(float(instance["health"])) * EnemyScaling.float32(1.20))
			instance["attack"] = EnemyScaling.float32(EnemyScaling.float32(float(instance["attack"])) * EnemyScaling.float32(1.12))
	if wave_number % 6 == 0:
		for instance in instances:
			instance["loot"] = maxi(1, floori(EnemyScaling.float32(EnemyScaling.float32(float(instance["loot"])) * EnemyScaling.float32(0.72))))
	var cycle := maxf(0.35, 2.7 - float(level) * 0.08)
	if wave_number % 9 == 0:
		instances.append_array(instances.duplicate(true))
		cycle = maxf(0.35, cycle * 0.82)
	return {"cycle": cycle, "entry_delay": 2.0, "enemies": instances}

static func composition(level: int, amount: int) -> PackedStringArray:
	var fast := maxi(1, amount / 4) if level >= 2 else 0
	var tanks := maxi(1, amount / 6) if level >= 3 else 0
	var specials := maxi(1, amount / 8) if level >= 4 else 0
	var basic := amount - fast - tanks - specials
	var special_ids := ["blindado", "regenerador", "dispersor", "protegido", "elite"]
	var result := PackedStringArray()
	for _index in basic: result.append("basico")
	for _index in fast: result.append("rapido")
	for _index in tanks: result.append("tanque")
	for _index in specials: result.append(special_ids[level % 5])
	return result
