class_name RunDifficultyProfile
extends RefCounted

const MAP_SIGNATURE_ENEMIES := {
	"planicie_serena": "rapido",
	"garganta_pedra": "blindado",
	"lago_fraturado": "regenerador",
	"cruzamento_solar": "dispersor",
	"bastiao_espiral": "protegido"
}

static func create(mode_id: String, map_id: String, context: Dictionary) -> Dictionary:
	var enabled := mode_id == "history" and bool(context.get("campaign_profile", false))
	var chapter := clampi(int(context.get("chapter", 1)), 1, 20)
	var stage := clampi(int(context.get("stage", 1)), 1, 5)
	return {
		"id": "cap%02d-est%02d-%s" % [chapter, stage, map_id],
		"enabled": enabled,
		"chapter": chapter,
		"stage": stage,
		"map_id": map_id,
		"signature_enemy": str(MAP_SIGNATURE_ENEMIES.get(map_id, "rapido")),
		"health_multiplier": 1.0 + float(chapter - 1) * 0.18 + float(stage - 1) * 0.035,
		"attack_multiplier": 1.0 + float(chapter - 1) * 0.12 + float(stage - 1) * 0.025,
		"speed_multiplier": 1.0 + minf(0.12, float(chapter - 1) * 0.018 + float(stage - 1) * 0.006),
		"loot_multiplier": 1.0 + float(chapter - 1) * 0.08 + float(stage - 1) * 0.02,
		"par_seconds": 720.0 + float(stage - 1) * 45.0 + float(chapter - 1) * 60.0
	}

static func apply(catalog: Variant, base_waves: Array, profile: Dictionary) -> Array:
	var result: Array = base_waves.duplicate(true)
	if not bool(profile.get("enabled", false)):
		return result
	var stage := int(profile.get("stage", 1))
	if int(profile.get("chapter", 1)) == 1 and stage == 1:
		return result
	var signature_enemy := str(profile.get("signature_enemy", "rapido"))
	for wave_index in result.size():
		var wave: Dictionary = result[wave_index]
		var instances: Array = wave.get("enemies", []).duplicate(true)
		for instance_index in instances.size():
			instances[instance_index] = _scale_instance(catalog, instances[instance_index], profile)
		if stage >= 2 and (wave_index + 1) % 3 == 0:
			instances.append(_profiled_instance(catalog, signature_enemy, wave_index + 1, profile))
		if stage >= 3 and (wave_index + 1) % 2 == 0:
			_replace_first_basic(instances, _profiled_instance(catalog, signature_enemy, wave_index + 1, profile))
		if stage >= 4 and (wave_index + 1) in [5, 8]:
			instances.append(_profiled_instance(catalog, _support_enemy(signature_enemy), wave_index + 1, profile))
		if stage >= 5 and wave_index == result.size() - 1:
			instances.append(_profiled_instance(catalog, "elite", wave_index + 1, profile))
		wave["enemies"] = instances
		if stage >= 4:
			wave["cycle"] = maxf(0.35, float(wave.get("cycle", 1.0)) * 0.94)
		result[wave_index] = wave
	return result

static func _scale_instance(catalog: Variant, value: Variant, profile: Dictionary) -> Dictionary:
	var instance: Dictionary = value.duplicate(true) if value is Dictionary else {}
	var class_id := str(instance.get("class_id", "basico"))
	var spec: Dictionary = catalog.enemy(class_id)
	instance["class_id"] = class_id
	instance["health"] = float(instance.get("health", spec.get("health", 1.0))) * float(profile.get("health_multiplier", 1.0))
	instance["attack"] = float(instance.get("attack", spec.get("attack", 1.0))) * float(profile.get("attack_multiplier", 1.0))
	instance["speed"] = float(instance.get("speed", spec.get("speed", 1.0))) * float(profile.get("speed_multiplier", 1.0))
	instance["loot"] = maxi(1, floori(float(instance.get("loot", spec.get("loot", 1))) * float(profile.get("loot_multiplier", 1.0))))
	return instance

static func _profiled_instance(catalog: Variant, class_id: String, wave_level: int, profile: Dictionary) -> Dictionary:
	var spec: Dictionary = catalog.enemy(class_id)
	var level_scale := float(maxi(0, wave_level - 1))
	return _scale_instance(catalog, {
		"class_id": class_id,
		"health": float(spec.get("health", 1.0)) * (1.0 + level_scale * 0.12),
		"attack": float(spec.get("attack", 1.0)) * (1.0 + level_scale * 0.07),
		"speed": float(spec.get("speed", 1.0)) + minf(0.04, level_scale * 0.002),
		"loot": floori(float(spec.get("loot", 1)) * (1.0 + level_scale * 0.05))
	}, profile)

static func _replace_first_basic(instances: Array, replacement: Dictionary) -> void:
	for index in instances.size():
		if str(instances[index].get("class_id", "")) == "basico":
			instances[index] = replacement
			return
	instances.append(replacement)

static func _support_enemy(signature_enemy: String) -> String:
	match signature_enemy:
		"blindado":
			return "protegido"
		"regenerador":
			return "rapido"
		"dispersor":
			return "tanque"
		"protegido":
			return "blindado"
		_:
			return "tanque"
