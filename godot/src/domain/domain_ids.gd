class_name DomainIds
extends RefCounted

const TERRAIN: PackedStringArray = ["grass", "path", "asphalt", "water"]
const MAPS: PackedStringArray = ["planicie_serena", "garganta_pedra", "lago_fraturado", "cruzamento_solar", "bastiao_espiral"]
const MODES: PackedStringArray = ["history", "infinite", "challenge", "boss", "sandbox"]
const TOWERS: PackedStringArray = ["sentinela", "glaciar", "braseiro", "panico", "venenoide", "tesla", "impacto", "solar", "tempestade"]
const ENEMIES: PackedStringArray = ["basico", "rapido", "tanque", "blindado", "regenerador", "dispersor", "protegido", "elite", "boss_acelerador", "boss_guardiao", "boss_ruptura"]

static func validate_catalog() -> PackedStringArray:
	var errors := PackedStringArray()
	_validate_group("terrain", TERRAIN, errors)
	_validate_group("maps", MAPS, errors)
	_validate_group("modes", MODES, errors)
	_validate_group("towers", TOWERS, errors)
	_validate_group("enemies", ENEMIES, errors)
	return errors

static func _validate_group(group_name: String, ids: PackedStringArray, errors: PackedStringArray) -> void:
	var seen: Dictionary[String, bool] = {}
	for stable_id: String in ids:
		if stable_id.is_empty() or stable_id != stable_id.to_lower() or " " in stable_id:
			errors.append(group_name + ": invalid id " + stable_id)
		elif seen.has(stable_id):
			errors.append(group_name + ": duplicate id " + stable_id)
		else:
			seen[stable_id] = true
