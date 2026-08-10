class_name DomainCatalog
extends RefCounted

const DomainIds = preload("res://src/domain/domain_ids.gd")
const DEFAULT_PATH := "res://data/domain-catalog-v1.json"

var errors := PackedStringArray()
var document: Dictionary = {}
var towers: Dictionary[String, Dictionary] = {}
var enemies: Dictionary[String, Dictionary] = {}
var maps: Dictionary[String, Dictionary] = {}
var modes: Dictionary[String, Dictionary] = {}

func load_default() -> void:
	_load(DEFAULT_PATH)

func is_valid() -> bool:
	return errors.is_empty()

func tower(tower_id: String) -> Dictionary:
	return towers.get(tower_id, {})

func enemy(enemy_id: String) -> Dictionary:
	return enemies.get(enemy_id, {})

func map_data(map_id: String) -> Dictionary:
	return maps.get(map_id, {})

func mode(mode_id: String) -> Dictionary:
	return modes.get(mode_id, {})

func _load(path: String) -> void:
	var file := FileAccess.open(path, FileAccess.READ)
	if file == null:
		errors.append("catalog: file could not be opened")
		return
	var parsed: Variant = JSON.parse_string(file.get_as_text())
	if not parsed is Dictionary:
		errors.append("catalog: root must be an object")
		return
	document = parsed
	if document.get("schema", "") != "immutable-towers-domain-catalog":
		errors.append("catalog: invalid schema")
	if int(document.get("version", 0)) != 1:
		errors.append("catalog: unsupported version")
	_index_group("towers", DomainIds.TOWERS, towers)
	_index_group("enemies", DomainIds.ENEMIES, enemies)
	_index_group("maps", DomainIds.MAPS, maps)
	_index_group("modes", DomainIds.MODES, modes)

func _index_group(group_name: String, expected_ids: PackedStringArray, target: Dictionary[String, Dictionary]) -> void:
	var entries: Variant = document.get(group_name)
	if not entries is Array:
		errors.append("catalog: " + group_name + " must be an array")
		return
	for entry: Variant in entries:
		if not entry is Dictionary:
			errors.append("catalog: invalid " + group_name + " entry")
			continue
		var item: Dictionary = entry
		var stable_id: String = item.get("id", "")
		if stable_id.is_empty() or target.has(stable_id):
			errors.append("catalog: invalid or duplicate " + group_name + " id")
		else:
			target[stable_id] = item
	for expected_id: String in expected_ids:
		if not target.has(expected_id):
			errors.append("catalog: missing " + group_name + " id " + expected_id)
