class_name LegacyRunConverter
extends RefCounted

const DomainCatalog = preload("res://src/domain/domain_catalog.gd")
const VerticalSliceSimulation = preload("res://src/domain/vertical_slice_simulation.gd")
const WorldGrid = preload("res://src/domain/world_grid.gd")

static func convert(document: Variant, mode_id: String, unlocked_towers: PackedStringArray) -> Variant:
	if not document is Dictionary:
		return null
	if document.get("schema", "") == "immutable-towers-run":
		return document.duplicate(true)
	if not document.get("map_grid") is Array or not document.get("base") is Dictionary:
		return null
	var catalog := DomainCatalog.new()
	catalog.load_default()
	if not catalog.is_valid():
		return null
	var map_id := _matching_map_id(catalog, document.get("map_grid"))
	var custom_map := {}
	if map_id.is_empty():
		custom_map = _custom_map(document)
		if custom_map.is_empty():
			return null
		map_id = "planicie_serena"
	var simulation := VerticalSliceSimulation.new(catalog, map_id, mode_id, unlocked_towers, custom_map)
	var snapshot := simulation.snapshot()
	var base: Dictionary = document.get("base")
	snapshot["base_health"] = maxf(0.0, float(base.get("health", snapshot.get("base_health", 80.0))))
	snapshot["credits"] = maxi(0, int(base.get("credits", snapshot.get("credits", 0))))
	snapshot["towers"] = _convert_towers(document.get("towers", []), catalog)
	snapshot["enemies"] = _convert_enemies(document.get("active_enemies", []), catalog, simulation.path)
	snapshot["wave_definitions"] = _convert_waves(document.get("portals", []), catalog)
	snapshot["wave_index"] = -1
	snapshot["pending_enemies"] = []
	snapshot["spawn_timer"] = 0.0
	snapshot["intermission"] = 0.0
	snapshot["elapsed"] = 0.0
	snapshot["finished"] = false
	snapshot["victory"] = false
	return snapshot

static func _matching_map_id(catalog: Variant, grid: Array) -> String:
	for map_id: String in catalog.maps:
		if catalog.map_data(map_id).get("grid", []) == grid:
			return map_id
	return ""

static func _custom_map(document: Dictionary) -> Dictionary:
	var portals: Array = document.get("portals", [])
	if portals.is_empty() or not portals[0] is Dictionary:
		return {}
	var portal_position: Variant = portals[0].get("position")
	var base_position: Variant = document.get("base", {}).get("position")
	if not portal_position is Dictionary or not base_position is Dictionary:
		return {}
	return {
		"schema": "immutable-towers-custom-map",
		"version": 1,
		"grid": document.get("map_grid", []).duplicate(true),
		"portal": {"x": floori(float(portal_position.get("x", -1))), "y": floori(float(portal_position.get("y", -1)))},
		"base": {"x": floori(float(base_position.get("x", -1))), "y": floori(float(base_position.get("y", -1)))}
	}

static func _convert_towers(values: Variant, catalog: Variant) -> Array[Dictionary]:
	var converted: Array[Dictionary] = []
	if not values is Array:
		return converted
	for value: Variant in values:
		if not value is Dictionary:
			continue
		var source: Dictionary = value
		var runtime: Variant = source.get("runtime")
		var tower_id := str(runtime.get("tower_id", "")) if runtime is Dictionary else ""
		if not catalog.towers.has(tower_id):
			tower_id = _infer_tower_id(source, catalog)
		if not catalog.towers.has(tower_id) or not source.get("position") is Dictionary:
			continue
		var spec: Dictionary = catalog.tower(tower_id)
		var projectile: Dictionary = source.get("projectile", {})
		var duration: Dictionary = projectile.get("duration", {})
		var position := _scaled_position(source.get("position"))
		var specialization := ""
		if runtime is Dictionary and runtime.get("specialization") != null:
			specialization = str(runtime.get("specialization"))
		converted.append({
			"tower_id": tower_id,
			"position": position,
			"damage": float(source.get("damage", spec.get("base", {}).get("damage", 1.0))),
			"range": float(source.get("range", spec.get("base", {}).get("range", 1.0))),
			"cycle": maxf(0.01, float(source.get("cycle", spec.get("base", {}).get("cycle", 1.0)))),
			"area": float(spec.get("area", 0.0)),
			"burst": maxi(1, int(source.get("burst", 1))),
			"projectile_type": str(projectile.get("type_id", spec.get("base", {}).get("projectile", {}).get("type_id", "resin"))),
			"projectile_duration": _duration_seconds(duration),
			"level": int(runtime.get("level", 1)) if runtime is Dictionary else 1,
			"max_level": int(spec.get("max_level", 1)),
			"purchase_price": int(spec.get("price", 0)),
			"specialization": specialization,
			"cooldown": maxf(0.0, float(source.get("remaining_cycle", 0.0)))
		})
	return converted

static func _infer_tower_id(source: Dictionary, catalog: Variant) -> String:
	var projectile_id := str(source.get("projectile", {}).get("type_id", ""))
	var best_id := ""
	var best_distance := INF
	for tower_id: String in catalog.towers:
		var base: Dictionary = catalog.tower(tower_id).get("base", {})
		if not projectile_id.is_empty() and str(base.get("projectile", {}).get("type_id", "")) != projectile_id:
			continue
		var distance := absf(float(source.get("damage", 0.0)) - float(base.get("damage", 0.0)))
		distance += absf(float(source.get("range", 0.0)) - float(base.get("range", 0.0))) * 2.0
		distance += absf(float(source.get("cycle", 0.0)) - float(base.get("cycle", 0.0))) * 3.0
		if distance < best_distance:
			best_distance = distance
			best_id = tower_id
	return best_id

static func _convert_enemies(values: Variant, catalog: Variant, path: Array[Vector2]) -> Array[Dictionary]:
	var converted: Array[Dictionary] = []
	if not values is Array:
		return converted
	for value: Variant in values:
		if value is Dictionary:
			var enemy := _convert_enemy(value, catalog)
			if not enemy.is_empty():
				enemy["position"] = _scaled_position(enemy.get("position"))
				enemy["segment"] = _nearest_segment(enemy.get("position"), path)
				converted.append(enemy)
	return converted

static func _convert_enemy(source: Dictionary, catalog: Variant) -> Dictionary:
	if not source.get("position") is Dictionary:
		return {}
	var class_id := _infer_enemy_class(float(source.get("base_speed", source.get("speed", 1.0))), catalog)
	var spec: Dictionary = catalog.enemy(class_id)
	var attack := maxf(0.0, float(source.get("attack", spec.get("attack", 1.0))))
	var base_attack := maxf(0.01, float(spec.get("attack", 1.0)))
	var level := maxi(1, 1 + roundi(((attack / base_attack) - 1.0) / 0.12))
	var max_health := float(spec.get("health", 1.0)) * (1.0 + float(level - 1) * 0.22)
	return {
		"class_id": class_id,
		"position": source.get("position").duplicate(true),
		"health": maxf(0.0, float(source.get("health", max_health))),
		"max_health": maxf(0.01, max_health),
		"speed": maxf(0.0, float(source.get("speed", spec.get("speed", 1.0)))),
		"attack": attack,
		"loot": maxi(0, int(source.get("loot", spec.get("loot", 1)))),
		"armor": float(spec.get("armor", 0.0)),
		"direct_resistance": float(spec.get("direct_resistance", 0.0)),
		"area_resistance": float(spec.get("area_resistance", 0.0)),
		"regeneration": float(spec.get("regeneration", 0.0)),
		"shield": float(spec.get("shield", 0.0)),
		"effects": _convert_effects(source.get("effects", [])),
		"hit_flash": 0.0
	}

static func _infer_enemy_class(base_speed: float, catalog: Variant) -> String:
	var best_id := "basico"
	var best_distance := INF
	for enemy_id: String in catalog.enemies:
		var distance := absf(base_speed - float(catalog.enemy(enemy_id).get("speed", 0.0)))
		if distance < best_distance:
			best_distance = distance
			best_id = enemy_id
	return best_id

static func _convert_effects(values: Variant) -> Dictionary:
	var effects := {}
	if not values is Array:
		return effects
	for value: Variant in values:
		if not value is Dictionary:
			continue
		var effect_id := str(value.get("type_id", ""))
		if effect_id in ["fire", "ice", "resin", "fear", "poison", "electric"]:
			effects[effect_id] = float(effects.get(effect_id, 0.0)) + _duration_seconds(value.get("duration", {}))
	return effects

static func _convert_waves(portals: Variant, catalog: Variant) -> Array[Dictionary]:
	var converted: Array[Dictionary] = []
	if not portals is Array:
		return converted
	for portal: Variant in portals:
		if not portal is Dictionary or not portal.get("waves") is Array:
			continue
		for wave: Variant in portal.get("waves", []):
			if not wave is Dictionary:
				continue
			var instances: Array[Dictionary] = []
			for enemy: Variant in wave.get("enemies", []):
				if enemy is Dictionary:
					var converted_enemy := _convert_enemy(enemy, catalog)
					if not converted_enemy.is_empty():
						converted_enemy.erase("position")
						converted_enemy.erase("segment")
						instances.append(converted_enemy)
			converted.append({
				"cycle": maxf(0.35, float(wave.get("cycle", 1.0))),
				"entry_delay": maxf(0.0, float(wave.get("entry_delay", 0.0))),
				"enemies": instances
			})
	return converted

static func _nearest_segment(position_value: Variant, path: Array[Vector2]) -> int:
	if not position_value is Dictionary or path.is_empty():
		return 0
	var position := Vector2(float(position_value.get("x", 0.0)), float(position_value.get("y", 0.0)))
	var best_index := 0
	var best_distance := INF
	for index in path.size():
		var distance := position.distance_squared_to(path[index])
		if distance < best_distance:
			best_distance = distance
			best_index = index
	return best_index

static func _scaled_position(position_value: Variant) -> Dictionary:
	if not position_value is Dictionary:
		return {}
	var position := Vector2(float(position_value.get("x", 0.0)), float(position_value.get("y", 0.0)))
	var scaled := WorldGrid.source_position_to_cell_center(position)
	return {"x": scaled.x, "y": scaled.y}

static func _duration_seconds(duration: Variant) -> float:
	if not duration is Dictionary:
		return 0.0
	return 1000000.0 if duration.get("kind", "finite") == "infinite" else maxf(0.0, float(duration.get("seconds", 0.0)))
