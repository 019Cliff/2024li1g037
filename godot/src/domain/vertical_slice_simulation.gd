class_name VerticalSliceSimulation
extends RefCounted

const DomainCatalog = preload("res://src/domain/domain_catalog.gd")
const EnemySpatialHash = preload("res://src/gameplay/enemy_spatial_hash.gd")
const MapPathfinder = preload("res://src/gameplay/map_pathfinder.gd")
const WorldGrid = preload("res://src/domain/world_grid.gd")
const EnemyScaling = preload("res://src/domain/enemy_scaling.gd")
const InfiniteWaveGenerator = preload("res://src/domain/infinite_wave_generator.gd")
const RunDifficultyProfile = preload("res://src/domain/run_difficulty_profile.gd")
const TowerEconomy = preload("res://src/economy/tower_economy.gd")
const CombatResolver = preload("res://src/combat/combat_resolver.gd")
const EnemyRuntime = preload("res://src/combat/enemy_runtime.gd")
const ProjectileEffects = preload("res://src/combat/projectile_effects.gd")
const TOWER_LIMITS := {
	"history": 18,
	"infinite": 24,
	"challenge": 12,
	"boss": 16,
	"sandbox": 30
}
class EnemyState extends RefCounted:
	var class_id: String
	var position: Vector2
	var segment: int = 0
	var health: float
	var max_health: float
	var speed: float
	var attack: float
	var loot: int
	var armor: float
	var direct_resistance: float
	var area_resistance: float
	var regeneration: float
	var shield: float
	var effects: Dictionary[String, float] = {}
	var hit_flash: float = 0.0
	var hard_cc_exposure: float = 0.0
	var cc_window_remaining: float = 0.0

	func _init(id: String, spawn: Vector2, spec: Dictionary, instance: Dictionary = {}) -> void:
		class_id = id
		position = spawn
		max_health = float(instance.get("health", spec.get("health", 1.0)))
		health = max_health
		speed = float(instance.get("speed", spec.get("speed", 1.0)))
		attack = float(instance.get("attack", spec.get("attack", 1.0)))
		loot = int(instance.get("loot", spec.get("loot", 1)))
		armor = float(spec.get("armor", 0.0))
		direct_resistance = float(spec.get("direct_resistance", 0.0))
		area_resistance = float(spec.get("area_resistance", 0.0))
		regeneration = float(spec.get("regeneration", 0.0))
		shield = float(spec.get("shield", 0.0))

class TowerState extends RefCounted:
	var tower_id: String
	var position: Vector2
	var damage: float
	var range_cells: float
	var cycle: float
	var area: float
	var burst: int
	var projectile_type: String
	var projectile_duration: float
	var level: int = 1
	var max_level: int
	var purchase_price: int
	var purchase_price_paid: int
	var upgrade_investment_paid: int = 0
	var investment_paid: int
	var specialization: String = ""
	var priority: String
	var cooldown: float = 0.0

	func _init(id: String, cell_position: Vector2, spec: Dictionary, price_paid: int = -1) -> void:
		var base: Dictionary = spec.get("base", {})
		tower_id = id
		position = cell_position
		damage = float(base.get("damage", 1.0))
		range_cells = float(base.get("range", 1.0))
		cycle = float(base.get("cycle", 1.0))
		area = float(spec.get("area", 0.0))
		burst = int(base.get("burst", 1))
		var projectile: Dictionary = base.get("projectile", {})
		projectile_type = str(projectile.get("type_id", "resin"))
		var duration: Dictionary = projectile.get("duration", {})
		projectile_duration = float(duration.get("seconds", 0.0))
		max_level = int(spec.get("max_level", 1))
		purchase_price_paid = int(spec.get("price", 0)) if price_paid < 0 else price_paid
		purchase_price = purchase_price_paid
		investment_paid = purchase_price_paid
		priority = str(spec.get("priority", "PrimeiroNaRota"))

var catalog: Variant
var map_grid: Array = []
var path: Array[Vector2] = []
var map_id := "planicie_serena"
var custom_map_document: Dictionary = {}
var mode_id := "history"
var run_context: Dictionary = {}
var difficulty_profile: Dictionary = {}
var tower_limit := 18
var available_tower_ids := PackedStringArray()
var enemy_spatial: Variant = EnemySpatialHash.new(2.0)
var enemies: Array[EnemyState] = []
var towers: Array[TowerState] = []
var obstacles: Dictionary[Vector2i, bool] = {}
var shots: Array[Dictionary] = []
var visual_events: Array[Dictionary] = []
var base_health: float = 80.0
var credits: int = 150
var wave_index: int = -1
var current_wave_total: int = 0
var wave_definitions: Array = []
var pending_enemies: Array[Dictionary] = []
var spawn_timer: float = 0.0
var intermission: float = 0.8
var elapsed: float = 0.0
var enemies_defeated: int = 0
var finished: bool = false
var victory: bool = false

func _init(source_catalog: Variant = null, selected_map_id: String = "planicie_serena", selected_mode_id: String = "history", unlocked_tower_ids: PackedStringArray = PackedStringArray(), custom_map: Dictionary = {}, context: Dictionary = {}) -> void:
	catalog = source_catalog
	if catalog == null:
		catalog = DomainCatalog.new()
		catalog.load_default()
	mode_id = selected_mode_id
	run_context = context.duplicate(true)
	available_tower_ids = unlocked_tower_ids if not unlocked_tower_ids.is_empty() else PackedStringArray(catalog.towers.keys())
	if custom_map.is_empty():
		_configure_map(selected_map_id)
	else:
		_configure_custom_map(custom_map)
	_configure_mode(selected_mode_id)

func _configure_map(selected_map_id: String) -> void:
	custom_map_document = {}
	map_id = selected_map_id if catalog.maps.has(selected_map_id) else "planicie_serena"
	var map_data: Dictionary = WorldGrid.official_map(catalog.map_data(map_id))
	map_grid = map_data.get("grid", [])
	var portal: Dictionary = map_data.get("portal", {})
	var base: Dictionary = map_data.get("base", {})
	var portal_position: Dictionary = portal.get("position", {})
	var base_position: Dictionary = base.get("position", {})
	var start := Vector2i(floori(float(portal_position.get("x", 0.5))), floori(float(portal_position.get("y", 0.5))))
	var goal := Vector2i(floori(float(base_position.get("x", 0.5))), floori(float(base_position.get("y", 0.5))))
	path = MapPathfinder.find_path(map_grid, start, goal)
	base_health = float(base.get("health", 80.0))
	credits = int(base.get("credits", 150))

func _configure_custom_map(document: Dictionary) -> void:
	var normalized := WorldGrid.normalize_custom_document(document)
	var portal_data: Dictionary = normalized.get("portal", {})
	var base_data: Dictionary = normalized.get("base", {})
	var start := Vector2i(int(portal_data.get("x", -1)), int(portal_data.get("y", -1)))
	var goal := Vector2i(int(base_data.get("x", -1)), int(base_data.get("y", -1)))
	var candidate_grid: Array = normalized.get("grid", [])
	var candidate_path := MapPathfinder.find_path(candidate_grid, start, goal)
	if candidate_path.is_empty():
		_configure_map("planicie_serena")
		return
	map_id = "custom"
	custom_map_document = normalized.duplicate(true)
	map_grid = candidate_grid.duplicate(true)
	path = candidate_path
	base_health = 80.0
	credits = 150

func _configure_mode(selected_mode_id: String) -> void:
	mode_id = selected_mode_id if catalog.modes.has(selected_mode_id) else "history"
	tower_limit = int(TOWER_LIMITS.get(mode_id, TOWER_LIMITS.history))
	var mode: Dictionary = catalog.mode(mode_id)
	difficulty_profile = RunDifficultyProfile.create(mode_id, map_id, run_context)
	wave_definitions = RunDifficultyProfile.apply(catalog, mode.get("waves", []), difficulty_profile)
	base_health = float(mode.get("starting_health", base_health))
	credits = int(mode.get("starting_credits", credits))

func update(delta: float) -> void:
	if finished or delta <= 0.0:
		return
	elapsed += delta
	shots.clear()
	_update_visual_events(delta)
	_update_wave(delta)
	_update_enemies(delta)
	_update_towers(delta)
	_remove_defeated()
	if base_health <= 0.0:
		base_health = 0.0
		finished = true
		victory = false
	elif mode_id != "infinite" and wave_index == wave_definitions.size() - 1 and pending_enemies.is_empty() and enemies.is_empty():
		finished = true
		victory = true

func _update_wave(delta: float) -> void:
	if pending_enemies.is_empty():
		if not enemies.is_empty():
			return
		if wave_index >= wave_definitions.size() - 1:
			if mode_id == "infinite":
				wave_definitions.append(_infinite_wave(wave_index + 2))
			else:
				return
		intermission -= delta
		if intermission <= 0.0:
			wave_index += 1
			var wave: Dictionary = wave_definitions[wave_index]
			pending_enemies.assign(wave.get("enemies", []))
			current_wave_total = pending_enemies.size()
			spawn_timer = 0.0
			intermission = maxf(0.0, float(wave.get("entry_delay", 1.6)))
		return

	spawn_timer -= delta
	if spawn_timer <= 0.0:
		var instance: Dictionary = pending_enemies.pop_front()
		enemies.append(_create_enemy_from_instance(instance))
		var wave: Dictionary = wave_definitions[wave_index]
		spawn_timer = maxf(0.35, float(wave.get("cycle", 1.0)))

func _infinite_wave(wave_number: int) -> Dictionary:
	var references: Array = catalog.mode("infinite").get("infinite_reference_waves", [])
	if wave_number >= 1 and wave_number <= references.size():
		return references[wave_number - 1].duplicate(true)
	return _generate_infinite_wave(wave_number)

func _generate_infinite_wave(wave_number: int) -> Dictionary:
	return InfiniteWaveGenerator.generate(catalog, wave_number)

func _update_enemies(delta: float) -> void:
	for index in range(enemies.size() - 1, -1, -1):
		var enemy := enemies[index]
		var terrain_multiplier := 1.45 if _terrain_at(enemy.position) == "asphalt" else 1.0
		if obstacles.has(Vector2i(floori(enemy.position.x), floori(enemy.position.y))):
			terrain_multiplier *= 0.45
		var reached_base := EnemyRuntime.update(enemy, delta, path, terrain_multiplier, _passive_speed_multiplier(enemy), WorldGrid.DISTANCE_SCALE)
		if reached_base:
			base_health -= enemy.attack
			enemies.remove_at(index)

func _update_towers(delta: float) -> void:
	enemy_spatial.rebuild(enemies)
	for tower in towers:
		tower.cooldown = maxf(0.0, tower.cooldown - delta)
		if tower.cooldown > 0.0:
			continue
		var targets := _targets_for(tower)
		if targets.is_empty():
			continue
		for target in targets:
			_resolve_tower_hit(tower, target)
			shots.append({"from": tower.position, "to": target.position, "tower_id": tower.tower_id})
		tower.cooldown = tower.cycle

func _remove_defeated() -> void:
	for index in range(enemies.size() - 1, -1, -1):
		if enemies[index].health <= 0.0:
			credits += enemies[index].loot
			enemies_defeated += 1
			_add_visual_event("death", enemies[index].position, 0.55, float(enemies[index].loot))
			enemies.remove_at(index)

func build_tower(tower_id: String, cell: Vector2i) -> String:
	var spec: Dictionary = catalog.tower(tower_id)
	if spec.is_empty():
		return "Torre desconhecida"
	if tower_id not in available_tower_ids:
		return "Torre bloqueada"
	if not has_tower_capacity():
		return "Limite de " + str(tower_limit) + " torres atingido"
	if not is_buildable(cell):
		return "A celula precisa de relva livre"
	var price := tower_price(tower_id)
	if credits < price:
		return "Creditos insuficientes"
	credits -= price
	towers.append(TowerState.new(tower_id, Vector2(cell) + Vector2(0.5, 0.5), spec, price))
	_add_visual_event("build", Vector2(cell) + Vector2(0.5, 0.5), 0.7)
	return ""

func tower_price(tower_id: String) -> int:
	var base_price := int(catalog.tower(tower_id).get("price", 0))
	return TowerEconomy.purchase_price(base_price, mode_id)

func is_buildable(cell: Vector2i) -> bool:
	if not has_tower_capacity():
		return false
	if cell.y < 0 or cell.y >= map_grid.size():
		return false
	var row: Variant = map_grid[cell.y]
	return row is Array and cell.x >= 0 and cell.x < row.size() and row[cell.x] == "grass" and tower_at(cell) < 0

func has_tower_capacity() -> bool:
	return towers.size() < tower_limit

func tower_occupied_cells(center_cell: Vector2i) -> Array[Vector2i]:
	return [center_cell]

func place_obstacle(cell: Vector2i) -> String:
	if obstacles.size() >= 6:
		return "Limite de 6 obstaculos atingido"
	if cell.y < 0 or cell.y >= map_grid.size():
		return "Posicao invalida"
	var row: Variant = map_grid[cell.y]
	if not row is Array or cell.x < 0 or cell.x >= row.size() or row[cell.x] not in ["path", "asphalt"]:
		return "Coloca o obstaculo sobre a estrada"
	var path_start := Vector2i(path[0])
	var path_end := Vector2i(path[path.size() - 1])
	if cell == path_start or cell == path_end:
		return "Portal e base nao podem ser bloqueados"
	if obstacles.has(cell):
		return "Ja existe um obstaculo nessa celula"
	obstacles[cell] = true
	_add_visual_event("obstacle", Vector2(cell) + Vector2(0.5, 0.5), 0.65)
	return ""

func tower_at(cell: Vector2i) -> int:
	for index in towers.size():
		var center_cell := Vector2i(floori(towers[index].position.x), floori(towers[index].position.y))
		if cell == center_cell:
			return index
	return -1

func world_distance(catalog_distance: float) -> float:
	return catalog_distance * WorldGrid.DISTANCE_SCALE

func upgrade_cost(tower_index: int, specialization_choice: String = "") -> int:
	if tower_index < 0 or tower_index >= towers.size():
		return -1
	return TowerEconomy.upgrade_cost(towers[tower_index], specialization_choice)

func upgrade_preview(tower_index: int, specialization_choice: String = "") -> Dictionary:
	var cost := upgrade_cost(tower_index, specialization_choice)
	if cost < 0 or tower_index < 0 or tower_index >= towers.size():
		return {}
	var tower := towers[tower_index]
	var specialization := tower.specialization
	if tower.level >= 3 and specialization.is_empty():
		specialization = specialization_choice
	var next_damage := tower.damage * 1.28 + 4.0
	var next_range := tower.range_cells + 0.28
	var next_burst := tower.burst
	var next_cycle := tower.cycle
	if tower.damage > 38.0:
		next_burst = mini(5, next_burst + 1)
		next_cycle = maxf(0.4, next_cycle * 0.9)
	if specialization == "a":
		next_damage *= 1.12
		next_range += 0.18
	elif specialization == "b":
		next_cycle = maxf(0.32, next_cycle * 0.86)
		next_burst = mini(6, next_burst + 1)
	return {"cost": cost, "level": tower.level + 1, "damage": next_damage, "range": next_range, "burst": next_burst, "cycle": next_cycle, "specialization": specialization}

func upgrade_tower(tower_index: int, specialization_choice: String = "") -> String:
	var cost := upgrade_cost(tower_index, specialization_choice)
	if cost < 0:
		return "Upgrade indisponivel"
	if credits < cost:
		return "Creditos insuficientes"
	var tower := towers[tower_index]
	credits -= cost
	tower.upgrade_investment_paid += cost
	tower.investment_paid += cost
	_apply_upgrade_stats(tower, specialization_choice)
	_add_visual_event("upgrade", tower.position, 0.75, float(tower.level))
	return ""

func _apply_upgrade_stats(tower: TowerState, specialization_choice: String = "") -> void:
	if tower.level >= 3 and tower.specialization.is_empty():
		tower.specialization = specialization_choice
	var old_damage := tower.damage
	tower.damage = tower.damage * 1.28 + 4.0
	tower.range_cells += 0.28
	if old_damage > 38.0:
		tower.burst = mini(5, tower.burst + 1)
	tower.cycle = maxf(0.4, tower.cycle * 0.9)
	tower.cooldown = minf(tower.cooldown, tower.cycle)
	tower.projectile_duration = tower.projectile_duration * 1.18 + 0.3
	if tower.specialization == "a":
		tower.damage *= 1.12
		tower.range_cells += 0.18
	elif tower.specialization == "b":
		tower.cycle = maxf(0.32, tower.cycle * 0.86)
		tower.burst = mini(6, tower.burst + 1)
	tower.level += 1

func sell_tower(tower_index: int) -> int:
	if tower_index < 0 or tower_index >= towers.size():
		return 0
	var tower := towers[tower_index]
	var sale_value := TowerEconomy.sale_value(tower)
	credits += sale_value
	_add_visual_event("sell", tower.position, 0.6, float(sale_value))
	towers.remove_at(tower_index)
	return sale_value

func _advance_enemy(enemy: EnemyState, distance: float) -> bool:
	return EnemyRuntime.advance(enemy, world_distance(distance), path, world_distance(0.5))

func _targets_for(tower: TowerState) -> Array[EnemyState]:
	var candidates: Array[EnemyState] = []
	var world_range := world_distance(tower.range_cells)
	var range_squared := world_range * world_range
	for enemy: Variant in enemy_spatial.query(tower.position, world_range):
		if enemy.health <= 0.0 or tower.position.distance_squared_to(enemy.position) > range_squared:
			continue
		candidates.append(enemy)
	var center := Vector2.ZERO
	if not candidates.is_empty():
		for enemy in candidates:
			center += enemy.position
		center /= float(candidates.size())
	candidates.sort_custom(func(left: EnemyState, right: EnemyState) -> bool: return _target_before(tower, center, left, right))
	return candidates.slice(0, mini(tower.burst, candidates.size()))

func _target_before(tower: TowerState, center: Vector2, left: EnemyState, right: EnemyState) -> bool:
	var base_position := path[path.size() - 1]
	match tower.priority:
		"MaisRapido":
			return left.speed > right.speed if left.speed != right.speed else left.position.distance_squared_to(base_position) < right.position.distance_squared_to(base_position)
		"MaisVida":
			return left.health > right.health if left.health != right.health else left.position.distance_squared_to(base_position) < right.position.distance_squared_to(base_position)
		"MaiorGrupo":
			return left.position.distance_squared_to(center) < right.position.distance_squared_to(center) if left.position.distance_squared_to(center) != right.position.distance_squared_to(center) else left.position.distance_squared_to(base_position) < right.position.distance_squared_to(base_position)
		_:
			return left.position.distance_squared_to(base_position) < right.position.distance_squared_to(base_position)

func _resolve_tower_hit(tower: TowerState, enemy: EnemyState) -> void:
	var damage := CombatResolver.resolve_hit(tower, enemy, _boss_context_multiplier(tower, enemy))
	_add_visual_event("damage", enemy.position, 0.7, damage)

func _add_visual_event(kind: String, position: Vector2, duration: float, value: float = 0.0) -> void:
	visual_events.append({"kind": kind, "position": position, "age": 0.0, "duration": duration, "value": value})
	if visual_events.size() > 64:
		visual_events.pop_front()

func _update_visual_events(delta: float) -> void:
	for index in range(visual_events.size() - 1, -1, -1):
		visual_events[index]["age"] = float(visual_events[index].get("age", 0.0)) + delta
		if float(visual_events[index].get("age")) >= float(visual_events[index].get("duration", 0.0)):
			visual_events.remove_at(index)

func _passive_speed_multiplier(enemy: EnemyState) -> float:
	var health_ratio := enemy.health / enemy.max_health
	if enemy.class_id == "boss_acelerador":
		return 2.1 if health_ratio <= 0.3 else (1.55 if health_ratio <= 0.65 else 1.0)
	if enemy.class_id == "elite" and health_ratio <= 0.35:
		return 1.22
	return 1.0

func _boss_context_multiplier(tower: TowerState, target: EnemyState) -> float:
	var multiplier := 1.0
	if target.class_id != "boss_guardiao" and _has_enemy_class_near("boss_guardiao", target.position, world_distance(3.2)):
		multiplier *= 0.58
	if _has_enemy_class_near("boss_ruptura", tower.position, world_distance(4.5)):
		multiplier *= 0.7
	return multiplier

func _has_enemy_class_near(class_id: String, position: Vector2, radius: float) -> bool:
	var radius_squared := radius * radius
	for candidate: Variant in enemy_spatial.query(position, radius):
		if candidate.health > 0.0 and candidate.class_id == class_id and candidate.position.distance_squared_to(position) <= radius_squared:
			return true
	return false

func _apply_projectile(enemy: EnemyState, projectile_type: String, duration: float) -> void:
	ProjectileEffects.apply(enemy, projectile_type, duration)

func _retreat_enemy(enemy: EnemyState, distance: float) -> void:
	EnemyRuntime.retreat(enemy, distance, path)

func _terrain_at(position: Vector2) -> String:
	var cell := Vector2i(floori(position.x), floori(position.y))
	if cell.y < 0 or cell.y >= map_grid.size() or not map_grid[cell.y] is Array:
		return ""
	var row: Array = map_grid[cell.y]
	return str(row[cell.x]) if cell.x >= 0 and cell.x < row.size() else ""

func _create_enemy(class_id: String, level: int) -> EnemyState:
	var stable_id := class_id if catalog.enemies.has(class_id) else "basico"
	return EnemyState.new(stable_id, path[0], catalog.enemy(stable_id), EnemyScaling.scaled_instance(catalog, stable_id, level))

func _create_enemy_from_instance(instance: Dictionary) -> EnemyState:
	var class_id := str(instance.get("class_id", "basico"))
	var stable_id := class_id if catalog.enemies.has(class_id) else "basico"
	return EnemyState.new(stable_id, path[0], catalog.enemy(stable_id), instance)

func snapshot() -> Dictionary:
	var tower_values: Array[Dictionary] = []
	for tower in towers:
		tower_values.append(_tower_snapshot(tower))
	var enemy_values: Array[Dictionary] = []
	for enemy in enemies:
		enemy_values.append(_enemy_snapshot(enemy))
	return {
		"schema": "immutable-towers-run",
		"version": 1,
		"world_grid_scale": WorldGrid.SOURCE_CELLS_PER_CELL,
		"map_id": map_id,
		"custom_map": custom_map_document.duplicate(true),
		"mode_id": mode_id,
		"run_context": run_context.duplicate(true),
		"difficulty_profile": difficulty_profile.duplicate(true),
		"available_tower_ids": Array(available_tower_ids),
		"base_health": base_health,
		"credits": credits,
		"wave_index": wave_index,
		"current_wave_total": current_wave_total,
		"wave_definitions": wave_definitions.duplicate(true),
		"pending_enemies": pending_enemies.duplicate(true),
		"spawn_timer": spawn_timer,
		"intermission": intermission,
		"elapsed": elapsed,
		"enemies_defeated": enemies_defeated,
		"finished": finished,
		"victory": victory,
		"obstacles": obstacles.keys().map(func(cell: Vector2i) -> Dictionary: return {"x": cell.x, "y": cell.y}),
		"towers": tower_values,
		"enemies": enemy_values
	}

func restore(document: Variant) -> PackedStringArray:
	var restore_errors := PackedStringArray()
	if not document is Dictionary:
		restore_errors.append("run: root must be an object")
		return restore_errors
	var state: Dictionary = document
	if state.get("schema", "") != "immutable-towers-run" or int(state.get("version", 0)) != 1:
		restore_errors.append("run: unsupported schema or version")
		return restore_errors
	var legacy_coordinates := int(state.get("world_grid_scale", 1)) != WorldGrid.SOURCE_CELLS_PER_CELL
	var restored_pending: Variant = state.get("pending_enemies")
	if not restored_pending is Array and state.get("pending_classes") is Array:
		restored_pending = []
		for class_id: Variant in state.get("pending_classes", []):
			restored_pending.append(EnemyScaling.scaled_instance(catalog, str(class_id), maxi(1, int(state.get("wave_index", -1)) + 1)))
	if not state.get("towers") is Array or not state.get("enemies") is Array or not restored_pending is Array:
		restore_errors.append("run: invalid collections")
		return restore_errors
	if str(state.get("map_id", "planicie_serena")) == "custom" and state.get("custom_map") is Dictionary:
		_configure_custom_map(state.get("custom_map"))
	else:
		_configure_map(str(state.get("map_id", "planicie_serena")))
	run_context = state.get("run_context", {}).duplicate(true) if state.get("run_context") is Dictionary else {}
	_configure_mode(str(state.get("mode_id", "history")))
	if state.get("difficulty_profile") is Dictionary:
		difficulty_profile = state.get("difficulty_profile", {}).duplicate(true)
	if state.get("wave_definitions") is Array:
		wave_definitions = state.get("wave_definitions", []).duplicate(true)
	available_tower_ids = PackedStringArray(state.get("available_tower_ids", available_tower_ids))
	obstacles.clear()
	for value: Variant in state.get("obstacles", []):
		if value is Dictionary:
			var cell := Vector2i(int(value.get("x", -1)), int(value.get("y", -1)))
			if legacy_coordinates:
				cell = WorldGrid.source_cell_to_world(cell)
			if cell.y >= 0 and cell.y < map_grid.size() and cell.x >= 0 and cell.x < map_grid[cell.y].size() and map_grid[cell.y][cell.x] in ["path", "asphalt"] and cell != Vector2i(path[0]) and cell != Vector2i(path[path.size() - 1]):
				obstacles[cell] = true
	base_health = maxf(0.0, float(state.get("base_health", 80.0)))
	credits = maxi(0, int(state.get("credits", 0)))
	wave_index = maxi(-1, int(state.get("wave_index", -1)))
	current_wave_total = maxi(0, int(state.get("current_wave_total", restored_pending.size() + state.get("enemies", []).size())))
	pending_enemies.assign(restored_pending)
	spawn_timer = maxf(0.0, float(state.get("spawn_timer", 0.0)))
	intermission = maxf(0.0, float(state.get("intermission", 0.0)))
	elapsed = maxf(0.0, float(state.get("elapsed", 0.0)))
	enemies_defeated = maxi(0, int(state.get("enemies_defeated", 0)))
	finished = bool(state.get("finished", false))
	victory = bool(state.get("victory", false))
	towers.clear()
	for value: Variant in state.get("towers", []):
		var restored_tower := _restore_tower(value, legacy_coordinates)
		if restored_tower == null:
			restore_errors.append("run: invalid tower")
		else:
			towers.append(restored_tower)
	enemies.clear()
	for value: Variant in state.get("enemies", []):
		var restored_enemy := _restore_enemy(value, legacy_coordinates)
		if restored_enemy == null:
			restore_errors.append("run: invalid enemy")
		else:
			enemies.append(restored_enemy)
	return restore_errors

func _tower_snapshot(tower: TowerState) -> Dictionary:
	return {
		"tower_id": tower.tower_id,
		"position": {"x": tower.position.x, "y": tower.position.y},
		"damage": tower.damage,
		"range": tower.range_cells,
		"cycle": tower.cycle,
		"area": tower.area,
		"burst": tower.burst,
		"projectile_type": tower.projectile_type,
		"projectile_duration": tower.projectile_duration,
		"level": tower.level,
		"max_level": tower.max_level,
		"purchase_price": tower.purchase_price,
		"purchase_price_paid": tower.purchase_price_paid,
		"upgrade_investment_paid": tower.upgrade_investment_paid,
		"investment_paid": tower.investment_paid,
		"specialization": tower.specialization,
		"cooldown": tower.cooldown
	}

func _enemy_snapshot(enemy: EnemyState) -> Dictionary:
	return {
		"class_id": enemy.class_id,
		"position": {"x": enemy.position.x, "y": enemy.position.y},
		"segment": enemy.segment,
		"health": enemy.health,
		"max_health": enemy.max_health,
		"speed": enemy.speed,
		"attack": enemy.attack,
		"loot": enemy.loot,
		"armor": enemy.armor,
		"direct_resistance": enemy.direct_resistance,
		"area_resistance": enemy.area_resistance,
		"regeneration": enemy.regeneration,
		"shield": enemy.shield,
		"effects": enemy.effects.duplicate(),
		"hit_flash": enemy.hit_flash,
		"hard_cc_exposure": enemy.hard_cc_exposure,
		"cc_window_remaining": enemy.cc_window_remaining
	}

func _restore_tower(value: Variant, legacy_coordinates: bool = false) -> TowerState:
	if not value is Dictionary:
		return null
	var state: Dictionary = value
	var tower_id: String = state.get("tower_id", "")
	var position: Vector2 = _position_from(state.get("position"))
	if not position.is_finite():
		return null
	if legacy_coordinates:
		position = WorldGrid.source_position_to_cell_center(position)
	if not catalog.towers.has(tower_id):
		return null
	var legacy_price := mini(int(state.get("purchase_price", tower_price(tower_id))), tower_price(tower_id))
	var paid_price := maxi(0, int(state.get("purchase_price_paid", legacy_price)))
	var tower := TowerState.new(tower_id, position, catalog.tower(tower_id), paid_price)
	tower.damage = maxf(0.0, float(state.get("damage", tower.damage)))
	tower.range_cells = maxf(0.0, float(state.get("range", tower.range_cells)))
	tower.cycle = maxf(0.01, float(state.get("cycle", tower.cycle)))
	tower.area = maxf(0.0, float(state.get("area", tower.area)))
	tower.burst = maxi(1, int(state.get("burst", tower.burst)))
	tower.projectile_type = str(state.get("projectile_type", tower.projectile_type))
	tower.projectile_duration = maxf(0.0, float(state.get("projectile_duration", tower.projectile_duration)))
	tower.level = clampi(int(state.get("level", 1)), 1, tower.max_level)
	tower.specialization = str(state.get("specialization", ""))
	tower.upgrade_investment_paid = maxi(0, int(state.get("upgrade_investment_paid", _estimate_legacy_upgrade_investment(tower_id, tower.level, tower.specialization))))
	tower.investment_paid = maxi(tower.purchase_price_paid, int(state.get("investment_paid", tower.purchase_price_paid + tower.upgrade_investment_paid)))
	tower.cooldown = maxf(0.0, float(state.get("cooldown", 0.0)))
	return tower

func _estimate_legacy_upgrade_investment(tower_id: String, target_level: int, specialization: String) -> int:
	var estimate := TowerState.new(tower_id, Vector2.ZERO, catalog.tower(tower_id), tower_price(tower_id))
	var investment := 0
	while estimate.level < target_level:
		var choice := specialization if estimate.level >= 3 and estimate.specialization.is_empty() else ""
		var cost := TowerEconomy.upgrade_cost(estimate, choice)
		if cost < 0:
			break
		investment += cost
		_apply_upgrade_stats(estimate, choice)
	return investment

func _restore_enemy(value: Variant, legacy_coordinates: bool = false) -> EnemyState:
	if not value is Dictionary:
		return null
	var state: Dictionary = value
	var class_id: String = state.get("class_id", "")
	var position: Vector2 = _position_from(state.get("position"))
	if not position.is_finite():
		return null
	var migrated_segment := -1
	if legacy_coordinates:
		position = WorldGrid.source_position_to_cell_center(position)
		migrated_segment = _nearest_path_index(position)
		if migrated_segment >= 0:
			position = path[migrated_segment]
	if not catalog.enemies.has(class_id):
		return null
	var enemy := EnemyState.new(class_id, position, catalog.enemy(class_id))
	enemy.position = position
	enemy.segment = migrated_segment if migrated_segment >= 0 else clampi(int(state.get("segment", 0)), 0, path.size() - 1)
	enemy.health = maxf(0.0, float(state.get("health", enemy.health)))
	enemy.max_health = maxf(0.01, float(state.get("max_health", enemy.max_health)))
	enemy.speed = maxf(0.0, float(state.get("speed", enemy.speed)))
	enemy.attack = maxf(0.0, float(state.get("attack", enemy.attack)))
	enemy.loot = maxi(0, int(state.get("loot", enemy.loot)))
	enemy.armor = maxf(0.0, float(state.get("armor", enemy.armor)))
	enemy.direct_resistance = clampf(float(state.get("direct_resistance", enemy.direct_resistance)), 0.0, 1.0)
	enemy.area_resistance = clampf(float(state.get("area_resistance", enemy.area_resistance)), 0.0, 1.0)
	enemy.regeneration = maxf(0.0, float(state.get("regeneration", enemy.regeneration)))
	enemy.shield = clampf(float(state.get("shield", enemy.shield)), 0.0, 1.0)
	enemy.hit_flash = maxf(0.0, float(state.get("hit_flash", 0.0)))
	enemy.hard_cc_exposure = maxf(0.0, float(state.get("hard_cc_exposure", 0.0)))
	enemy.cc_window_remaining = maxf(0.0, float(state.get("cc_window_remaining", 0.0)))
	var effect_values: Variant = state.get("effects", {})
	if effect_values is Dictionary:
		for effect_id: Variant in effect_values:
			var stable_effect_id := str(effect_id)
			if stable_effect_id in ["fire", "ice", "resin", "fear", "poison", "electric"]:
				enemy.effects[stable_effect_id] = maxf(0.0, float(effect_values[effect_id]))
	return enemy

func _nearest_path_index(position: Vector2) -> int:
	if path.is_empty():
		return -1
	var best_index := 0
	var best_distance := INF
	for index in path.size():
		var distance := path[index].distance_squared_to(position)
		if distance < best_distance:
			best_distance = distance
			best_index = index
	return best_index

func _position_from(value: Variant) -> Vector2:
	if not value is Dictionary:
		return Vector2(INF, INF)
	var position: Dictionary = value
	return Vector2(float(position.get("x", 0.0)), float(position.get("y", 0.0)))
