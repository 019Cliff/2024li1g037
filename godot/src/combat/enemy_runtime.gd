class_name EnemyRuntime
extends RefCounted

const CrowdControlPolicy = preload("res://src/combat/crowd_control_policy.gd")

static func update(enemy: Variant, delta: float, path: Array[Vector2], terrain_multiplier: float, passive_speed_multiplier: float, movement_scale: float = 1.0) -> bool:
	enemy.hit_flash = maxf(0.0, enemy.hit_flash - delta * 5.0)
	enemy.health = minf(enemy.max_health, enemy.health + enemy.regeneration * delta)
	var expired := PackedStringArray()
	if not enemy.effects.is_empty():
		for effect_id: String in enemy.effects:
			enemy.effects[effect_id] = maxf(0.0, enemy.effects[effect_id] - delta)
			if enemy.effects[effect_id] <= 0.0:
				expired.append(effect_id)
	CrowdControlPolicy.update_recovery(enemy, delta)
	var continuous_damage := (2.0 if enemy.effects.has("fire") else 0.0) + (3.2 if enemy.effects.has("poison") else 0.0)
	enemy.health = maxf(0.0, enemy.health - continuous_damage * delta)
	for effect_id: String in expired:
		enemy.effects.erase(effect_id)
	var move_distance := 0.0
	if not enemy.effects.has("electric"):
		move_distance = enemy.speed * passive_speed_multiplier * CrowdControlPolicy.movement_multiplier(enemy) * terrain_multiplier * delta * movement_scale
	var reached_base := false
	if CrowdControlPolicy.fear_causes_retreat(enemy):
		retreat(enemy, move_distance, path)
	else:
		reached_base = advance(enemy, move_distance, path, 0.5 * movement_scale)
	return reached_base

static func advance(enemy: Variant, distance: float, path: Array[Vector2], reach_radius: float = 0.5) -> bool:
	var base_position := path[path.size() - 1]
	var reach_radius_squared := reach_radius * reach_radius
	if enemy.position.distance_squared_to(base_position) < reach_radius_squared:
		return true
	var remaining := maxf(0.0, distance)
	while remaining > 0.0 and enemy.segment < path.size() - 1:
		var target := path[enemy.segment + 1]
		var to_target: Vector2 = target - enemy.position
		var segment_distance: float = to_target.length()
		if segment_distance <= remaining + 0.0001:
			enemy.position = target
			enemy.segment += 1
			remaining -= segment_distance
		else:
			enemy.position += to_target / segment_distance * remaining
			remaining = 0.0
	return enemy.segment >= path.size() - 1 or enemy.position.distance_squared_to(base_position) < reach_radius_squared

static func retreat(enemy: Variant, distance: float, path: Array[Vector2]) -> void:
	var remaining := maxf(0.0, distance)
	while remaining > 0.0:
		var target := path[enemy.segment]
		var to_target: Vector2 = target - enemy.position
		var segment_distance: float = to_target.length()
		if segment_distance <= remaining + 0.0001:
			enemy.position = target
			remaining -= segment_distance
			if enemy.segment == 0:
				return
			enemy.segment -= 1
		else:
			enemy.position += to_target / segment_distance * remaining
			return
