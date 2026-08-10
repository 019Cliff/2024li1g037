class_name BotRollout
extends RefCounted

const MAX_CANDIDATES := 12
const HORIZON_SECONDS := 10.0

static func rank(simulation: Variant, candidates: Array[Dictionary], threats: Dictionary[String, int]) -> Array[Dictionary]:
	var threat_pool := _threat_pool(simulation, threats)
	var ranked: Array[Dictionary] = []
	for candidate: Dictionary in candidates:
		var evaluated := candidate.duplicate(true)
		var projection := _project_action(simulation, evaluated, threat_pool)
		evaluated["score"] = float(evaluated.get("score", 0.0)) + projection * 0.28
		var breakdown: Dictionary = evaluated.get("breakdown", {}).duplicate(true)
		breakdown["previsao"] = projection
		evaluated["breakdown"] = breakdown
		ranked.append(evaluated)
	ranked.sort_custom(_decision_before)
	return ranked

static func _project_action(simulation: Variant, candidate: Dictionary, threat_pool: Dictionary) -> float:
	var added_dps := _added_dps(simulation, candidate, int(threat_pool.get("count", 1)))
	var coverage := clampf(float(candidate.get("breakdown", {}).get("cobertura_nova", 0.35)), 0.15, 1.0)
	if candidate.get("kind") == "upgrade":
		coverage = 0.85
	var effective_damage := added_dps * HORIZON_SECONDS * coverage
	var total_health := maxf(1.0, float(threat_pool.get("health", 1.0)))
	var prevented_pressure := minf(1.0, effective_damage / total_health)
	var income_unlocked := float(threat_pool.get("loot", 0)) * prevented_pressure
	var cost := maxi(0, int(candidate.get("cost", 0)))
	var reserve_after_action: int = int(simulation.credits) - cost
	var reserve_penalty := 0.0
	if reserve_after_action < 20 and not simulation.enemies.is_empty():
		reserve_penalty = 0.18
	var base_pressure := clampf(float(threat_pool.get("attack", 0.0)) / maxf(1.0, simulation.base_health * 2.0), 0.0, 1.0)
	var damage_efficiency := minf(0.3, added_dps / float(maxi(1, cost)) * 0.9)
	return prevented_pressure * (0.72 + base_pressure * 0.38) + damage_efficiency + minf(0.25, income_unlocked / 180.0) - reserve_penalty

static func _added_dps(simulation: Variant, candidate: Dictionary, threat_count: int) -> float:
	if candidate.get("kind") == "upgrade":
		return maxf(0.0, float(candidate.get("breakdown", {}).get("ganho_dps", 0.0)))
	var tower_id := str(candidate.get("tower_id", ""))
	var spec: Dictionary = simulation.catalog.tower(tower_id)
	var base: Dictionary = spec.get("base", {})
	var raw_dps := float(base.get("damage", 0.0)) * float(base.get("burst", 1)) / maxf(0.1, float(base.get("cycle", 1.0)))
	var area_attack: bool = float(spec.get("area", 0.0)) > 0.0 or "Area" in spec.get("tags", []) or "Chain" in spec.get("tags", [])
	var crowd_multiplier := 1.0 + minf(0.4, float(maxi(0, threat_count - 1)) * 0.05) if area_attack else 1.0
	return raw_dps * crowd_multiplier

static func _threat_pool(simulation: Variant, threats: Dictionary[String, int]) -> Dictionary:
	var health := 0.0
	var attack := 0.0
	var loot := 0
	var enemy_count := 0
	for enemy: Variant in simulation.enemies:
		health += maxf(0.0, enemy.health)
		attack += maxf(0.0, enemy.attack)
		loot += maxi(0, enemy.loot)
		enemy_count += 1
	for instance: Dictionary in simulation.pending_enemies:
		var class_id := str(instance.get("class_id", "basico"))
		var spec: Dictionary = simulation.catalog.enemy(class_id)
		health += float(instance.get("health", spec.get("health", 1.0)))
		attack += float(instance.get("attack", spec.get("attack", 1.0)))
		loot += int(instance.get("loot", spec.get("loot", 0)))
		enemy_count += 1
	if health <= 0.0:
		for class_id: String in threats:
			var spec: Dictionary = simulation.catalog.enemy(class_id)
			var class_count := int(threats[class_id])
			health += float(spec.get("health", 1.0)) * class_count
			attack += float(spec.get("attack", 1.0)) * class_count
			loot += int(spec.get("loot", 0)) * class_count
			enemy_count += class_count
	return {"health": health, "attack": attack, "loot": loot, "count": enemy_count}

static func _decision_before(left: Dictionary, right: Dictionary) -> bool:
	var left_score := float(left.get("score", 0.0))
	var right_score := float(right.get("score", 0.0))
	if not is_equal_approx(left_score, right_score):
		return left_score > right_score
	return _decision_key(left) < _decision_key(right)

static func _decision_key(decision: Dictionary) -> String:
	return str(decision.get("kind", "")) + ":" + str(decision.get("tower_id", "")) + ":" + str(decision.get("cell", "")) + ":" + str(decision.get("tower_index", -1)) + ":" + str(decision.get("specialization", ""))
