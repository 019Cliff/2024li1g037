class_name VerticalSliceBot
extends RefCounted

const DomainIds = preload("res://src/domain/domain_ids.gd")
const BotRollout = preload("res://src/bot/bot_rollout.gd")

static func decide(simulation: Variant, memory: Variant = null) -> Dictionary:
	if simulation.available_tower_ids.size() <= 1:
		var restricted := _decide_legacy(simulation)
		restricted["reason"] = str(restricted.get("reason", "")) + "\nArsenal atual: apenas SENTINELA"
		return restricted
	return _decide_strategic(simulation, memory if memory is Dictionary else {})

static func _decide_legacy(simulation: Variant) -> Dictionary:
	var threats := _visible_threats(simulation)
	var candidates: Array[Dictionary] = []
	for tower_id: String in DomainIds.TOWERS:
		if tower_id not in simulation.available_tower_ids:
			continue
		var spec: Dictionary = simulation.catalog.tower(tower_id)
		var price: int = simulation.tower_price(tower_id)
		if price > simulation.credits:
			continue
		for position: Vector2i in _best_positions(simulation, spec, 8):
			var coverage := _coverage(simulation, spec, position)
			var combat_value := _counter_value(simulation, spec, threats)
			var emergency := _emergency_value(simulation, spec, position, coverage)
			var waste := _range_waste(simulation, spec, position)
			var score := 0.55 * coverage + 0.25 * combat_value + 0.12 * emergency - 0.08 * waste + 0.23 * (coverage + combat_value) / float(maxi(1, price))
			candidates.append({
				"kind": "build",
				"tower_id": tower_id,
				"cell": position,
				"score": score,
				"reason": "Construir " + str(spec.get("name", tower_id)) + " em " + str(position) + " para cobrir a rota"
			})
	for tower_index in simulation.towers.size():
		var tower: Variant = simulation.towers[tower_index]
		if tower.level >= 3 and tower.specialization.is_empty():
			for specialization in ["a", "b"]:
				var specialization_cost: int = simulation.upgrade_cost(tower_index, specialization)
				if specialization_cost > 0 and specialization_cost <= simulation.credits:
					candidates.append({
						"kind": "upgrade",
						"tower_index": tower_index,
						"specialization": specialization,
						"score": tower.damage / float(maxi(1, specialization_cost)),
						"reason": "Especializar " + tower.tower_id.to_upper() + " em " + ("POTENCIA" if specialization == "a" else "CADENCIA")
					})
		else:
			var cost: int = simulation.upgrade_cost(tower_index)
			if cost > 0 and cost <= simulation.credits:
				candidates.append({
					"kind": "upgrade",
					"tower_index": tower_index,
					"specialization": "",
					"score": tower.damage / float(maxi(1, cost)),
					"reason": "Melhorar " + tower.tower_id.to_upper() + " pelo melhor ganho por credito"
				})
	candidates.sort_custom(_decision_before)
	if candidates.is_empty():
		return {"kind": "save", "score": 0.05, "reason": "Poupar creditos para uma resposta melhor", "alternatives": []}
	var decision: Dictionary = candidates[0].duplicate(true)
	decision["alternatives"] = candidates.slice(1, mini(6, candidates.size()))
	return decision

static func _decide_strategic(simulation: Variant, memory: Dictionary) -> Dictionary:
	var threats := _visible_threats(simulation)
	var all_candidates: Array[Dictionary] = []
	var saturation := float(simulation.towers.size()) / float(maxi(1, simulation.tower_limit))
	var control_tower_count := _control_tower_count(simulation)
	var control_budget := maxi(1, ceili(float(simulation.towers.size() + 1) * 0.25))
	var terminal_defenders := _terminal_defender_count(simulation)
	for tower_id: String in DomainIds.TOWERS:
		if tower_id not in simulation.available_tower_ids:
			continue
		var spec: Dictionary = simulation.catalog.tower(tower_id)
		var price: int = simulation.tower_price(tower_id)
		for position: Vector2i in _best_positions_strategic(simulation, spec, 10):
			var metrics := _position_metrics(simulation, spec, position)
			var combat := _combat_utility(simulation, spec, threats)
			var efficiency := combat * 60.0 / float(maxi(1, price))
			var raw_dps := float(spec.get("base", {}).get("damage", 0.0)) * float(spec.get("base", {}).get("burst", 1)) / maxf(0.1, float(spec.get("base", {}).get("cycle", 1.0)))
			var damage_efficiency := raw_dps * 45.0 / float(maxi(1, price))
			var emergency := _emergency_value(simulation, spec, position, float(metrics.coverage))
			var emergency_weight := 0.38 if simulation.towers.size() < 3 and terminal_defenders < 2 else 0.10
			var score := (
				combat * 0.42
				+ float(metrics.marginal) * 0.34
				+ float(metrics.coverage) * 0.12
				+ emergency * emergency_weight
				+ efficiency * 0.12
				+ damage_efficiency * 0.24
				- float(metrics.redundancy) * 0.20
				- _range_waste(simulation, spec, position) * 0.06
			) * (1.0 - saturation * 0.42)
			if _is_control_first_tower(spec) and control_tower_count >= control_budget:
				score *= 0.58
			var startup_reserve_penalty := 0.0
			if simulation.towers.size() < 3:
				var remaining_bootstrap_slots: int = 3 - int(simulation.towers.size())
				var maximum_bootstrap_spend: int = floori(float(simulation.credits) / float(remaining_bootstrap_slots))
				if price > maximum_bootstrap_spend:
					score *= 0.24
					startup_reserve_penalty = -0.76
			all_candidates.append({
				"kind": "build",
				"tower_id": tower_id,
				"cell": position,
				"cost": price,
				"score": score,
				"label": "Construir " + str(spec.get("name", tower_id)),
				"reason": "Construir " + str(spec.get("name", tower_id)) + " para responder a " + _primary_threat_label(simulation, threats),
				"breakdown": {
					"combate": combat,
					"dano_credito": damage_efficiency,
					"cobertura_nova": metrics.marginal,
					"emergencia": emergency,
					"reserva_inicial": startup_reserve_penalty,
					"redundancia": -float(metrics.redundancy)
				}
			})
	for tower_index in simulation.towers.size():
		var tower: Variant = simulation.towers[tower_index]
		var choices := ["a", "b"] if tower.level >= 3 and tower.specialization.is_empty() else [""]
		for specialization: String in choices:
			var preview: Dictionary = simulation.upgrade_preview(tower_index, specialization)
			if preview.is_empty():
				continue
			var current_dps: float = tower.damage * float(tower.burst) / maxf(0.1, tower.cycle)
			var next_dps: float = float(preview.get("damage", tower.damage)) * float(preview.get("burst", tower.burst)) / maxf(0.1, float(preview.get("cycle", tower.cycle)))
			var delta_dps: float = maxf(0.0, next_dps - current_dps)
			var delta_range: float = maxf(0.0, float(preview.get("range", tower.range_cells)) - tower.range_cells)
			var spec: Dictionary = simulation.catalog.tower(tower.tower_id)
			var counter := _combat_utility(simulation, spec, threats)
			var marginal_gain := delta_dps / 22.0 + delta_range * 0.16
			var cost := int(preview.get("cost", -1))
			var score := marginal_gain * (0.8 + counter * 0.35) * 70.0 / float(maxi(1, cost)) * (0.85 + saturation * 0.95)
			all_candidates.append({
				"kind": "upgrade",
				"tower_index": tower_index,
				"specialization": specialization,
				"cost": cost,
				"score": score,
				"label": "Melhorar " + tower.tower_id.to_upper(),
				"reason": "Melhorar " + tower.tower_id.to_upper() + " por +" + str(snappedf(delta_dps, 0.1)) + " DPS",
				"breakdown": {
					"ganho_dps": delta_dps,
					"ganho_alcance": delta_range,
					"counter": counter,
					"custo": -float(cost) / 100.0
				}
			})
	all_candidates.sort_custom(_decision_before)
	if all_candidates.is_empty():
		return {"kind": "save", "score": 0.0, "reason": "Poupar: nao existe uma acao legal", "alternatives": []}
	var rollout_pool: Array[Dictionary] = all_candidates.slice(0, mini(BotRollout.MAX_CANDIDATES, all_candidates.size()))
	if not _contains_affordable(rollout_pool, simulation.credits):
		for candidate: Dictionary in all_candidates:
			if int(candidate.get("cost", 0)) <= simulation.credits:
				if rollout_pool.size() >= BotRollout.MAX_CANDIDATES:
					rollout_pool[rollout_pool.size() - 1] = candidate
				else:
					rollout_pool.append(candidate)
				break
	all_candidates = BotRollout.rank(simulation, rollout_pool, threats)
	_apply_memory_bias(all_candidates, memory)
	all_candidates.sort_custom(_decision_before)
	var affordable: Array[Dictionary] = []
	for candidate: Dictionary in all_candidates:
		if int(candidate.get("cost", 0)) <= simulation.credits:
			affordable.append(candidate)
	var best_overall: Dictionary = all_candidates[0]
	if affordable.is_empty():
		return {
			"kind": "save",
			"score": best_overall.get("score", 0.0),
			"target_cost": best_overall.get("cost", 0),
			"target_key": _decision_key(best_overall),
			"label": "Poupar",
			"reason": "Aguardar rendimento para " + str(best_overall.get("label", "a proxima defesa")) + " (" + str(best_overall.get("cost", 0)) + " creditos)",
			"breakdown": best_overall.get("breakdown", {}),
			"alternatives": []
		}
	var target_gap := maxi(0, int(best_overall.get("cost", 0)) - simulation.credits)
	var saving_is_reachable := _projected_income(simulation) >= target_gap
	var defense_is_ready := _minimum_defense_ready(simulation, threats)
	if (
		not simulation.towers.is_empty()
		and
		int(best_overall.get("cost", 0)) > simulation.credits
		and simulation.credits >= floori(float(best_overall.get("cost", 0)) * 0.65)
		and (affordable.is_empty() or float(best_overall.get("score", 0.0)) > float(affordable[0].get("score", 0.0)) * 1.15)
		and saving_is_reachable
		and defense_is_ready
	):
		return {
			"kind": "save",
			"score": best_overall.get("score", 0.0),
			"target_cost": best_overall.get("cost", 0),
			"target_key": _decision_key(best_overall),
			"label": "Poupar",
			"reason": "Poupar para " + str(best_overall.get("label", "uma resposta melhor")) + " (" + str(best_overall.get("cost", 0)) + " creditos)",
			"breakdown": best_overall.get("breakdown", {}),
			"alternatives": affordable.slice(0, mini(2, affordable.size()))
		}
	var decision: Dictionary = affordable[0].duplicate(true)
	decision["alternatives"] = affordable.slice(1, mini(3, affordable.size()))
	return decision

static func _projected_income(simulation: Variant) -> int:
	var income := 0
	for enemy: Variant in simulation.enemies:
		income += maxi(0, int(enemy.loot))
	for instance: Dictionary in simulation.pending_enemies:
		var class_id := str(instance.get("class_id", "basico"))
		income += maxi(0, int(instance.get("loot", simulation.catalog.enemy(class_id).get("loot", 0))))
	if simulation.enemies.is_empty() and simulation.pending_enemies.is_empty() and simulation.wave_index + 1 < simulation.wave_definitions.size():
		for instance: Dictionary in simulation.wave_definitions[simulation.wave_index + 1].get("enemies", []):
			var class_id := str(instance.get("class_id", "basico"))
			income += maxi(0, int(instance.get("loot", simulation.catalog.enemy(class_id).get("loot", 0))))
	return income

static func _minimum_defense_ready(simulation: Variant, threats: Dictionary[String, int]) -> bool:
	if simulation.towers.size() < 3:
		return false
	var available_dps := 0.0
	for tower: Variant in simulation.towers:
		available_dps += tower.damage * float(tower.burst) / maxf(0.1, tower.cycle)
	var threat_health := 0.0
	for class_id: String in threats:
		threat_health += float(simulation.catalog.enemy(class_id).get("health", 1.0)) * float(threats[class_id])
	var required_dps := threat_health / maxf(35.0, float(simulation.path.size()) * 2.2)
	return available_dps >= required_dps * 0.55

static func apply(simulation: Variant, decision: Dictionary) -> String:
	match decision.get("kind", ""):
		"build":
			return simulation.build_tower(str(decision.get("tower_id", "")), decision.get("cell", Vector2i(-1, -1)))
		"upgrade":
			return simulation.upgrade_tower(int(decision.get("tower_index", -1)), str(decision.get("specialization", "")))
		"save":
			return ""
		_:
			return "Sem acao legal"

static func _decision_before(left: Dictionary, right: Dictionary) -> bool:
	var left_score := float(left.get("score", 0.0))
	var right_score := float(right.get("score", 0.0))
	if not is_equal_approx(left_score, right_score):
		return left_score > right_score
	return _decision_key(left) < _decision_key(right)

static func _decision_key(decision: Dictionary) -> String:
	return str(decision.get("kind", "")) + ":" + str(decision.get("tower_id", "")) + ":" + str(decision.get("cell", "")) + ":" + str(decision.get("tower_index", -1)) + ":" + str(decision.get("specialization", ""))

static func decision_key(decision: Dictionary) -> String:
	return _decision_key(decision)

static func _contains_affordable(candidates: Array[Dictionary], credits: int) -> bool:
	for candidate: Dictionary in candidates:
		if int(candidate.get("cost", 0)) <= credits:
			return true
	return false

static func _apply_memory_bias(candidates: Array[Dictionary], memory: Dictionary) -> void:
	var target_key := str(memory.get("target_key", ""))
	var last_action_key := str(memory.get("last_action_key", ""))
	var recent := PackedStringArray(memory.get("recent_tower_ids", PackedStringArray()))
	for candidate: Dictionary in candidates:
		var key := _decision_key(candidate)
		var score := float(candidate.get("score", 0.0))
		if not target_key.is_empty() and key == target_key:
			score *= 1.12
		if not last_action_key.is_empty() and key == last_action_key:
			score *= 0.96
		if candidate.get("kind") == "build":
			var tower_id := str(candidate.get("tower_id", ""))
			var recent_count := recent.count(tower_id)
			if recent_count >= 2:
				score *= maxf(0.88, 1.0 - float(recent_count - 1) * 0.04)
				var breakdown: Dictionary = candidate.get("breakdown", {}).duplicate(true)
				breakdown["diversidade"] = -float(recent_count) * 0.04
				candidate["breakdown"] = breakdown
		candidate["score"] = score

static func _control_tower_count(simulation: Variant) -> int:
	var count := 0
	for tower: Variant in simulation.towers:
		var spec: Dictionary = simulation.catalog.tower(tower.tower_id)
		if _is_control_first_tower(spec):
			count += 1
	return count

static func _is_control_first_tower(spec: Dictionary) -> bool:
	var projectile_type := str(spec.get("base", {}).get("projectile", {}).get("type_id", ""))
	var raw_dps := float(spec.get("base", {}).get("damage", 0.0)) * float(spec.get("base", {}).get("burst", 1)) / maxf(0.1, float(spec.get("base", {}).get("cycle", 1.0)))
	return projectile_type in ["ice", "fear"] or (projectile_type == "electric" and raw_dps < 18.0)

static func _terminal_defender_count(simulation: Variant) -> int:
	if simulation.path.is_empty():
		return 0
	var terminal: Vector2 = simulation.path[simulation.path.size() - 1]
	var count := 0
	for tower: Variant in simulation.towers:
		if tower.position.distance_to(terminal) <= simulation.world_distance(tower.range_cells) * 1.5:
			count += 1
	return count

static func _visible_threats(simulation: Variant) -> Dictionary[String, int]:
	var counts: Dictionary[String, int] = {}
	for enemy: Variant in simulation.enemies:
		counts[enemy.class_id] = counts.get(enemy.class_id, 0) + 1
	for instance: Dictionary in simulation.pending_enemies:
		var class_id := str(instance.get("class_id", "basico"))
		counts[class_id] = counts.get(class_id, 0) + 1
	if simulation.pending_enemies.is_empty() and simulation.wave_index + 1 < simulation.wave_definitions.size():
		var next_wave: Dictionary = simulation.wave_definitions[simulation.wave_index + 1]
		for instance: Dictionary in next_wave.get("enemies", []):
			var class_id := str(instance.get("class_id", "basico"))
			counts[class_id] = counts.get(class_id, 0) + 1
	return counts

static func _counter_value(simulation: Variant, tower_spec: Dictionary, threats: Dictionary[String, int]) -> float:
	var swarm := 0
	var armor := 0
	for class_id: String in threats:
		var enemy_tags: Array = simulation.catalog.enemy(class_id).get("tags", [])
		if "Enxame" in enemy_tags:
			swarm += threats[class_id]
		if "Armadura" in enemy_tags:
			armor += threats[class_id]
	var tower_tags: Array = tower_spec.get("tags", [])
	var value := 0.2
	if "Area" in tower_tags or "Chain" in tower_tags:
		value += float(swarm) / 10.0
	if "Burst" in tower_tags:
		value += float(armor) / 12.0
	if "Slow" in tower_tags or "Fear" in tower_tags:
		value += float(swarm) / 14.0
	return minf(1.0, value)

static func _best_positions(simulation: Variant, spec: Dictionary, limit: int) -> Array[Vector2i]:
	var scored: Array[Dictionary] = []
	for y in simulation.map_grid.size():
		var row: Variant = simulation.map_grid[y]
		if not row is Array:
			continue
		for x in row.size():
			var cell := Vector2i(x, y)
			if not simulation.is_buildable(cell):
				continue
			var coverage := _coverage(simulation, spec, cell)
			if coverage > 0.0:
				scored.append({"cell": cell, "coverage": coverage})
	scored.sort_custom(func(left: Dictionary, right: Dictionary) -> bool:
		var left_coverage := float(left.get("coverage", 0.0))
		var right_coverage := float(right.get("coverage", 0.0))
		return left_coverage > right_coverage if not is_equal_approx(left_coverage, right_coverage) else str(left.get("cell")) < str(right.get("cell"))
	)
	var result: Array[Vector2i] = []
	for index in mini(limit, scored.size()):
		result.append(scored[index].get("cell"))
	return result

static func _coverage(simulation: Variant, spec: Dictionary, cell: Vector2i) -> float:
	var attack_range: float = simulation.world_distance(float(spec.get("base", {}).get("range", 1.0)))
	var center := Vector2(cell) + Vector2(0.5, 0.5)
	var covered_count := 0
	for point: Vector2 in simulation.path:
		if center.distance_to(point) <= attack_range:
			covered_count += 1
	return minf(1.0, float(covered_count) / float(maxi(1, simulation.path.size())))

static func _emergency_value(simulation: Variant, spec: Dictionary, cell: Vector2i, coverage: float) -> float:
	var attack_range: float = simulation.world_distance(float(spec.get("base", {}).get("range", 1.0)))
	var position := Vector2(cell) + Vector2(0.5, 0.5)
	return 1.0 if position.distance_to(simulation.path[simulation.path.size() - 1]) <= attack_range * 1.5 and coverage > 0.05 else 0.0

static func _range_waste(simulation: Variant, spec: Dictionary, cell: Vector2i) -> float:
	var attack_range: float = simulation.world_distance(float(spec.get("base", {}).get("range", 1.0)))
	var position := Vector2(cell) + Vector2(0.5, 0.5)
	var minimum_distance := INF
	for point: Vector2 in simulation.path:
		minimum_distance = minf(minimum_distance, position.distance_to(point))
	return minf(1.0, maxf(0.0, attack_range - minimum_distance) / maxf(1.0, attack_range))

static func _best_positions_strategic(simulation: Variant, spec: Dictionary, limit: int) -> Array[Vector2i]:
	var scored: Array[Dictionary] = []
	for y in simulation.map_grid.size():
		var row: Variant = simulation.map_grid[y]
		if not row is Array:
			continue
		for x in row.size():
			var cell := Vector2i(x, y)
			if not simulation.is_buildable(cell):
				continue
			var metrics := _position_metrics(simulation, spec, cell)
			if float(metrics.get("coverage", 0.0)) <= 0.0:
				continue
			var position_score := float(metrics.get("marginal", 0.0)) * 0.7 + float(metrics.get("coverage", 0.0)) * 0.3 - float(metrics.get("redundancy", 0.0)) * 0.25
			scored.append({"cell": cell, "score": position_score})
	scored.sort_custom(func(left: Dictionary, right: Dictionary) -> bool:
		var left_score := float(left.get("score", 0.0))
		var right_score := float(right.get("score", 0.0))
		return left_score > right_score if not is_equal_approx(left_score, right_score) else str(left.get("cell")) < str(right.get("cell"))
	)
	var result: Array[Vector2i] = []
	for index in mini(limit, scored.size()):
		result.append(scored[index].get("cell"))
	return result

static func _position_metrics(simulation: Variant, spec: Dictionary, cell: Vector2i) -> Dictionary:
	var attack_range: float = simulation.world_distance(float(spec.get("base", {}).get("range", 1.0)))
	var center := Vector2(cell) + Vector2(0.5, 0.5)
	var covered := 0
	var marginal := 0
	var overlap := 0
	for point: Vector2 in simulation.path:
		if center.distance_to(point) > attack_range:
			continue
		covered += 1
		var covered_before := false
		for tower: Variant in simulation.towers:
			if tower.position.distance_to(point) <= simulation.world_distance(tower.range_cells):
				covered_before = true
				break
		if covered_before:
			overlap += 1
		else:
			marginal += 1
	var path_size := float(maxi(1, simulation.path.size()))
	return {
		"coverage": float(covered) / path_size,
		"marginal": float(marginal) / path_size,
		"redundancy": float(overlap) / float(maxi(1, covered))
	}

static func _combat_utility(simulation: Variant, tower_spec: Dictionary, threats: Dictionary[String, int]) -> float:
	var base: Dictionary = tower_spec.get("base", {})
	var damage := float(base.get("damage", 1.0))
	var burst := float(base.get("burst", 1))
	var cycle := maxf(0.1, float(base.get("cycle", 1.0)))
	var area_attack: bool = float(tower_spec.get("area", 0.0)) > 0.0 or "Area" in tower_spec.get("tags", []) or "Chain" in tower_spec.get("tags", [])
	var projectile_type := str(base.get("projectile", {}).get("type_id", ""))
	var weighted_utility := 0.0
	var total_weight := 0.0
	var total_count := 0
	for class_id: String in threats:
		var enemy: Dictionary = simulation.catalog.enemy(class_id)
		var count := int(threats[class_id])
		total_count += count
		var threat_weight := float(maxi(1, int(enemy.get("threat", 1)))) * float(count)
		var resistance := float(enemy.get("area_resistance", 0.0) if area_attack else enemy.get("direct_resistance", 0.0))
		var hit_damage := maxf(1.0, damage * (1.0 - resistance) - float(enemy.get("armor", 0.0)))
		var shield_multiplier := 1.0 - float(enemy.get("shield", 0.0)) * 0.55
		var dps := hit_damage * burst / cycle * shield_multiplier
		var control := _control_value(projectile_type, enemy)
		var sustain_bonus := minf(0.65, (float(enemy.get("regeneration", 0.0)) + float(enemy.get("health", 1.0)) / 500.0) * dps / maxf(1.0, float(enemy.get("health", 1.0))) * 2.0)
		weighted_utility += (dps / 28.0 + control + sustain_bonus) * threat_weight
		total_weight += threat_weight
	if total_weight <= 0.0:
		return damage * burst / cycle / 28.0
	var crowd_bonus := 1.0
	if area_attack:
		crowd_bonus += minf(0.8, float(maxi(0, total_count - 1)) * 0.08)
	return weighted_utility / total_weight * crowd_bonus

static func _control_value(projectile_type: String, enemy: Dictionary) -> float:
	var speed := float(enemy.get("speed", 1.0))
	match projectile_type:
		"ice":
			return 0.55 + minf(0.55, speed * 0.18)
		"resin":
			return 0.28 + minf(0.35, speed * 0.1)
		"fear":
			return 0.48 + minf(0.3, speed * 0.08)
		"electric":
			return 0.24 + minf(0.32, speed * 0.1)
		"poison":
			return 0.35 if float(enemy.get("regeneration", 0.0)) > 0.0 else 0.16
		_:
			return 0.0

static func _primary_threat_label(simulation: Variant, threats: Dictionary[String, int]) -> String:
	var best_id := "basico"
	var best_weight := -1
	for class_id: String in threats:
		var weight := int(threats[class_id]) * maxi(1, int(simulation.catalog.enemy(class_id).get("threat", 1)))
		if weight > best_weight:
			best_weight = weight
			best_id = class_id
	return str(simulation.catalog.enemy(best_id).get("name", best_id)).to_upper()
