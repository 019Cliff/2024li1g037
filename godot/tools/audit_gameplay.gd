extends SceneTree

const Simulation = preload("res://src/domain/vertical_slice_simulation.gd")
const VerticalSliceBot = preload("res://src/bot/vertical_slice_bot.gd")
const StrategicBotPlanner = preload("res://src/bot/strategic_bot_planner.gd")
const DomainIds = preload("res://src/domain/domain_ids.gd")

const STEP_SECONDS := 0.1
const BOT_INTERVAL_SECONDS := 1.0
const FINITE_RUN_LIMIT_SECONDS := 900.0
const INFINITE_RUN_LIMIT_SECONDS := 480.0

func _init() -> void:
	var arguments := OS.get_cmdline_user_args()
	if not arguments.is_empty() and arguments[0] == "gate":
		var gate_transactions := _transaction_rows()
		var gate_runs := _focus_runs()
		var campaign_gate_runs := _campaign_runs()
		gate_runs.append_array(campaign_gate_runs)
		var failures: Array[String] = []
		for row: Dictionary in gate_transactions:
			if not str(row.get("build_error", "")).is_empty() or int(row.get("profit", 0)) > 0:
				failures.append("economy:" + str(row.get("mode_id")) + ":" + str(row.get("tower_id")))
		for run: Dictionary in gate_runs:
			var progress_limit := 30.0 if run.get("mode_id") == "boss" else 20.0
			if not bool(run.get("finished", false)):
				failures.append("unfinished:" + str(run.get("arsenal")))
			if float(run.get("max_no_progress_seconds", 0.0)) > progress_limit:
				failures.append("stalled:" + str(run.get("arsenal")))
			if not run.get("action_errors", []).is_empty():
				failures.append("actions:" + str(run.get("arsenal")))
		for run: Dictionary in campaign_gate_runs:
			if not bool(run.get("victory", false)):
				failures.append("campaign:" + str(run.get("difficulty_profile")))
		print(JSON.stringify({
			"schema": "immutable-towers-gameplay-gate",
			"status": "pass" if failures.is_empty() else "fail",
			"failures": failures,
			"transactions_checked": gate_transactions.size(),
			"finite_runs_checked": gate_runs.size()
		}, "\t"))
		quit(0 if failures.is_empty() else 1)
		return
	if not arguments.is_empty() and arguments[0] == "focus":
		print(JSON.stringify({"focus_runs": _focus_runs()}, "\t"))
		quit(0)
		return
	if not arguments.is_empty() and arguments[0] == "bot-compare":
		var comparison_runs: Array[Dictionary] = []
		for map_id: String in DomainIds.MAPS:
			comparison_runs.append(_run_bot(map_id, "history", DomainIds.TOWERS, "arsenal_completo"))
			comparison_runs.append(_run_bot(map_id, "history", PackedStringArray(["sentinela"]), "apenas_sentinela"))
		print(JSON.stringify({"bot_comparison_runs": comparison_runs}, "\t"))
		quit(0)
		return
	if not arguments.is_empty() and arguments[0] == "campaign":
		print(JSON.stringify({"campaign_runs": _campaign_runs()}, "\t"))
		quit(0)
		return
	if not arguments.is_empty() and arguments[0] == "economy":
		var economy_runs: Array[Dictionary] = []
		for credits: int in [125, 175, 225, 275, 350]:
			economy_runs.append(_run_bot("planicie_serena", "challenge", DomainIds.TOWERS, "completo_" + str(credits), credits))
			economy_runs.append(_run_bot("planicie_serena", "challenge", PackedStringArray(["sentinela"]), "sentinela_" + str(credits), credits))
		for credits: int in [180, 250, 350, 500, 700]:
			economy_runs.append(_run_bot("planicie_serena", "boss", DomainIds.TOWERS, "completo_" + str(credits), credits))
			economy_runs.append(_run_bot("planicie_serena", "boss", PackedStringArray(["sentinela"]), "sentinela_" + str(credits), credits))
		print(JSON.stringify({"economy_runs": economy_runs}, "\t"))
		quit(0)
		return
	if not arguments.is_empty() and arguments[0] == "transactions":
		print(JSON.stringify({"transaction_rows": _transaction_rows()}, "\t"))
		quit(0)
		return

	var maps: Array[Dictionary] = []
	for map_id: String in DomainIds.MAPS:
		maps.append(_map_metrics(map_id))

	var bot_runs: Array[Dictionary] = []
	for map_id: String in DomainIds.MAPS:
		bot_runs.append(_run_bot(map_id, "history", DomainIds.TOWERS, "arsenal_completo"))
		bot_runs.append(_run_bot(map_id, "history", PackedStringArray(["sentinela"]), "apenas_sentinela"))
	for mode_id: String in DomainIds.MODES:
		if mode_id == "history":
			continue
		bot_runs.append(_run_bot("planicie_serena", mode_id, DomainIds.TOWERS, "arsenal_completo"))
		bot_runs.append(_run_bot("planicie_serena", mode_id, PackedStringArray(["sentinela"]), "apenas_sentinela"))

	var single_tower_runs: Array[Dictionary] = []
	for mode_id: String in ["history", "challenge", "boss"]:
		for tower_id: String in DomainIds.TOWERS:
			single_tower_runs.append(_run_bot(
				"planicie_serena",
				mode_id,
				PackedStringArray([tower_id]),
				"apenas_" + tower_id
			))

	print(JSON.stringify({
		"schema": "immutable-towers-gameplay-audit",
		"world_grid": {"width": 12, "height": 11, "source_cells_per_cell": 3},
		"maps": maps,
		"bot_runs": bot_runs,
		"single_tower_runs": single_tower_runs
	}, "\t"))
	quit(0)

func _focus_runs() -> Array[Dictionary]:
	return [
		_run_bot("cruzamento_solar", "history", DomainIds.TOWERS, "arsenal_completo"),
		_run_bot("planicie_serena", "history", PackedStringArray(["glaciar"]), "apenas_glaciar"),
		_run_bot("planicie_serena", "boss", PackedStringArray(["glaciar"]), "apenas_glaciar"),
		_run_bot("planicie_serena", "boss", PackedStringArray(["panico"]), "apenas_panico")
	]

func _campaign_runs() -> Array[Dictionary]:
	var runs: Array[Dictionary] = []
	for stage_index in DomainIds.MAPS.size():
		runs.append(_run_bot(
			DomainIds.MAPS[stage_index],
			"history",
			DomainIds.TOWERS,
			"cap1_est" + str(stage_index + 1),
			-1,
			{"campaign_profile": true, "chapter": 1, "stage": stage_index + 1}
		))
	return runs

func _transaction_rows() -> Array[Dictionary]:
	var rows: Array[Dictionary] = []
	for mode_id: String in DomainIds.MODES:
		for tower_id: String in DomainIds.TOWERS:
			rows.append(_transaction_metrics(mode_id, tower_id))
	return rows

func _map_metrics(map_id: String) -> Dictionary:
	var simulation := Simulation.new(null, map_id, "history", DomainIds.TOWERS)
	var terrain_counts := {
		"grass": 0,
		"path": 0,
		"asphalt": 0,
		"water": 0,
		"other": 0
	}
	var buildable_cells: Array[Vector2i] = []
	for y in simulation.map_grid.size():
		var row: Variant = simulation.map_grid[y]
		if not row is Array:
			continue
		for x in row.size():
			var terrain := str(row[x])
			if terrain_counts.has(terrain):
				terrain_counts[terrain] = int(terrain_counts[terrain]) + 1
			else:
				terrain_counts.other = int(terrain_counts.other) + 1
			var cell := Vector2i(x, y)
			if simulation.is_buildable(cell):
				buildable_cells.append(cell)

	var route_distance := 0.0
	for index in range(1, simulation.path.size()):
		route_distance += simulation.path[index - 1].distance_to(simulation.path[index])

	var tower_coverage: Dictionary = {}
	for tower_id: String in DomainIds.TOWERS:
		var spec: Dictionary = simulation.catalog.tower(tower_id)
		var range_world := simulation.world_distance(float(spec.get("base", {}).get("range", 0.0)))
		var best_covered := 0
		var useful_cells := 0
		for cell: Vector2i in buildable_cells:
			var center := Vector2(cell) + Vector2(0.5, 0.5)
			var covered := 0
			for point: Vector2 in simulation.path:
				if center.distance_to(point) <= range_world:
					covered += 1
			if covered > 0:
				useful_cells += 1
			best_covered = maxi(best_covered, covered)
		tower_coverage[tower_id] = {
			"range_world_cells": snappedf(range_world, 0.001),
			"best_route_coverage_ratio": snappedf(float(best_covered) / float(maxi(1, simulation.path.size())), 0.001),
			"useful_build_cells": useful_cells
		}

	return {
		"map_id": map_id,
		"width": simulation.map_grid[0].size() if not simulation.map_grid.is_empty() else 0,
		"height": simulation.map_grid.size(),
		"terrain": terrain_counts,
		"buildable_cells": buildable_cells.size(),
		"history_tower_limit": simulation.tower_limit,
		"buildable_to_limit_ratio": snappedf(float(buildable_cells.size()) / float(maxi(1, simulation.tower_limit)), 0.01),
		"route_cells": simulation.path.size(),
		"route_distance": snappedf(route_distance, 0.01),
		"tower_coverage": tower_coverage
	}

func _transaction_metrics(mode_id: String, tower_id: String) -> Dictionary:
	var simulation := Simulation.new(null, "planicie_serena", mode_id, DomainIds.TOWERS)
	simulation.credits = 100000
	var build_cell := Vector2i(-1, -1)
	for y in simulation.map_grid.size():
		for x in simulation.map_grid[y].size():
			var candidate := Vector2i(x, y)
			if simulation.is_buildable(candidate):
				build_cell = candidate
				break
		if build_cell.x >= 0:
			break
	var purchase_price := simulation.tower_price(tower_id)
	var build_error := simulation.build_tower(tower_id, build_cell)
	var sale_value := simulation.sell_tower(0) if build_error.is_empty() else 0
	return {
		"mode_id": mode_id,
		"tower_id": tower_id,
		"purchase_price": purchase_price,
		"sale_value": sale_value,
		"profit": sale_value - purchase_price,
		"build_error": build_error
	}

func _run_bot(map_id: String, mode_id: String, arsenal: PackedStringArray, arsenal_label: String, starting_credits_override: int = -1, run_context: Dictionary = {}) -> Dictionary:
	var simulation := Simulation.new(null, map_id, mode_id, arsenal, {}, run_context)
	if starting_credits_override >= 0:
		simulation.credits = starting_credits_override
	var initial_credits := simulation.credits
	var initial_health := simulation.base_health
	var build_counts: Dictionary = {}
	var upgrade_counts: Dictionary = {}
	var decision_counts := {"build": 0, "upgrade": 0, "save": 0, "other": 0}
	var decision_trace: Array[Dictionary] = []
	var action_errors: Array[String] = []
	var next_bot_time := 0.0
	var limit := INFINITE_RUN_LIMIT_SECONDS if mode_id == "infinite" else FINITE_RUN_LIMIT_SECONDS
	var no_progress_seconds := 0.0
	var max_no_progress_seconds := 0.0
	var previous_progress := _progress_sample(simulation)
	var planner := StrategicBotPlanner.new()

	while not simulation.finished and simulation.elapsed < limit:
		if simulation.elapsed + 0.0001 >= next_bot_time:
			var decision: Dictionary = planner.decide(simulation)
			var kind := str(decision.get("kind", "other"))
			if decision_counts.has(kind):
				decision_counts[kind] = int(decision_counts[kind]) + 1
			else:
				decision_counts.other = int(decision_counts.other) + 1
			if kind in ["build", "upgrade"]:
				var tower_id := str(decision.get("tower_id", ""))
				if kind == "upgrade":
					var tower_index := int(decision.get("tower_index", -1))
					if tower_index >= 0 and tower_index < simulation.towers.size():
						tower_id = simulation.towers[tower_index].tower_id
				var error := VerticalSliceBot.apply(simulation, decision)
				if decision_trace.size() < 20:
					decision_trace.append({
						"time": snappedf(simulation.elapsed, 0.1),
						"kind": kind,
						"tower_id": tower_id,
						"cell": decision.get("cell", Vector2i(-1, -1)),
						"score": snappedf(float(decision.get("score", 0.0)), 0.001)
					})
				planner.record_outcome(simulation, decision, {
					"status": "saved" if error.is_empty() else "error",
					"error": error,
					"mutated": error.is_empty()
				})
				if error.is_empty():
					var counts: Dictionary = build_counts if kind == "build" else upgrade_counts
					counts[tower_id] = int(counts.get(tower_id, 0)) + 1
				elif action_errors.size() < 8:
					action_errors.append(error)
			elif kind == "save":
				planner.record_outcome(simulation, decision, {"status": "planned", "error": "", "mutated": false})
			next_bot_time += BOT_INTERVAL_SECONDS
		simulation.update(STEP_SECONDS)
		var current_progress := _progress_sample(simulation)
		if _made_progress(previous_progress, current_progress):
			no_progress_seconds = 0.0
		else:
			no_progress_seconds += STEP_SECONDS
			max_no_progress_seconds = maxf(max_no_progress_seconds, no_progress_seconds)
		previous_progress = current_progress

	var built_types := PackedStringArray()
	for tower_id: String in build_counts:
		if int(build_counts[tower_id]) > 0:
			built_types.append(tower_id)
	built_types.sort()
	var active_enemy_counts: Dictionary = {}
	var active_effect_max_seconds: Dictionary = {}
	var lowest_health_ratio := 1.0
	var highest_health_ratio := 0.0
	for enemy: Variant in simulation.enemies:
		active_enemy_counts[enemy.class_id] = int(active_enemy_counts.get(enemy.class_id, 0)) + 1
		for effect_id: String in enemy.effects:
			active_effect_max_seconds[effect_id] = maxf(
				float(active_effect_max_seconds.get(effect_id, 0.0)),
				float(enemy.effects.get(effect_id, 0.0))
			)
		var health_ratio: float = float(enemy.health) / maxf(1.0, float(enemy.max_health))
		lowest_health_ratio = minf(lowest_health_ratio, health_ratio)
		highest_health_ratio = maxf(highest_health_ratio, health_ratio)

	return {
		"map_id": map_id,
		"mode_id": mode_id,
		"difficulty_profile": simulation.difficulty_profile.get("id", ""),
		"arsenal": arsenal_label,
		"finished": simulation.finished,
		"victory": simulation.victory,
		"elapsed_seconds": snappedf(simulation.elapsed, 0.1),
		"waves_started": maxi(0, simulation.wave_index + 1),
		"base_health": snappedf(simulation.base_health, 0.1),
		"base_health_ratio": snappedf(simulation.base_health / maxf(1.0, initial_health), 0.001),
		"initial_credits": initial_credits,
		"final_credits": simulation.credits,
		"tower_count": simulation.towers.size(),
		"tower_limit": simulation.tower_limit,
		"build_counts": build_counts,
		"upgrade_counts": upgrade_counts,
		"built_type_count": built_types.size(),
		"built_types": Array(built_types),
		"decision_counts": decision_counts,
		"decision_trace": decision_trace,
		"action_errors": action_errors,
		"active_enemy_counts": active_enemy_counts,
		"active_effect_max_seconds": active_effect_max_seconds,
		"active_enemy_health_ratio_min": snappedf(lowest_health_ratio, 0.001) if not simulation.enemies.is_empty() else 0.0,
		"active_enemy_health_ratio_max": snappedf(highest_health_ratio, 0.001) if not simulation.enemies.is_empty() else 0.0,
		"pending_enemy_count": simulation.pending_enemies.size(),
		"max_no_progress_seconds": snappedf(max_no_progress_seconds, 0.1)
	}

func _progress_sample(simulation: Variant) -> Dictionary:
	var total_health := 0.0
	var route_progress := 0.0
	for enemy: Variant in simulation.enemies:
		total_health += float(enemy.health)
		var segment := clampi(int(enemy.segment), 0, simulation.path.size() - 1)
		var fractional := 0.0
		if segment < simulation.path.size() - 1:
			var start: Vector2 = simulation.path[segment]
			var target: Vector2 = simulation.path[segment + 1]
			var length := maxf(0.0001, start.distance_to(target))
			fractional = clampf(start.distance_to(enemy.position) / length, 0.0, 1.0)
		route_progress += float(segment) + fractional
	return {
		"wave": simulation.wave_index,
		"active": simulation.enemies.size(),
		"pending": simulation.pending_enemies.size(),
		"health": total_health,
		"route": route_progress,
		"base": simulation.base_health
	}

func _made_progress(previous: Dictionary, current: Dictionary) -> bool:
	return (
		int(previous.get("wave", -1)) != int(current.get("wave", -1))
		or int(previous.get("active", 0)) != int(current.get("active", 0))
		or int(previous.get("pending", 0)) != int(current.get("pending", 0))
		or float(current.get("health", 0.0)) < float(previous.get("health", 0.0)) - 0.001
		or float(current.get("route", 0.0)) > float(previous.get("route", 0.0)) + 0.0001
		or float(current.get("base", 0.0)) < float(previous.get("base", 0.0)) - 0.001
	)
