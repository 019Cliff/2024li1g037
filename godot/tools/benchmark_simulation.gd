extends SceneTree

const Simulation = preload("res://src/domain/vertical_slice_simulation.gd")

func _init() -> void:
	var simulation := Simulation.new(null, "planicie_serena", "infinite")
	simulation.base_health = 1000000.0
	simulation.wave_index = 0
	simulation.wave_definitions = [simulation.wave_definitions[0]]
	for index in 600:
		var class_id: String = simulation.catalog.enemies.keys()[index % simulation.catalog.enemies.size()]
		var enemy := simulation._create_enemy(class_id, 12 + index % 8)
		enemy.segment = index % maxi(1, simulation.path.size() - 1)
		enemy.position = simulation.path[enemy.segment]
		simulation.enemies.append(enemy)
	simulation.credits = 1000000
	var stress_positions: Array[Vector2] = []
	for y in simulation.map_grid.size():
		for x in simulation.map_grid[y].size():
			if simulation.map_grid[y][x] == "grass":
				stress_positions.append(Vector2(x, y) + Vector2(0.5, 0.5))
	for index in 120:
		simulation.towers.append(Simulation.TowerState.new("tempestade", stress_positions[index % stress_positions.size()], simulation.catalog.tower("tempestade")))
	var samples := PackedFloat64Array()
	for _frame in 600:
		var started := Time.get_ticks_usec()
		simulation.update(1.0 / 60.0)
		samples.append(float(Time.get_ticks_usec() - started) / 1000.0)
	samples.sort()
	var total := 0.0
	for sample in samples:
		total += sample
	print(JSON.stringify({
		"enemies_initial": 600,
		"towers": simulation.towers.size(),
		"frames": samples.size(),
		"average_update_ms": total / samples.size(),
		"p95_update_ms": samples[floori(samples.size() * 0.95)],
		"max_update_ms": samples[samples.size() - 1],
		"visual_event_cap": 64
	}))
	quit(0)
