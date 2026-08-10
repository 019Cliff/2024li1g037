class_name WorldGrid
extends RefCounted

const SOURCE_CELLS_PER_CELL := 3
const WIDTH := 12
const HEIGHT := 11
const DISTANCE_SCALE := 1.0 / float(SOURCE_CELLS_PER_CELL)

static func official_map(source: Dictionary) -> Dictionary:
	var result := source.duplicate(true)
	result["grid"] = compress_grid(source.get("grid", []))
	result["portal"] = _scaled_endpoint(source.get("portal", {}))
	result["base"] = _scaled_endpoint(source.get("base", {}))
	result["world_grid_scale"] = SOURCE_CELLS_PER_CELL
	_force_endpoint_route(result)
	return result

static func normalize_custom_document(source: Dictionary) -> Dictionary:
	if source.is_empty():
		return {}
	var result := source.duplicate(true)
	var source_grid: Array = source.get("grid", [])
	if _needs_compression(source_grid):
		result["grid"] = compress_grid(source_grid)
		result["portal"] = _scaled_cell_dictionary(source.get("portal", {}))
		result["base"] = _scaled_cell_dictionary(source.get("base", {}))
	else:
		result["grid"] = source_grid.duplicate(true)
	result["schema"] = "immutable-towers-custom-map"
	result["version"] = 2
	result["world_grid_scale"] = SOURCE_CELLS_PER_CELL
	_force_endpoint_route(result)
	return result

static func compress_grid(source_grid: Array) -> Array:
	var result: Array = []
	for target_y in HEIGHT:
		var row: Array = []
		for target_x in WIDTH:
			row.append(_terrain_for_block(source_grid, target_x, target_y))
		result.append(row)
	return result

static func source_position_to_cell_center(position: Vector2) -> Vector2:
	return Vector2(
		floori(position.x / float(SOURCE_CELLS_PER_CELL)) + 0.5,
		floori(position.y / float(SOURCE_CELLS_PER_CELL)) + 0.5
	)

static func source_cell_to_world(cell: Vector2i) -> Vector2i:
	return Vector2i(
		floori(float(cell.x) / float(SOURCE_CELLS_PER_CELL)),
		floori(float(cell.y) / float(SOURCE_CELLS_PER_CELL))
	)

static func _terrain_for_block(source_grid: Array, target_x: int, target_y: int) -> String:
	var path_count := 0
	var asphalt_count := 0
	var water_count := 0
	for offset_y in SOURCE_CELLS_PER_CELL:
		var source_y := target_y * SOURCE_CELLS_PER_CELL + offset_y
		if source_y < 0 or source_y >= source_grid.size() or not source_grid[source_y] is Array:
			continue
		var source_row: Array = source_grid[source_y]
		for offset_x in SOURCE_CELLS_PER_CELL:
			var source_x := target_x * SOURCE_CELLS_PER_CELL + offset_x
			if source_x < 0 or source_x >= source_row.size():
				continue
			match str(source_row[source_x]):
				"path":
					path_count += 1
				"asphalt":
					asphalt_count += 1
				"water":
					water_count += 1
	if path_count + asphalt_count > 0:
		return "asphalt" if asphalt_count > path_count else "path"
	if water_count >= SOURCE_CELLS_PER_CELL:
		return "water"
	return "grass"

static func _scaled_endpoint(source: Variant) -> Dictionary:
	if not source is Dictionary:
		return {}
	var result: Dictionary = source.duplicate(true)
	var position: Variant = source.get("position")
	if position is Dictionary:
		var cell := source_cell_to_world(Vector2i(
			floori(float(position.get("x", 0.0))),
			floori(float(position.get("y", 0.0)))
		))
		result["position"] = {"x": cell.x + 0.5, "y": cell.y + 0.5}
	return result

static func _scaled_cell_dictionary(source: Variant) -> Dictionary:
	if not source is Dictionary:
		return {}
	var cell := source_cell_to_world(Vector2i(
		int(source.get("x", -1)),
		int(source.get("y", -1))
	))
	return {"x": cell.x, "y": cell.y}

static func _needs_compression(grid: Array) -> bool:
	if grid.size() > HEIGHT:
		return true
	for row: Variant in grid:
		if row is Array and row.size() > WIDTH:
			return true
	return false

static func _force_endpoint_route(document: Dictionary) -> void:
	var grid: Variant = document.get("grid")
	if not grid is Array:
		return
	for key in ["portal", "base"]:
		var endpoint: Variant = document.get(key)
		if not endpoint is Dictionary:
			continue
		var position: Variant = endpoint.get("position", endpoint)
		if not position is Dictionary:
			continue
		var cell := Vector2i(
			floori(float(position.get("x", -1.0))),
			floori(float(position.get("y", -1.0)))
		)
		if cell.y >= 0 and cell.y < grid.size() and grid[cell.y] is Array and cell.x >= 0 and cell.x < grid[cell.y].size():
			if grid[cell.y][cell.x] not in ["path", "asphalt"]:
				grid[cell.y][cell.x] = "path"
