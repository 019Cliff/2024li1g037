class_name MapPathfinder
extends RefCounted

const NEIGHBORS: Array[Vector2i] = [Vector2i.UP, Vector2i.DOWN, Vector2i.RIGHT, Vector2i.LEFT]

static func find_path(grid: Array, start: Vector2i, goal: Vector2i) -> Array[Vector2]:
	var result: Array[Vector2] = []
	if not _is_route_cell(grid, start) or not _is_route_cell(grid, goal):
		return result
	var queue: Array[Vector2i] = [start]
	var parents: Dictionary[Vector2i, Vector2i] = {}
	var visited: Dictionary[Vector2i, bool] = {start: true}
	var cursor := 0
	while cursor < queue.size():
		var current := queue[cursor]
		cursor += 1
		if current == goal:
			break
		for offset: Vector2i in NEIGHBORS:
			var neighbor := current + offset
			if not visited.has(neighbor) and _is_route_cell(grid, neighbor):
				visited[neighbor] = true
				parents[neighbor] = current
				queue.append(neighbor)
	if not visited.has(goal):
		return result
	var reversed: Array[Vector2i] = [goal]
	var cell := goal
	while cell != start:
		cell = parents[cell]
		reversed.append(cell)
	reversed.reverse()
	for route_cell: Vector2i in reversed:
		result.append(Vector2(route_cell) + Vector2(0.5, 0.5))
	return result

static func _is_route_cell(grid: Array, cell: Vector2i) -> bool:
	if cell.y < 0 or cell.y >= grid.size() or not grid[cell.y] is Array:
		return false
	var row: Array = grid[cell.y]
	return cell.x >= 0 and cell.x < row.size() and row[cell.x] in ["path", "asphalt"]
