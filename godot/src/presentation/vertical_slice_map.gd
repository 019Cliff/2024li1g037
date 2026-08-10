class_name VerticalSliceMap
extends Control

const DomainCatalog = preload("res://src/domain/domain_catalog.gd")
const GameLayout = preload("res://src/presentation/game_layout.gd")
const WorldGrid = preload("res://src/domain/world_grid.gd")
var grid: Array = []

func _ready() -> void:
	var catalog: Variant = DomainCatalog.new()
	catalog.load_default()
	grid = WorldGrid.official_map(catalog.map_data("planicie_serena")).get("grid", [])
	custom_minimum_size = GameLayout.MAP_PIXEL_SIZE
	mouse_filter = Control.MOUSE_FILTER_IGNORE

func set_grid(value: Array) -> void:
	grid = value
	queue_redraw()

func _draw() -> void:
	for y in GameLayout.MAP_HEIGHT:
		for x in GameLayout.MAP_WIDTH:
			var terrain_id := "grass"
			if y < grid.size() and grid[y] is Array and x < grid[y].size():
				terrain_id = str(grid[y][x])
			var color := _terrain_color(terrain_id, x, y)
			var rect := Rect2(Vector2(x, y) * GameLayout.TILE_SIZE, Vector2.ONE * GameLayout.TILE_SIZE)
			draw_rect(rect, color)
			_draw_terrain_detail(rect, terrain_id, x, y)
			draw_rect(rect, Color(0.055, 0.075, 0.06, 0.52), false, 1.5)

func _terrain_color(terrain_id: String, x: int, y: int) -> Color:
	match terrain_id:
		"water":
			return Color("456f86")
		"path":
			return Color("695139")
		"asphalt":
			return Color("414846")
		_:
			return Color("3f5e36") if (x + y) % 2 == 0 else Color("45663b")

func _draw_terrain_detail(rect: Rect2, terrain_id: String, x: int, y: int) -> void:
	var seed := x * 31 + y * 47
	match terrain_id:
		"grass":
			for index in 4:
				var local_x := 10.0 + float((seed + index * 17) % 45)
				var local_y := 14.0 + float((seed * 3 + index * 19) % 39)
				var base := rect.position + Vector2(local_x, local_y)
				draw_line(base, base + Vector2(-2.5, -5.5), Color(0.58, 0.73, 0.42, 0.28), 1.5)
				draw_line(base, base + Vector2(3.0, -4.0), Color(0.33, 0.48, 0.28, 0.42), 1.5)
		"water":
			for index in 3:
				var y_offset := 16.0 + float(index) * 17.0 + float(seed % 5)
				draw_line(rect.position + Vector2(9, y_offset), rect.position + Vector2(29, y_offset - 2), Color(0.57, 0.78, 0.86, 0.24), 2.0)
				draw_line(rect.position + Vector2(38, y_offset + 5), rect.position + Vector2(57, y_offset + 3), Color(0.2, 0.38, 0.49, 0.34), 2.0)
		"path":
			for index in 3:
				var stone_center := rect.position + Vector2(14.0 + float((seed + index * 21) % 40), 15.0 + float((seed * 2 + index * 13) % 36))
				draw_circle(stone_center, 4.0 + float(index % 2), Color(0.33, 0.24, 0.17, 0.2))
				draw_arc(stone_center, 4.0 + float(index % 2), 0.0, TAU, 10, Color(0.74, 0.59, 0.39, 0.13), 1.0)
		"asphalt":
			draw_line(rect.position + Vector2(8, 33), rect.position + Vector2(24, 33), Color(0.78, 0.72, 0.5, 0.34), 2.0)
			draw_line(rect.position + Vector2(42, 33), rect.position + Vector2(58, 33), Color(0.78, 0.72, 0.5, 0.34), 2.0)
