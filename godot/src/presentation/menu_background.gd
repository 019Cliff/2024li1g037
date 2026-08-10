class_name MenuBackground
extends Control

var elapsed := 0.0
var reduced_effects := false

func _ready() -> void:
	var app_state := get_node_or_null("/root/AppState")
	if app_state != null:
		reduced_effects = bool(app_state.settings().get("reduced_effects", false))
	queue_redraw()

func _process(delta: float) -> void:
	elapsed += delta * (0.22 if reduced_effects else 1.0)
	queue_redraw()

func _draw() -> void:
	var viewport_size := get_viewport_rect().size
	_draw_sky(viewport_size)
	_draw_distant_landscape(viewport_size)
	_draw_tactical_grid(viewport_size)
	var road_points := _draw_winding_road(viewport_size)
	_draw_portal(road_points[0])
	_draw_battlefield(road_points, viewport_size)
	_draw_atmosphere(viewport_size)
	_draw_vignette(viewport_size)

func _draw_sky(viewport_size: Vector2) -> void:
	var top := Color("081310")
	var bottom := Color("193126")
	var bands := 18
	for index in bands:
		var ratio := float(index) / float(bands - 1)
		var band_color := top.lerp(bottom, ratio)
		draw_rect(Rect2(0, ratio * viewport_size.y, viewport_size.x, viewport_size.y / float(bands) + 2.0), band_color)
	var moon_center := Vector2(viewport_size.x * 0.82, viewport_size.y * 0.2)
	draw_circle(moon_center, 92.0, Color(0.42, 0.58, 0.42, 0.035))
	draw_circle(moon_center, 54.0, Color(0.73, 0.78, 0.56, 0.055))
	draw_arc(moon_center, 55.0, -0.4, 3.4, 48, Color(0.82, 0.78, 0.48, 0.16), 2.0)

func _draw_distant_landscape(viewport_size: Vector2) -> void:
	var horizon := viewport_size.y * 0.48
	var far_points := PackedVector2Array([Vector2(0, horizon + 48)])
	for index in 13:
		var x := float(index) * viewport_size.x / 12.0
		var y := horizon - 44.0 - sin(float(index) * 1.7) * 54.0 - float(index % 3) * 18.0
		far_points.append(Vector2(x, y))
	far_points.append(Vector2(viewport_size.x, viewport_size.y))
	far_points.append(Vector2(0, viewport_size.y))
	draw_colored_polygon(far_points, Color("13271f"))

	var near_points := PackedVector2Array([Vector2(0, horizon + 88)])
	for index in 17:
		var x := float(index) * viewport_size.x / 16.0
		var y := horizon + 30.0 - sin(float(index) * 2.15 + 0.8) * 35.0 - float(index % 4) * 12.0
		near_points.append(Vector2(x, y))
	near_points.append(Vector2(viewport_size.x, viewport_size.y))
	near_points.append(Vector2(0, viewport_size.y))
	draw_colored_polygon(near_points, Color("102019"))

	for index in 34:
		var tree_x := fmod(float(index) * 137.0 + 41.0, viewport_size.x)
		var tree_y := horizon + 18.0 + sin(float(index) * 1.31) * 26.0
		var height := 22.0 + float(index % 5) * 8.0
		draw_colored_polygon(PackedVector2Array([
			Vector2(tree_x, tree_y - height),
			Vector2(tree_x - height * 0.36, tree_y),
			Vector2(tree_x + height * 0.36, tree_y)
		]), Color(0.07, 0.15, 0.115, 0.96))

func _draw_tactical_grid(viewport_size: Vector2) -> void:
	var horizon_y := viewport_size.y * 0.46
	var grid_color := Color(0.24, 0.42, 0.29, 0.095)
	var drift := fmod(elapsed * 10.0, 58.0)
	for index in range(-10, 11):
		var bottom_x := viewport_size.x * 0.5 + float(index) * 150.0 + drift
		draw_line(Vector2(viewport_size.x * 0.5, horizon_y), Vector2(bottom_x, viewport_size.y), grid_color, 1.0)
	for index in 11:
		var ratio := float(index) / 10.0
		var eased := ratio * ratio
		var y := lerpf(horizon_y, viewport_size.y, eased)
		draw_line(Vector2(0, y), Vector2(viewport_size.x, y), Color(grid_color, grid_color.a * ratio), 1.0)

func _draw_winding_road(viewport_size: Vector2) -> PackedVector2Array:
	var center_points := PackedVector2Array()
	var upper := PackedVector2Array()
	var lower := PackedVector2Array()
	var samples := 42
	for index in samples:
		var ratio := float(index) / float(samples - 1)
		var point := _road_point(ratio, viewport_size)
		var width := 31.0 + ratio * 20.0
		center_points.append(point)
		upper.append(point - Vector2(0, width))
		lower.append(point + Vector2(0, width))
	var polygon := PackedVector2Array()
	for point in upper:
		polygon.append(point)
	for index in range(lower.size() - 1, -1, -1):
		polygon.append(lower[index])
	draw_colored_polygon(polygon, Color(0.22, 0.19, 0.135, 0.88))
	draw_polyline(upper, Color(0.56, 0.43, 0.22, 0.4), 3.0)
	draw_polyline(lower, Color(0.56, 0.43, 0.22, 0.4), 3.0)
	for index in range(0, center_points.size() - 1, 3):
		var offset := fmod(elapsed * 28.0 + float(index) * 25.0, 110.0)
		if offset > 54.0:
			continue
		draw_line(center_points[index] - Vector2(8, 0), center_points[index] + Vector2(8, 0), Color(0.83, 0.7, 0.37, 0.17), 2.0)
	return center_points

func _road_point(ratio: float, viewport_size: Vector2) -> Vector2:
	var y := viewport_size.y * 0.72 + sin(ratio * TAU * 1.25 + 0.4) * 54.0 + sin(ratio * TAU * 2.7) * 13.0
	return Vector2(ratio * viewport_size.x, y)

func _draw_portal(center: Vector2) -> void:
	var pulse := 0.5 + sin(elapsed * 2.4) * 0.5
	draw_circle(center + Vector2(16, 0), 56.0 + pulse * 8.0, Color(0.38, 0.22, 0.55, 0.07))
	draw_arc(center + Vector2(16, 0), 35.0 + pulse * 4.0, 0.0, TAU, 42, Color(0.63, 0.39, 0.82, 0.42), 5.0)
	draw_arc(center + Vector2(16, 0), 24.0, -elapsed * 0.65, TAU - elapsed * 0.65, 32, Color(0.78, 0.58, 0.94, 0.56), 3.0)
	draw_circle(center + Vector2(16, 0), 15.0, Color(0.14, 0.08, 0.21, 0.8))

func _draw_battlefield(road_points: PackedVector2Array, viewport_size: Vector2) -> void:
	var enemy_count := 4 if reduced_effects else 8
	var enemy_positions := PackedVector2Array()
	for index in enemy_count:
		var progress := fmod(elapsed * (0.032 + float(index % 3) * 0.006) + float(index) * 0.137, 1.08)
		var sample := mini(road_points.size() - 1, floori(progress * float(road_points.size() - 1)))
		var enemy_position := road_points[sample]
		enemy_positions.append(enemy_position)
		_draw_enemy(enemy_position, index, progress)

	var tower_a := _road_point(0.28, viewport_size) + Vector2(0, -86)
	var tower_b := _road_point(0.66, viewport_size) + Vector2(0, 92)
	var tower_c := _road_point(0.84, viewport_size) + Vector2(0, -90)
	_draw_tower(tower_a, Color("d6ad54"), enemy_positions[mini(2, enemy_positions.size() - 1)], "sentinel")
	_draw_tower(tower_b, Color("75d8e6"), enemy_positions[mini(5, enemy_positions.size() - 1)], "tesla")
	_draw_tower(tower_c, Color("ef8054"), enemy_positions[0], "fire")
	if not reduced_effects:
		_draw_projectile(tower_a, enemy_positions[mini(2, enemy_positions.size() - 1)], 0.0, Color("e8c45e"))
		_draw_projectile(tower_b, enemy_positions[mini(5, enemy_positions.size() - 1)], 0.34, Color("81e8f2"))
		_draw_projectile(tower_c, enemy_positions[0], 0.68, Color("f28d58"))

func _draw_enemy(center: Vector2, index: int, progress: float) -> void:
	var bob := sin(elapsed * 5.0 + float(index)) * 2.0
	center.y += bob
	var colors := [Color("a46e4d"), Color("d39a43"), Color("6f5c50"), Color("936a91")]
	var color: Color = colors[index % colors.size()]
	var radius := 7.0 + float(index % 3) * 2.0
	if index % 4 == 1:
		draw_colored_polygon(PackedVector2Array([
			center + Vector2(radius + 3, 0),
			center + Vector2(-radius, -radius * 0.75),
			center + Vector2(-radius, radius * 0.75)
		]), color)
	elif index % 4 == 2:
		draw_rect(Rect2(center - Vector2.ONE * radius, Vector2.ONE * radius * 2.0), color)
	else:
		draw_circle(center, radius, color)
	var health := clampf(0.98 - progress * 0.48 - float(index % 3) * 0.08, 0.18, 1.0)
	draw_rect(Rect2(center + Vector2(-12, -18), Vector2(24, 3)), Color(0.17, 0.07, 0.055, 0.85))
	draw_rect(Rect2(center + Vector2(-12, -18), Vector2(24.0 * health, 3)), Color(0.43, 0.72, 0.4, 0.9))

func _draw_tower(center: Vector2, color: Color, target: Vector2, kind: String) -> void:
	draw_circle(center + Vector2(4, 7), 19.0, Color(0.015, 0.025, 0.02, 0.58))
	draw_circle(center, 16.0, Color("1d2a24"))
	draw_circle(center, 11.0, Color(color, 0.82))
	var direction := center.direction_to(target)
	match kind:
		"tesla":
			for radius in [8.0, 13.0, 18.0]:
				draw_arc(center, radius, -1.0, 1.0, 12, Color(color, 0.56), 2.0)
		"fire":
			draw_colored_polygon(PackedVector2Array([
				center + direction * 25.0,
				center + direction.rotated(2.2) * 11.0,
				center + direction.rotated(-2.2) * 11.0
			]), color)
		_:
			draw_line(center, center + direction * 29.0, color, 7.0)
	draw_arc(center, 20.0, elapsed * 0.22, elapsed * 0.22 + 4.4, 24, Color(color, 0.28), 2.0)

func _draw_projectile(origin: Vector2, target: Vector2, phase: float, color: Color) -> void:
	var progress := fmod(elapsed * 1.45 + phase, 1.0)
	var projectile := origin.lerp(target, progress)
	var previous := origin.lerp(target, maxf(0.0, progress - 0.08))
	draw_line(previous, projectile, Color(color, 0.65), 3.0)
	draw_circle(projectile, 3.0, color)
	if progress > 0.92:
		var burst := (progress - 0.92) / 0.08
		draw_arc(target, 4.0 + burst * 13.0, 0.0, TAU, 18, Color(color, 1.0 - burst), 2.0)

func _draw_atmosphere(viewport_size: Vector2) -> void:
	var mote_count := 12 if reduced_effects else 28
	for index in mote_count:
		var speed := 7.0 + float(index % 5) * 2.4
		var x := fmod(float(index) * 149.0 + elapsed * speed, viewport_size.x + 60.0) - 30.0
		var base_y := fmod(float(index * 83), viewport_size.y)
		var y := base_y + sin(elapsed * 0.7 + float(index)) * 17.0
		var alpha := 0.08 + float(index % 4) * 0.025
		draw_circle(Vector2(x, y), 1.0 + float(index % 3), Color(0.82, 0.72, 0.39, alpha))

func _draw_vignette(viewport_size: Vector2) -> void:
	draw_rect(Rect2(0, 0, viewport_size.x, 90), Color(0.01, 0.025, 0.02, 0.35))
	draw_rect(Rect2(0, viewport_size.y - 110, viewport_size.x, 110), Color(0.01, 0.025, 0.02, 0.42))
	draw_rect(Rect2(0, 0, 110, viewport_size.y), Color(0.01, 0.025, 0.02, 0.36))
	draw_rect(Rect2(viewport_size.x - 110, 0, 110, viewport_size.y), Color(0.01, 0.025, 0.02, 0.36))
