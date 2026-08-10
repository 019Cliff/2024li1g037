class_name VerticalSliceView
extends Control

const GameLayout = preload("res://src/presentation/game_layout.gd")
const TILE_SIZE := GameLayout.TILE_SIZE
const MODEL_SCALE := 2.15
const ENEMY_SCALE := 2.15
var simulation: VerticalSliceSimulation
var selected_tower_index: int = -1
var build_tower_id: String = ""
var obstacle_mode := false
var hovered_cell := Vector2i(-1, -1)
var reduced_effects := false
var show_damage_numbers := true

func _ready() -> void:
	mouse_filter = Control.MOUSE_FILTER_IGNORE

func _draw() -> void:
	if simulation == null:
		return
	if selected_tower_index >= 0 and selected_tower_index < simulation.towers.size():
		var selected := simulation.towers[selected_tower_index]
		var selected_cell := Vector2i(floori(selected.position.x), floori(selected.position.y))
		_draw_cell_preview(selected_cell, Color(0.92, 0.75, 0.28, 0.18), Color(0.95, 0.8, 0.32, 0.72))
		var range_pixels := simulation.world_distance(selected.range_cells) * TILE_SIZE
		draw_circle(selected.position * TILE_SIZE, range_pixels, Color(0.85, 0.75, 0.3, 0.08))
		draw_arc(selected.position * TILE_SIZE, range_pixels, 0.0, TAU, 64, Color(0.85, 0.75, 0.3, 0.55), 2.5)
	if not build_tower_id.is_empty() and hovered_cell.x >= 0:
		var preview_color := Color(0.4, 0.8, 0.5, 0.35) if simulation.is_buildable(hovered_cell) else Color(0.9, 0.25, 0.2, 0.35)
		var preview_border := Color(0.48, 0.95, 0.58, 0.9) if simulation.is_buildable(hovered_cell) else Color(1.0, 0.34, 0.27, 0.9)
		_draw_cell_preview(hovered_cell, preview_color, preview_border)
	elif obstacle_mode and hovered_cell.x >= 0:
		var terrain := simulation._terrain_at(Vector2(hovered_cell) + Vector2(0.5, 0.5))
		var preview_color := Color(0.75, 0.62, 0.36, 0.42) if terrain in ["path", "asphalt"] else Color(0.9, 0.25, 0.2, 0.35)
		draw_rect(Rect2(Vector2(hovered_cell) * TILE_SIZE, Vector2.ONE * TILE_SIZE), preview_color)
	if not simulation.path.is_empty():
		_draw_portal(simulation.path[0] * TILE_SIZE)
		_draw_base(simulation.path[simulation.path.size() - 1] * TILE_SIZE)
	for cell: Vector2i in simulation.obstacles:
		_draw_obstacle((Vector2(cell) + Vector2(0.5, 0.5)) * TILE_SIZE)
	for tower in simulation.towers:
		_draw_tower(tower)
	for enemy in simulation.enemies:
		_draw_enemy(enemy)
	for shot: Dictionary in simulation.shots:
		var shot_color := _tower_color(str(shot.get("tower_id")))
		draw_line(shot.get("from") * TILE_SIZE, shot.get("to") * TILE_SIZE, shot_color, 3.0)
	for event: Dictionary in simulation.visual_events:
		_draw_visual_event(event)

func _draw_tower(tower: VerticalSliceSimulation.TowerState) -> void:
	var center := tower.position * TILE_SIZE
	var color := _tower_color(tower.tower_id)
	_draw_tower_foundation(center, color)
	var tier_scale := MODEL_SCALE + float(tower.level - 1) * 0.13
	draw_circle(center, 12.0 * tier_scale, Color("202a25"))
	match tower.tower_id:
		"sentinela":
			draw_rect(Rect2(center - Vector2(7, 8) * tier_scale, Vector2(14, 16) * tier_scale), color)
			draw_line(center, center + Vector2(0, -19) * tier_scale, color, 5.0)
		"glaciar":
			draw_colored_polygon(PackedVector2Array([center + Vector2(0, -16) * tier_scale, center + Vector2(10, 7) * tier_scale, center, center + Vector2(-10, 7) * tier_scale]), color)
		"braseiro":
			draw_circle(center, 9.0 * tier_scale, color)
			draw_colored_polygon(PackedVector2Array([center + Vector2(-7, 1) * tier_scale, center + Vector2(0, -18) * tier_scale, center + Vector2(7, 1) * tier_scale]), Color("f29a51"))
		"panico":
			draw_arc(center, 10.0 * tier_scale, 0.0, TAU, 24, color, 5.0)
			draw_circle(center, 3.0 * tier_scale, color)
		"venenoide":
			draw_rect(Rect2(center + Vector2(-7, -5) * tier_scale, Vector2(14, 14) * tier_scale), color)
			draw_line(center + Vector2(-4, -7) * tier_scale, center + Vector2(4, -7) * tier_scale, Color("dce8d5"), 5.0)
		"tesla":
			for radius in [5.0, 9.0, 13.0]: draw_arc(center, radius * tier_scale, -1.1, 1.1, 12, color, 3.0)
		"impacto":
			draw_rect(Rect2(center - Vector2(10, 7) * tier_scale, Vector2(20, 14) * tier_scale), color)
			draw_line(center, center + Vector2(17, -10) * tier_scale, color, 9.0)
		"solar":
			draw_circle(center, 7.0 * tier_scale, color)
			for angle in range(0, 360, 45): draw_line(center + Vector2.from_angle(deg_to_rad(angle)) * 10.0 * tier_scale, center + Vector2.from_angle(deg_to_rad(angle)) * 16.0 * tier_scale, color, 4.0)
		"tempestade":
			draw_colored_polygon(PackedVector2Array([center + Vector2(0, -17) * tier_scale, center + Vector2(13, -5) * tier_scale, center + Vector2(8, 13) * tier_scale, center + Vector2(-8, 13) * tier_scale, center + Vector2(-13, -5) * tier_scale]), color)
			draw_arc(center, 17.0 * tier_scale, elapsed_rotation(), elapsed_rotation() + 4.7, 24, Color("d8f0ff"), 2.0)
	if tower.level >= 4:
		draw_arc(center, 15.0 * tier_scale, 0.0, TAU, 24, Color(color, 0.7), 2.0)

func _draw_cell_preview(center_cell: Vector2i, fill_color: Color, border_color: Color) -> void:
	var cell_rect := Rect2(Vector2(center_cell) * TILE_SIZE, Vector2.ONE * TILE_SIZE)
	draw_rect(cell_rect, fill_color)
	draw_rect(cell_rect, border_color, false, 3.0)

func _draw_tower_foundation(center: Vector2, color: Color) -> void:
	var radius := TILE_SIZE * 0.44
	var cut := 9.0
	var points := PackedVector2Array([
		center + Vector2(-radius + cut, -radius),
		center + Vector2(radius - cut, -radius),
		center + Vector2(radius, -radius + cut),
		center + Vector2(radius, radius - cut),
		center + Vector2(radius - cut, radius),
		center + Vector2(-radius + cut, radius),
		center + Vector2(-radius, radius - cut),
		center + Vector2(-radius, -radius + cut)
	])
	draw_colored_polygon(points, Color(0.08, 0.12, 0.1, 0.86))
	var outline := points.duplicate()
	outline.append(points[0])
	draw_polyline(outline, Color(color, 0.55), 2.0)
	draw_line(center + Vector2(-radius + 5, 0), center + Vector2(radius - 5, 0), Color(color, 0.14), 1.0)
	draw_line(center + Vector2(0, -radius + 5), center + Vector2(0, radius - 5), Color(color, 0.14), 1.0)
	var bolt_offset := radius * 0.68
	for offset in [Vector2(-bolt_offset, -bolt_offset), Vector2(bolt_offset, -bolt_offset), Vector2(bolt_offset, bolt_offset), Vector2(-bolt_offset, bolt_offset)]:
		draw_circle(center + offset, 2.5, Color(color, 0.72))

func _draw_enemy(enemy: VerticalSliceSimulation.EnemyState) -> void:
	var center := enemy.position * TILE_SIZE
	var spec: Dictionary = simulation.catalog.enemy(enemy.class_id)
	var rgb: Dictionary = spec.get("color_rgb", {})
	var enemy_color := Color(float(rgb.get("r", 141)) / 255.0, float(rgb.get("g", 101)) / 255.0, float(rgb.get("b", 80)) / 255.0)
	var radius := 8.0 * ENEMY_SCALE
	match enemy.class_id:
		"rapido":
			enemy_color = Color("daa64a")
			radius = 6.0 * ENEMY_SCALE
		"tanque":
			enemy_color = Color("74574a")
			radius = 11.0 * ENEMY_SCALE
		"dispersor":
			radius = 9.0 * ENEMY_SCALE
		"blindado": radius = 10.0 * ENEMY_SCALE
		"regenerador": radius = 9.0 * ENEMY_SCALE
		"protegido": radius = 9.0 * ENEMY_SCALE
		"elite": radius = 11.0 * ENEMY_SCALE
		"boss_acelerador", "boss_guardiao", "boss_ruptura": radius = 15.0 * ENEMY_SCALE
	if enemy.class_id in ["rapido", "dispersor"]:
		draw_colored_polygon(PackedVector2Array([center + Vector2(radius, 0), center + Vector2(-radius, -radius * 0.7), center + Vector2(-radius, radius * 0.7)]), enemy_color)
	elif enemy.class_id in ["blindado", "boss_guardiao"]:
		draw_rect(Rect2(center - Vector2.ONE * radius, Vector2.ONE * radius * 2.0), enemy_color)
	else:
		draw_circle(center, radius, enemy_color)
	if enemy.hit_flash > 0.0:
		draw_circle(center, radius + 2.0, Color(1, 1, 1, enemy.hit_flash * 0.65))
	if enemy.class_id == "boss_guardiao":
		draw_arc(center, simulation.world_distance(3.2) * TILE_SIZE, 0.0, TAU, 40, Color(0.4, 0.7, 0.9, 0.18), 2.0)
	elif enemy.class_id == "boss_ruptura":
		draw_arc(center, simulation.world_distance(4.5) * TILE_SIZE, 0.0, TAU, 40, Color(0.8, 0.35, 0.7, 0.18), 2.0)
	if not enemy.effects.is_empty():
		var effect_color := _effect_color(str(enemy.effects.keys()[0]))
		draw_arc(center, radius + 3.0, 0.0, TAU, 20, effect_color, 2.0)
	var health_ratio := clampf(enemy.health / enemy.max_health, 0.0, 1.0)
	var health_width := 28.0 * ENEMY_SCALE
	draw_rect(Rect2(center + Vector2(-health_width * 0.5, -radius - 11.0), Vector2(health_width, 5)), Color("351f1f"))
	draw_rect(Rect2(center + Vector2(-health_width * 0.5, -radius - 11.0), Vector2(health_width * health_ratio, 5)), Color("80b56a"))

func _draw_visual_event(event: Dictionary) -> void:
	if reduced_effects and str(event.get("kind", "")) in ["build", "sell", "death", "obstacle"]:
		return
	var age := float(event.get("age", 0.0))
	var duration := maxf(0.01, float(event.get("duration", 1.0)))
	var progress := clampf(age / duration, 0.0, 1.0)
	var center: Vector2 = event.get("position", Vector2.ZERO) * TILE_SIZE
	match str(event.get("kind", "")):
		"damage":
			if not show_damage_numbers:
				return
			var text := str(maxi(1, roundi(float(event.get("value", 0.0)))))
			draw_string(ThemeDB.fallback_font, center + Vector2(-8, -22 - progress * 18), text, HORIZONTAL_ALIGNMENT_LEFT, -1, 14, Color(1.0, 0.9, 0.72, 1.0 - progress))
		"upgrade":
			draw_arc(center, 8.0 + progress * 22.0, 0.0, TAU, 28, Color(0.95, 0.78, 0.3, 1.0 - progress), 3.0)
		"build":
			draw_arc(center, 5.0 + progress * 16.0, 0.0, TAU, 24, Color(0.45, 0.85, 0.55, 1.0 - progress), 3.0)
		"sell":
			draw_string(ThemeDB.fallback_font, center + Vector2(-12, -progress * 24), "+" + str(int(event.get("value", 0))), HORIZONTAL_ALIGNMENT_LEFT, -1, 14, Color(0.5, 0.9, 0.65, 1.0 - progress))
		"death":
			draw_circle(center, 6.0 + progress * 18.0, Color(0.9, 0.55, 0.28, 0.32 * (1.0 - progress)))
			for angle in range(0, 360, 60):
				var direction := Vector2.from_angle(deg_to_rad(angle))
				draw_line(center + direction * (4.0 + progress * 8.0), center + direction * (10.0 + progress * 18.0), Color(1.0, 0.75, 0.35, 1.0 - progress), 2.0)
		"obstacle":
			draw_arc(center, 6.0 + progress * 14.0, 0.0, TAU, 20, Color(0.82, 0.68, 0.4, 1.0 - progress), 2.0)

func _tower_color(tower_id: String) -> Color:
	var spec: Dictionary = simulation.catalog.tower(tower_id)
	var rgb: Dictionary = spec.get("color_rgb", {})
	return Color(float(rgb.get("r", 214)) / 255.0, float(rgb.get("g", 173)) / 255.0, float(rgb.get("b", 84)) / 255.0)

func elapsed_rotation() -> float:
	return fmod(simulation.elapsed * 1.8, TAU)

func _effect_color(effect_id: String) -> Color:
	match effect_id:
		"fire": return Color("ef8054")
		"ice": return Color("79c7e8")
		"resin": return Color("c4a26a")
		"fear": return Color("b68ad7")
		"poison": return Color("78b66b")
		_: return Color("e7d36c")

func _draw_portal(center: Vector2) -> void:
	draw_circle(center, 27.0, Color("292036"))
	draw_arc(center, 21.0, 0.0, TAU, 28, Color("8f68bf"), 7.0)
	draw_arc(center, 12.0, elapsed_rotation(), elapsed_rotation() + 4.8, 20, Color("c49de8"), 3.0)

func _draw_base(center: Vector2) -> void:
	draw_rect(Rect2(center - Vector2(28, 28), Vector2(56, 56)), Color("9da39a"))
	draw_rect(Rect2(center - Vector2(17, 17), Vector2(34, 34)), Color("3c443f"))
	draw_rect(Rect2(center - Vector2(7, 27), Vector2(14, 15)), Color("d7c986"))

func _draw_obstacle(center: Vector2) -> void:
	draw_circle(center + Vector2(-12, 4), 14.0, Color("77776f"))
	draw_circle(center + Vector2(10, 6), 16.0, Color("96948a"))
	draw_line(center + Vector2(-22, 19), center + Vector2(23, 19), Color("3d403b"), 6.0)
