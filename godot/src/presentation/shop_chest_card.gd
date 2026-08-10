class_name ShopChestCard
extends Button

@export_enum("wood", "crystal", "imperial") var chest_id := "wood"
@export var display_name := "BAU DE MADEIRA"
@export var cost := 35
@export var pool_text := "COMUNS"
@export var accent_color := Color("d6a65e")

var elapsed := 0.0
var hover_amount := 0.0
var open_amount := 0.0
var can_afford := true
var reward_pending := false
var opening := false

func _ready() -> void:
	flat = true
	text = ""
	mouse_default_cursor_shape = Control.CURSOR_POINTING_HAND
	tooltip_text = display_name + "\n" + pool_text + "\n" + str(cost) + " gemas"
	mouse_entered.connect(func() -> void: queue_redraw())
	mouse_exited.connect(func() -> void: queue_redraw())
	focus_entered.connect(func() -> void: queue_redraw())
	focus_exited.connect(func() -> void: queue_redraw())
	queue_redraw()

func _process(delta: float) -> void:
	elapsed += delta
	var target := 1.0 if is_hovered() or has_focus() else 0.0
	hover_amount = move_toward(hover_amount, target, delta * 5.5)
	queue_redraw()

func set_shop_state(affordable: bool, has_pending_reward: bool) -> void:
	can_afford = affordable
	reward_pending = has_pending_reward
	disabled = not affordable or has_pending_reward or opening
	queue_redraw()

func play_opening(reduced_effects: bool) -> void:
	opening = true
	disabled = true
	open_amount = 0.0
	if reduced_effects:
		open_amount = 1.0
		queue_redraw()
		await get_tree().create_timer(0.12).timeout
	else:
		var tween := create_tween()
		tween.set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_OUT)
		tween.tween_property(self, "open_amount", 0.18, 0.1)
		tween.tween_property(self, "open_amount", 1.0, 0.42)
		await tween.finished
	opening = false
	queue_redraw()

func reset_opening() -> void:
	open_amount = 0.0
	opening = false
	queue_redraw()

func _draw() -> void:
	var card_rect := Rect2(Vector2(3, 3), size - Vector2(6, 6))
	var enabled_alpha := 1.0 if not disabled or opening else 0.78
	var background := Color("111d18").lerp(Color("1b2b21"), hover_amount * 0.72)
	background.a = enabled_alpha
	_draw_rounded(card_rect, background, 12, 1.0 + hover_amount, Color(accent_color, (0.38 + hover_amount * 0.55) * enabled_alpha))
	draw_rect(Rect2(card_rect.position + Vector2(14, 13), Vector2(card_rect.size.x - 28, 3)), Color(accent_color, (0.48 + hover_amount * 0.45) * enabled_alpha))

	var idle_offset := sin(elapsed * 1.45 + float(["wood", "crystal", "imperial"].find(chest_id))) * (2.2 + hover_amount * 1.6)
	var center := Vector2(size.x * 0.5, size.y * 0.53 + idle_offset)
	var glow := (0.08 + hover_amount * 0.13 + open_amount * 0.42) * enabled_alpha
	draw_circle(center + Vector2(0, 4), 68.0 + open_amount * 18.0, Color(accent_color, glow))
	for index in 6:
		var angle := elapsed * 0.28 + float(index) * TAU / 6.0
		var radius := 62.0 + float(index % 2) * 11.0
		var sparkle := center + Vector2.from_angle(angle) * radius
		var sparkle_alpha := (0.2 + hover_amount * 0.32 + open_amount * 0.45) * enabled_alpha
		draw_circle(sparkle, 1.8 + float(index % 2), Color(accent_color, sparkle_alpha))

	_draw_chest(center, enabled_alpha)

	var title_color := Color("f4e8c7") if not disabled or opening else Color("b8b9ac")
	draw_string(ThemeDB.fallback_font, Vector2(14, 39), display_name, HORIZONTAL_ALIGNMENT_CENTER, size.x - 28, 21, title_color)
	draw_string(ThemeDB.fallback_font, Vector2(14, size.y - 48), pool_text, HORIZONTAL_ALIGNMENT_CENTER, size.x - 28, 15, Color(0.69, 0.75, 0.68, enabled_alpha))
	var cost_color := Color("f3d26f") if can_afford else Color("dd776b")
	draw_colored_polygon(PackedVector2Array([
		Vector2(size.x * 0.5 - 40, size.y - 25),
		Vector2(size.x * 0.5 - 34, size.y - 31),
		Vector2(size.x * 0.5 - 28, size.y - 25),
		Vector2(size.x * 0.5 - 34, size.y - 19)
	]), Color(cost_color, enabled_alpha))
	draw_string(ThemeDB.fallback_font, Vector2(size.x * 0.5 - 22, size.y - 19), str(cost) + " GEMAS", HORIZONTAL_ALIGNMENT_LEFT, 104, 17, Color(cost_color, enabled_alpha))

	if reward_pending:
		_draw_rounded(Rect2(18, size.y * 0.47, size.x - 36, 38), Color(0.04, 0.06, 0.05, 0.9), 8, 1.0, Color("d9bd63"))
		draw_string(ThemeDB.fallback_font, Vector2(25, size.y * 0.47 + 26), "RECOLHE A RECOMPENSA", HORIZONTAL_ALIGNMENT_CENTER, size.x - 50, 14, Color("f1d978"))
	elif not can_afford:
		_draw_rounded(Rect2(30, size.y * 0.47, size.x - 60, 38), Color(0.08, 0.05, 0.05, 0.88), 8, 1.0, Color("9a4f49"))
		draw_string(ThemeDB.fallback_font, Vector2(35, size.y * 0.47 + 26), "GEMAS INSUFICIENTES", HORIZONTAL_ALIGNMENT_CENTER, size.x - 70, 14, Color("e58a80"))

func _draw_chest(center: Vector2, alpha: float) -> void:
	var body_rect := Rect2(center + Vector2(-61, -7), Vector2(122, 76))
	var lid_offset := Vector2(0, -open_amount * 31.0)
	var lid_rect := Rect2(center + Vector2(-64, -42) + lid_offset, Vector2(128, 42))
	if open_amount > 0.18:
		var beam_alpha := sin(open_amount * PI) * 0.36 * alpha
		draw_colored_polygon(PackedVector2Array([
			center + Vector2(-44, -14),
			center + Vector2(-76, -150),
			center + Vector2(76, -150),
			center + Vector2(44, -14)
		]), Color(accent_color, beam_alpha))
	draw_rect(Rect2(body_rect.position + Vector2(4, 8), body_rect.size), Color(0.01, 0.015, 0.012, 0.48 * alpha))
	match chest_id:
		"crystal":
			_draw_crystal_chest(body_rect, lid_rect, alpha)
		"imperial":
			_draw_imperial_chest(body_rect, lid_rect, alpha)
		_:
			_draw_wood_chest(body_rect, lid_rect, alpha)
	var lock_center := center + Vector2(0, 15 - open_amount * 8.0)
	_draw_rounded(Rect2(lock_center - Vector2(13, 13), Vector2(26, 28)), Color("d9b85b", alpha), 5, 2.0, Color("fff0a0", 0.7 * alpha))
	draw_circle(lock_center + Vector2(0, 1), 4.0, Color("40331e", alpha))
	draw_rect(Rect2(lock_center + Vector2(-2, 3), Vector2(4, 8)), Color("40331e", alpha))

func _draw_wood_chest(body: Rect2, lid: Rect2, alpha: float) -> void:
	_draw_rounded(body, Color("85572f", alpha), 9, 3.0, Color("c58a48", alpha))
	_draw_rounded(lid, Color("9a6635", alpha), 12, 3.0, Color("d29a55", alpha))
	for x_offset in [-36.0, 0.0, 36.0]:
		draw_line(body.position + Vector2(body.size.x * 0.5 + x_offset, 2), body.position + Vector2(body.size.x * 0.5 + x_offset, body.size.y - 2), Color("5c3a22", 0.62 * alpha), 2.0)
	for y_offset in [20.0, 47.0]:
		draw_line(body.position + Vector2(4, y_offset), body.position + Vector2(body.size.x - 4, y_offset), Color("d19a59", 0.35 * alpha), 2.0)
	draw_rect(Rect2(body.position + Vector2(10, 0), Vector2(10, body.size.y)), Color("c59a5b", 0.72 * alpha))
	draw_rect(Rect2(body.end - Vector2(20, body.size.y), Vector2(10, body.size.y)), Color("c59a5b", 0.72 * alpha))

func _draw_crystal_chest(body: Rect2, lid: Rect2, alpha: float) -> void:
	_draw_rounded(body, Color("304d66", alpha), 9, 3.0, Color("78c5e5", alpha))
	_draw_rounded(lid, Color("3d6480", alpha), 12, 3.0, Color("a2e4f4", alpha))
	for offset in [-38.0, -12.0, 15.0, 41.0]:
		draw_colored_polygon(PackedVector2Array([
			body.position + Vector2(body.size.x * 0.5 + offset, 8),
			body.position + Vector2(body.size.x * 0.5 + offset + 14, 31),
			body.position + Vector2(body.size.x * 0.5 + offset + 2, 63),
			body.position + Vector2(body.size.x * 0.5 + offset - 12, 31)
		]), Color("82cde5", 0.24 * alpha))
	draw_line(lid.position + Vector2(15, lid.size.y - 7), lid.position + Vector2(lid.size.x - 15, 7), Color("d6f6ff", 0.42 * alpha), 3.0)

func _draw_imperial_chest(body: Rect2, lid: Rect2, alpha: float) -> void:
	_draw_rounded(body, Color("513057", alpha), 9, 3.0, Color("e1bd55", alpha))
	_draw_rounded(lid, Color("673a68", alpha), 12, 3.0, Color("f4d876", alpha))
	draw_rect(Rect2(body.position + Vector2(12, 0), Vector2(9, body.size.y)), Color("d8b84f", 0.82 * alpha))
	draw_rect(Rect2(body.end - Vector2(21, body.size.y), Vector2(9, body.size.y)), Color("d8b84f", 0.82 * alpha))
	var crest := body.position + Vector2(body.size.x * 0.5, 48)
	draw_colored_polygon(PackedVector2Array([
		crest + Vector2(0, -15),
		crest + Vector2(14, -5),
		crest + Vector2(9, 13),
		crest + Vector2(-9, 13),
		crest + Vector2(-14, -5)
	]), Color("e1bd55", 0.72 * alpha))
	draw_circle(crest, 5.0, Color("fff0a1", 0.9 * alpha))

func _draw_rounded(rect: Rect2, color: Color, radius: int, border_width: float = 0.0, border_color: Color = Color.TRANSPARENT) -> void:
	var style := StyleBoxFlat.new()
	style.bg_color = color
	style.corner_radius_top_left = radius
	style.corner_radius_top_right = radius
	style.corner_radius_bottom_left = radius
	style.corner_radius_bottom_right = radius
	if border_width > 0.0:
		var width := maxi(1, roundi(border_width))
		style.border_width_left = width
		style.border_width_top = width
		style.border_width_right = width
		style.border_width_bottom = width
		style.border_color = border_color
	draw_style_box(style, rect)
