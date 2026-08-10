class_name ShopRewardReveal
extends Control

var reward: Dictionary = {}
var reveal_amount := 1.0
var elapsed := 0.0

func _ready() -> void:
	mouse_filter = Control.MOUSE_FILTER_IGNORE
	set_process(false)

func present(value: Dictionary, animated: bool = true) -> void:
	reward = value.duplicate(true)
	visible = true
	set_process(true)
	if not animated:
		reveal_amount = 1.0
		queue_redraw()
		return
	reveal_amount = 0.0
	var tween := create_tween().set_trans(Tween.TRANS_BACK).set_ease(Tween.EASE_OUT)
	tween.tween_property(self, "reveal_amount", 1.0, 0.48)

func dismiss() -> void:
	reward = {}
	visible = false
	set_process(false)

func _process(delta: float) -> void:
	elapsed += delta
	queue_redraw()

func _draw() -> void:
	if reward.is_empty():
		return
	var accent := _tower_color(str(reward.get("tower_id", "")))
	var panel_rect := Rect2(Vector2(2, 2), size - Vector2(4, 4))
	var style := StyleBoxFlat.new()
	style.bg_color = Color("13221b")
	style.border_width_left = 1
	style.border_width_top = 1
	style.border_width_right = 1
	style.border_width_bottom = 1
	style.border_color = Color(accent, 0.72)
	style.corner_radius_top_left = 12
	style.corner_radius_top_right = 12
	style.corner_radius_bottom_left = 12
	style.corner_radius_bottom_right = 12
	draw_style_box(style, panel_rect)

	var center := Vector2(58, size.y * 0.5)
	for index in 10:
		var angle := elapsed * 0.18 + float(index) * TAU / 10.0
		var start := center + Vector2.from_angle(angle) * (27.0 * reveal_amount)
		var finish := center + Vector2.from_angle(angle) * (39.0 * reveal_amount)
		draw_line(start, finish, Color(accent, 0.28), 3.0)
	draw_circle(center, 24.0 * reveal_amount, Color(accent, 0.2))
	draw_circle(center, 15.0 * reveal_amount, Color(accent, 0.9))
	draw_circle(center, 7.0 * reveal_amount, Color("f8f0d0"))

	var tower_name := _display_id(str(reward.get("tower_id", "")))
	var compensation := int(reward.get("gems", 0))
	var heading := "NOVA TORRE DESBLOQUEADA" if str(reward.get("kind", "")) != "duplicate_compensation" else "TORRE REPETIDA"
	var detail := tower_name if compensation <= 0 else tower_name + "  +  " + str(compensation) + " GEMAS"
	draw_string(ThemeDB.fallback_font, Vector2(108, 38), heading, HORIZONTAL_ALIGNMENT_LEFT, size.x - 120, 16, Color("d6bd69"))
	draw_string(ThemeDB.fallback_font, Vector2(108, 69), detail, HORIZONTAL_ALIGNMENT_LEFT, size.x - 120, 24, Color("f5edd5"))

func _tower_color(tower_id: String) -> Color:
	var colors := {
		"sentinela": Color("d6ad54"),
		"glaciar": Color("70c7e8"),
		"braseiro": Color("ef8054"),
		"panico": Color("b68ad7"),
		"venenoide": Color("78b66b"),
		"tesla": Color("75d8e6"),
		"impacto": Color("c99b70"),
		"solar": Color("f4cf58"),
		"tempestade": Color("9bcdf2")
	}
	return colors.get(tower_id, Color("d6bd69"))

func _display_id(value: String) -> String:
	return value.to_upper().replace("_", " ")
