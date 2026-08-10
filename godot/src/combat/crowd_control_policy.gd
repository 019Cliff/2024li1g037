class_name CrowdControlPolicy
extends RefCounted

const EFFECT_CAPS := {
	"resin": 6.0,
	"ice": 3.0,
	"electric": 1.5,
	"fear": 1.5,
	"fire": 8.0,
	"poison": 10.0
}

static func apply(enemy: Variant, effect_id: String, duration: float) -> void:
	if duration <= 0.0:
		return
	var effective_duration := duration
	if effect_id in ["electric", "fear"]:
		effective_duration *= _hard_cc_multiplier(enemy)
		effective_duration *= 1.0 / (1.0 + float(enemy.hard_cc_exposure) * 0.75)
		enemy.hard_cc_exposure += effective_duration
		enemy.cc_window_remaining = _cc_window(enemy)
	elif effect_id == "ice":
		effective_duration *= _soft_cc_multiplier(enemy)
	var cap := _effect_cap(enemy, effect_id)
	enemy.effects[effect_id] = minf(cap, maxf(float(enemy.effects.get(effect_id, 0.0)), effective_duration))

static func movement_multiplier(enemy: Variant) -> float:
	var multiplier := 1.0
	if enemy.effects.has("ice"):
		multiplier = minf(multiplier, _minimum_slow_speed(enemy))
	if enemy.effects.has("resin"):
		multiplier = minf(multiplier, 0.8 if not _is_boss(enemy) else 0.9)
	if _is_boss(enemy) and enemy.effects.has("fear"):
		multiplier = minf(multiplier, 0.65)
	return multiplier

static func fear_causes_retreat(enemy: Variant) -> bool:
	return enemy.effects.has("fear") and not _is_boss(enemy)

static func update_recovery(enemy: Variant, delta: float) -> void:
	if enemy.cc_window_remaining > 0.0:
		enemy.cc_window_remaining = maxf(0.0, enemy.cc_window_remaining - delta)
		return
	enemy.hard_cc_exposure = maxf(0.0, enemy.hard_cc_exposure - delta * 0.5)

static func _effect_cap(enemy: Variant, effect_id: String) -> float:
	var cap := float(EFFECT_CAPS.get(effect_id, 8.0))
	if effect_id == "electric":
		return minf(cap, 0.5 if _is_boss(enemy) else (1.0 if _is_elite(enemy) else 1.5))
	if effect_id == "fear":
		return minf(cap, 0.5 if _is_boss(enemy) else (1.0 if _is_elite(enemy) else 1.5))
	if effect_id == "ice":
		return minf(cap, 1.0 if _is_boss(enemy) else (2.0 if _is_elite(enemy) else 3.0))
	return cap

static func _minimum_slow_speed(enemy: Variant) -> float:
	return 0.5 if _is_boss(enemy) else (0.35 if _is_elite(enemy) else 0.25)

static func _hard_cc_multiplier(enemy: Variant) -> float:
	return 0.25 if _is_boss(enemy) else (0.6 if _is_elite(enemy) else 1.0)

static func _soft_cc_multiplier(enemy: Variant) -> float:
	return 0.5 if _is_boss(enemy) else (0.7 if _is_elite(enemy) else 1.0)

static func _cc_window(enemy: Variant) -> float:
	return 10.0 if _is_boss(enemy) else (8.0 if _is_elite(enemy) else 6.0)

static func _is_boss(enemy: Variant) -> bool:
	return str(enemy.class_id).begins_with("boss_")

static func _is_elite(enemy: Variant) -> bool:
	return str(enemy.class_id) == "elite"
