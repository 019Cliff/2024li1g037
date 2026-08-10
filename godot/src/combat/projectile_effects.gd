class_name ProjectileEffects
extends RefCounted

const CrowdControlPolicy = preload("res://src/combat/crowd_control_policy.gd")

static func apply(enemy: Variant, projectile_type: String, duration: float) -> void:
	if projectile_type == "fire":
		enemy.effects.erase("resin")
	elif projectile_type == "electric":
		enemy.effects.erase("ice")
	CrowdControlPolicy.apply(enemy, projectile_type, duration)
	var types_before: Array = enemy.effects.keys()
	if enemy.effects.has("fire") and enemy.effects.has("resin"):
		var fire_duration: float = enemy.effects.get("fire", 0.0) * 2.0
		enemy.effects.clear()
		CrowdControlPolicy.apply(enemy, "fire", fire_duration)
	elif enemy.effects.has("fire") and enemy.effects.has("ice"):
		enemy.effects.erase("fire")
		enemy.effects.erase("ice")
	if "ice" in types_before and "electric" in types_before:
		CrowdControlPolicy.apply(enemy, "electric", 1.2)
	if "fear" in types_before and "poison" in types_before:
		CrowdControlPolicy.apply(enemy, "fear", 2.5)
