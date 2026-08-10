class_name CombatResolver
extends RefCounted

const ProjectileEffects = preload("res://src/combat/projectile_effects.gd")

static func resolve_hit(tower: Variant, enemy: Variant, context_multiplier: float) -> float:
	var health_before: float = enemy.health
	var resistance: float = enemy.area_resistance if tower.area > 0.0 else enemy.direct_resistance
	var shield_multiplier: float = 1.0 - enemy.shield if enemy.shield > 0.0 and enemy.health > enemy.max_health * 0.66 else 1.0
	var final_damage := maxf(0.5, maxf(0.0, tower.damage - enemy.armor) * (1.0 - resistance) * shield_multiplier) * context_multiplier
	ProjectileEffects.apply(enemy, tower.projectile_type, tower.projectile_duration)
	var synergy_damage := 0.0
	if enemy.effects.has("fire") and enemy.effects.has("electric"):
		synergy_damage += 8.0
	if enemy.effects.has("poison") and enemy.effects.has("resin"):
		synergy_damage += 5.0
	enemy.health = maxf(0.0, enemy.health - final_damage - synergy_damage)
	enemy.hit_flash = 1.0
	return health_before - enemy.health
