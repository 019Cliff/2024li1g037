class_name RunScore
extends RefCounted

const MODE_MULTIPLIERS := {
	"history": 1.0,
	"infinite": 1.15,
	"challenge": 1.35,
	"boss": 1.5,
	"sandbox": 0.25
}

const MODE_PAR_SECONDS := {
	"history": 720.0,
	"infinite": 900.0,
	"challenge": 600.0,
	"boss": 540.0,
	"sandbox": 720.0
}

static func calculate(simulation: Variant, survived_waves: int) -> int:
	var mode_id := str(simulation.mode_id)
	var base_points := maxi(0, survived_waves) * 750 + maxi(0, int(simulation.enemies_defeated)) * 50
	var health_bonus := maxi(0, floori(float(simulation.base_health) * 20.0))
	var par_seconds := float(simulation.difficulty_profile.get("par_seconds", MODE_PAR_SECONDS.get(mode_id, 720.0)))
	var time_bonus := maxi(0, floori((par_seconds - float(simulation.elapsed)) * 2.0))
	var efficiency_bonus := mini(500, maxi(0, int(simulation.credits)) * 2)
	var subtotal := base_points + health_bonus + time_bonus + efficiency_bonus
	return maxi(0, floori(float(subtotal) * float(MODE_MULTIPLIERS.get(mode_id, 1.0))))
