class_name EnemyScaling
extends RefCounted

static func scaled_instance(catalog: Variant, class_id: String, level: int) -> Dictionary:
	var spec: Dictionary = catalog.enemy(class_id)
	var scale := float32(float(maxi(1, level) - 1))
	var health_scale := float32(1.0 + float32(scale * float32(0.22)))
	var speed_gain := minf(float32(0.05), float32(scale * float32(0.003)))
	var attack_scale := float32(1.0 + float32(scale * float32(0.12)))
	var loot_scale := float32(1.0 + float32(scale * float32(0.08)))
	return {
		"class_id": class_id,
		"health": float32(float32(float(spec.get("health", 1.0))) * health_scale),
		"speed": float32(float32(float(spec.get("speed", 1.0))) + speed_gain),
		"attack": float32(float32(float(spec.get("attack", 1.0))) * attack_scale),
		"loot": floori(float32(float32(float(spec.get("loot", 1))) * loot_scale))
	}

static func float32(value: float) -> float:
	var bytes := PackedByteArray()
	bytes.resize(4)
	bytes.encode_float(0, value)
	return bytes.decode_float(0)
