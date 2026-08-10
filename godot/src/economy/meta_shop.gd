class_name MetaShop
extends RefCounted

const COSTS := {"wood": 35, "crystal": 90, "imperial": 170}
const POOLS := {
	"wood": ["sentinela", "glaciar", "braseiro"],
	"crystal": ["glaciar", "braseiro", "panico", "venenoide", "tesla", "impacto"],
	"imperial": ["panico", "venenoide", "tesla", "impacto", "solar", "tempestade"]
}
const FUSION_COST := 180

static func buy_chest(chest_id: String, progress: Dictionary) -> Dictionary:
	if not COSTS.has(chest_id):
		return {"error": "Bau desconhecido"}
	var cost: int = COSTS[chest_id]
	if int(progress.get("gems", 0)) < cost:
		return {"error": "Gemas insuficientes"}
	var pool: Array = POOLS[chest_id]
	var seed := _next_seed(progress, chest_id)
	var tower_id: String = pool[seed % pool.size()]
	var unlocked: Array = progress.get("unlocked_tower_ids", []).duplicate()
	var compensation := maxi(5, cost / 4) if tower_id in unlocked else 0
	if tower_id not in unlocked:
		unlocked.append(tower_id)
	var updated := progress.duplicate(true)
	updated["gems"] = int(progress.get("gems", 0)) - cost + compensation
	updated["unlocked_tower_ids"] = unlocked
	return {
		"error": "",
		"progress": updated,
		"reward": {
			"kind": "duplicate_compensation" if compensation > 0 else "new_tower",
			"tower_id": tower_id,
			"gems": compensation
		},
		"next_seed": seed + 1
	}

static func fuse_tempestade(progress: Dictionary) -> Dictionary:
	var unlocked: Array = progress.get("unlocked_tower_ids", [])
	if "tempestade" in unlocked:
		return {"error": "Tempestade ja desbloqueada"}
	if "tesla" not in unlocked or "solar" not in unlocked:
		return {"error": "Falta Tesla e Solar"}
	if int(progress.get("gems", 0)) < FUSION_COST:
		return {"error": "Faltam 180 gemas"}
	var updated := progress.duplicate(true)
	var updated_unlocked := unlocked.duplicate()
	updated_unlocked.append("tempestade")
	var fused: Array = progress.get("fused_tower_ids", []).duplicate()
	fused.push_front("tempestade")
	updated["gems"] = int(progress.get("gems", 0)) - FUSION_COST
	updated["unlocked_tower_ids"] = updated_unlocked
	updated["fused_tower_ids"] = fused
	updated["level"] = maxi(int(progress.get("level", 1)), 6)
	return {"error": "", "progress": updated}

static func _next_seed(progress: Dictionary, chest_id: String) -> int:
	var chest_index := ["wood", "crystal", "imperial"].find(chest_id)
	return (
		int(progress.get("gems", 0))
		+ int(progress.get("level", 1)) * 13
		+ int(progress.get("completed_stages", 0)) * 17
		+ chest_index * 29
		+ int(progress.get("map_rotation", 0)) * 7
	)
