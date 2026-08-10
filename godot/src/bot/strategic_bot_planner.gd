class_name StrategicBotPlanner
extends RefCounted

const VerticalSliceBot = preload("res://src/bot/vertical_slice_bot.gd")
const RECENT_ACTION_LIMIT := 5

var memory: Dictionary = {
	"target_key": "",
	"recent_tower_ids": PackedStringArray(),
	"last_action_key": "",
	"last_wave": -1
}

func decide(simulation: Variant) -> Dictionary:
	return VerticalSliceBot.decide(simulation, memory)

func record_outcome(simulation: Variant, decision: Dictionary, outcome: Dictionary) -> void:
	if not str(outcome.get("error", "")).is_empty():
		memory["target_key"] = ""
		return
	var kind := str(decision.get("kind", ""))
	if kind == "save":
		memory["target_key"] = str(decision.get("target_key", memory.get("target_key", "")))
		memory["last_wave"] = simulation.wave_index
		return
	if not bool(outcome.get("mutated", false)):
		return
	var action_key := VerticalSliceBot.decision_key(decision)
	memory["last_action_key"] = action_key
	memory["last_wave"] = simulation.wave_index
	if action_key == str(memory.get("target_key", "")):
		memory["target_key"] = ""
	var tower_id := str(decision.get("tower_id", ""))
	if kind == "upgrade":
		var tower_index := int(decision.get("tower_index", -1))
		if tower_index >= 0 and tower_index < simulation.towers.size():
			tower_id = simulation.towers[tower_index].tower_id
	if tower_id.is_empty():
		return
	var recent := PackedStringArray(memory.get("recent_tower_ids", PackedStringArray()))
	recent.append(tower_id)
	while recent.size() > RECENT_ACTION_LIMIT:
		recent.remove_at(0)
	memory["recent_tower_ids"] = recent

func snapshot() -> Dictionary:
	return {
		"target_key": str(memory.get("target_key", "")),
		"recent_tower_ids": Array(memory.get("recent_tower_ids", PackedStringArray())),
		"last_action_key": str(memory.get("last_action_key", "")),
		"last_wave": int(memory.get("last_wave", -1))
	}

func restore(document: Variant) -> void:
	if not document is Dictionary:
		return
	memory["target_key"] = str(document.get("target_key", ""))
	memory["recent_tower_ids"] = PackedStringArray(document.get("recent_tower_ids", []))
	memory["last_action_key"] = str(document.get("last_action_key", ""))
	memory["last_wave"] = int(document.get("last_wave", -1))
