class_name BotRuntime
extends RefCounted

const VerticalSliceBot = preload("res://src/bot/vertical_slice_bot.gd")

static func execute(simulation: Variant, decision: Dictionary, checkpoint: Callable) -> Dictionary:
	if decision.get("kind") == "save":
		return {"status": "planned", "error": "", "mutated": false}
	var action_error := VerticalSliceBot.apply(simulation, decision)
	if not action_error.is_empty():
		return {"status": "error", "error": action_error, "mutated": false}
	if decision.get("kind") not in ["build", "upgrade"]:
		return {"status": "idle", "error": "", "mutated": false}
	var save_error := str(checkpoint.call())
	return {
		"status": "saved" if save_error.is_empty() else "error",
		"error": save_error,
		"mutated": true
	}
