class_name SaveCoordinator
extends RefCounted

var interval_seconds: float
var dirty := false
var elapsed_since_save := 0.0

func _init(save_interval_seconds: float = 12.0) -> void:
	interval_seconds = maxf(1.0, save_interval_seconds)

func mark_dirty() -> void:
	dirty = true

func tick(delta: float, saver: Callable) -> String:
	if not dirty:
		return ""
	elapsed_since_save += maxf(0.0, delta)
	if elapsed_since_save < interval_seconds:
		return ""
	return flush(saver)

func flush(saver: Callable) -> String:
	if not dirty:
		return ""
	var error := str(saver.call())
	if error.is_empty():
		dirty = false
		elapsed_since_save = 0.0
	return error

func clear() -> void:
	dirty = false
	elapsed_since_save = 0.0
