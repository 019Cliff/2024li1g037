class_name RunPersistence
extends RefCounted

const AtomicJsonStore = preload("res://src/persistence/atomic_json_store.gd")
const DEFAULT_PATH := "user://vertical-slice-run-v1.json"

static func save_snapshot(snapshot: Dictionary, path: String = DEFAULT_PATH) -> String:
	return AtomicJsonStore.save(snapshot, path)

static func load_snapshot(path: String = DEFAULT_PATH) -> Variant:
	return AtomicJsonStore.load_with_backup(path)
