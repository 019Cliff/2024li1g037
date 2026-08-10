extends SceneTree

const AccountRepository = preload("res://src/persistence/account_repository.gd")
const SOURCE_ROOT := "user://native-ui-source"
const TARGET_ROOT := "user://native-ui-target"

func _init() -> void:
	call_deferred("_run")

func _run() -> void:
	_remove_tree(ProjectSettings.globalize_path(SOURCE_ROOT))
	_remove_tree(ProjectSettings.globalize_path(TARGET_ROOT))
	var source := AccountRepository.new(SOURCE_ROOT)
	var target := AccountRepository.new(TARGET_ROOT)
	var create_error := source.create_account("portable-profile", "Portatil")
	if not create_error.is_empty():
		push_error(create_error)
		quit(2)
		return
	var source_account: Dictionary = source.load_account("portable-profile")
	source_account["meta_progress"]["gems"] = 456
	source_account["meta_progress"]["unlocked_tower_ids"] = ["sentinela", "glaciar", "tesla"]
	source_account["shop_state"] = source_account["meta_progress"].duplicate(true)
	source_account["pending_reward"] = {"tower_id": "tesla", "gems": 0}
	source_account["settings"]["reduced_effects"] = true
	source.save_account(source_account)
	var export_path := SOURCE_ROOT + "/portable-profile.json"
	var export_error := source.export_account("portable-profile", export_path)
	if not export_error.is_empty():
		push_error(export_error)
		quit(3)
		return
	var app_state: Node = root.get_node("AppState")
	app_state.repository = target
	app_state.active_account = {}
	var scene := load("res://scenes/boot/boot.tscn") as PackedScene
	var screen := scene.instantiate()
	root.add_child(screen)
	await process_frame
	screen._import_transfer_file(export_path)
	await process_frame
	var restored: Variant = target.load_account("portable-profile")
	var clean_round_trip := (
		restored is Dictionary
		and int(restored.get("meta_progress", {}).get("gems", 0)) == 456
		and restored.get("pending_reward") is Dictionary
		and bool(restored.get("settings", {}).get("reduced_effects", false))
	)
	screen._import_transfer_file(export_path)
	await process_frame
	var collision_visible: bool = screen.get_node("ConfirmImport").visible
	screen._on_import_custom_action(&"import_as_new")
	await process_frame
	var accounts: Array = target.load_index().get("accounts", [])
	var succeeded: bool = clean_round_trip and collision_visible and accounts.size() == 2
	print(JSON.stringify({
		"status": "pass" if succeeded else "fail",
		"clean_round_trip": clean_round_trip,
		"collision_prompt": collision_visible,
		"account_count_after_copy": accounts.size()
	}))
	screen.queue_free()
	await process_frame
	_remove_tree(ProjectSettings.globalize_path(SOURCE_ROOT))
	_remove_tree(ProjectSettings.globalize_path(TARGET_ROOT))
	quit(0 if succeeded else 1)

func _remove_tree(path: String) -> void:
	if not DirAccess.dir_exists_absolute(path):
		return
	var directory := DirAccess.open(path)
	if directory == null:
		return
	directory.list_dir_begin()
	var entry := directory.get_next()
	while not entry.is_empty():
		if entry != "." and entry != "..":
			var child := path.path_join(entry)
			if directory.current_is_dir():
				_remove_tree(child)
			else:
				DirAccess.remove_absolute(child)
		entry = directory.get_next()
	directory.list_dir_end()
	DirAccess.remove_absolute(path)
