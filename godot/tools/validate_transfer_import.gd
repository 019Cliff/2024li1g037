extends SceneTree

const AccountRepository = preload("res://src/persistence/account_repository.gd")
const TEST_ROOT := "user://transfer-ui-validation"

func _init() -> void:
	call_deferred("_run")

func _run() -> void:
	var arguments := OS.get_cmdline_user_args()
	if arguments.is_empty():
		push_error("Usage: validate_transfer_import.gd -- TRANSFER_JSON")
		quit(2)
		return
	_remove_tree(ProjectSettings.globalize_path(TEST_ROOT))
	var repository := AccountRepository.new(TEST_ROOT)
	var create_error := repository.create_account("ui-import-test", "Import Test")
	if not create_error.is_empty():
		push_error(create_error)
		quit(3)
		return
	var app_state: Node = root.get_node("AppState")
	app_state.repository = repository
	app_state.active_account = repository.load_account("ui-import-test")
	app_state.selected_map_id = "planicie_serena"
	var scene := load("res://scenes/boot/boot.tscn") as PackedScene
	var screen := scene.instantiate()
	root.add_child(screen)
	await process_frame
	screen._show_section("profile")
	screen._import_transfer_file(arguments[0])
	if screen.pending_transfer is Dictionary and screen.get_node("ConfirmImport").visible:
		screen._confirm_transfer_import()
	await process_frame
	var imported: Variant = repository.load_account("legacy-global")
	var succeeded := imported is Dictionary and str(screen.message.text).contains("importada")
	print(JSON.stringify({
		"status": "pass" if succeeded else "fail",
		"message": screen.message.text,
		"imported_account": imported.get("account_id", "") if imported is Dictionary else "",
		"source_path": arguments[0]
	}))
	screen.queue_free()
	await process_frame
	_remove_tree(ProjectSettings.globalize_path(TEST_ROOT))
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
