extends SceneTree

func _init() -> void:
	call_deferred("_capture")

func _capture() -> void:
	var arguments := OS.get_cmdline_user_args()
	if arguments.size() < 3:
		push_error("Usage: capture_scene.gd -- WIDTH HEIGHT OUTPUT [SCENE] [SECTION]")
		quit(2)
		return
	var size := Vector2i(int(arguments[0]), int(arguments[1]))
	var output := arguments[2]
	root.content_scale_size = Vector2i(1920, 1080)
	root.content_scale_mode = Window.CONTENT_SCALE_MODE_CANVAS_ITEMS
	root.content_scale_aspect = Window.CONTENT_SCALE_ASPECT_EXPAND
	DisplayServer.window_set_size(size)
	for _resize_frame in 20:
		await process_frame
		if DisplayServer.window_get_size() == size:
			break
	var scene_path := arguments[3] if arguments.size() >= 4 else "res://scenes/boot/boot.tscn"
	var scene := load(scene_path) as PackedScene
	if scene == null:
		push_error("Could not load scene: " + scene_path)
		quit(3)
		return
	var instance := scene.instantiate()
	root.add_child(instance)
	if arguments.size() >= 5:
		if arguments[4] in ["global_scale_qa", "footprint_qa"]:
			await process_frame
			_prepare_global_scale_preview(instance)
		elif instance.has_method("_show_section"):
			instance.call_deferred("_show_section", arguments[4])
	for _frame in 30:
		await process_frame
	var image := root.get_texture().get_image()
	var error := image.save_png(output)
	instance.queue_free()
	await process_frame
	quit(0 if error == OK else 1)

func _prepare_global_scale_preview(instance: Node) -> void:
	var simulation: Variant = instance.get("simulation")
	if simulation == null:
		return
	simulation.credits = 100000
	var built := 0
	for y in simulation.map_grid.size():
		for x in simulation.map_grid[y].size():
			var cell := Vector2i(x, y)
			if simulation.is_buildable(cell) and simulation.build_tower("sentinela", cell).is_empty():
				built += 1
			if built >= 4:
				return
