extends Control

const MapEditorState = preload("res://src/domain/map_editor_state.gd")
const AtomicJsonStore = preload("res://src/persistence/atomic_json_store.gd")
const GameLayout = preload("res://src/presentation/game_layout.gd")
const WorldGrid = preload("res://src/domain/world_grid.gd")
const SAVE_PATH := "user://custom-map-v1.json"

@onready var app_state: Node = get_node("/root/AppState")
@onready var map_layer: VerticalSliceMap = %MapLayer
@onready var map_frame: PanelContainer = %MapFrame
@onready var message: Label = %Message

var editor: Variant
var placement_mode := "terrain"

func _ready() -> void:
	_reset()
	map_frame.gui_input.connect(_on_map_input)
	%Terrain.pressed.connect(func() -> void: _set_mode("terrain"))
	%Portal.pressed.connect(func() -> void: _set_mode("portal"))
	%Base.pressed.connect(func() -> void: _set_mode("base"))
	%Save.pressed.connect(_save)
	%Load.pressed.connect(_load)
	%Reset.pressed.connect(_reset)
	%PlayCustom.pressed.connect(_play_custom)
	%UseOfficial.pressed.connect(_use_official)
	%Back.pressed.connect(func() -> void: get_tree().change_scene_to_file("res://scenes/boot/boot.tscn"))

func _reset() -> void:
	var map_data: Dictionary = WorldGrid.official_map(app_state.catalog.map_data(app_state.selected_map_id))
	var portal_data: Dictionary = map_data.get("portal", {}).get("position", {})
	var base_data: Dictionary = map_data.get("base", {}).get("position", {})
	editor = MapEditorState.new(
		map_data.get("grid", []),
		Vector2i(floori(float(portal_data.get("x", 0))), floori(float(portal_data.get("y", 0)))),
		Vector2i(floori(float(base_data.get("x", 0))), floori(float(base_data.get("y", 0))))
	)
	_refresh("Mapa oficial carregado como copia editavel")

func _set_mode(value: String) -> void:
	placement_mode = value
	_refresh("Ferramenta: " + value.to_upper())

func _on_map_input(event: InputEvent) -> void:
	if not event is InputEventMouseButton or event.button_index != MOUSE_BUTTON_LEFT or not event.pressed:
		return
	var cell := Vector2i(floori(event.position.x / GameLayout.TILE_SIZE), floori(event.position.y / GameLayout.TILE_SIZE))
	var error := ""
	match placement_mode:
		"portal": error = editor.set_portal(cell)
		"base": error = editor.set_base(cell)
		_: error = editor.cycle_cell(cell)
	_refresh("Mapa atualizado" if error.is_empty() else error)

func _save() -> void:
	var error := AtomicJsonStore.save(editor.snapshot(), SAVE_PATH)
	if error.is_empty():
		error = app_state.select_custom_map(editor.snapshot())
	_refresh("Mapa personalizado guardado" if error.is_empty() else error)

func _load() -> void:
	var document: Variant = AtomicJsonStore.load_with_backup(SAVE_PATH)
	var error: String = editor.restore(document)
	_refresh("Mapa personalizado carregado" if error.is_empty() else error)

func _play_custom() -> void:
	var validation: String = editor.validate()
	if not validation.is_empty():
		_refresh(validation)
		return
	var error: String = app_state.select_custom_map(editor.snapshot())
	if error.is_empty():
		app_state.clear_pending_run()
		get_tree().change_scene_to_file("res://scenes/game/vertical_slice.tscn")
	else:
		_refresh(error)

func _use_official() -> void:
	var error: String = app_state.select_official_maps()
	_refresh("Rotacao oficial selecionada" if error.is_empty() else error)

func _refresh(text: String) -> void:
	map_layer.set_grid(editor.grid)
	message.text = text + "\nPortal " + str(editor.portal) + "   Base " + str(editor.base)
	%Terrain.disabled = placement_mode == "terrain"
	%Portal.disabled = placement_mode == "portal"
	%Base.disabled = placement_mode == "base"
