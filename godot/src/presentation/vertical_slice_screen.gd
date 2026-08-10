extends Control

const Simulation = preload("res://src/domain/vertical_slice_simulation.gd")
const RunPersistence = preload("res://src/persistence/run_persistence.gd")
const SaveCoordinator = preload("res://src/persistence/save_coordinator.gd")
const VerticalSliceBot = preload("res://src/bot/vertical_slice_bot.gd")
const StrategicBotPlanner = preload("res://src/bot/strategic_bot_planner.gd")
const BotRuntime = preload("res://src/bot/bot_runtime.gd")
const GameLayout = preload("res://src/presentation/game_layout.gd")
const TowerEconomy = preload("res://src/economy/tower_economy.gd")
const RunRewards = preload("res://src/gameplay/run_rewards.gd")

@onready var map_layer: VerticalSliceMap = %MapLayer
@onready var entity_layer: VerticalSliceView = %EntityLayer
@onready var wave_label: Label = %WaveValue
@onready var health_label: Label = %HealthValue
@onready var credits_label: Label = %CreditsValue
@onready var status_label: Label = %Status
@onready var wave_progress: ProgressBar = %WaveProgress
@onready var next_wave_label: Label = %NextWave
@onready var pause_button: Button = %Pause
@onready var speed_buttons: Array[Button] = [%Speed1, %Speed2, %Speed4]
@onready var map_frame: PanelContainer = %MapFrame
@onready var side_panel: PanelContainer = %SidePanel
@onready var hud_values: HBoxContainer = %HudValues
@onready var autosave_status: Label = %AutosaveStatus
@onready var map_name_label: Label = %MapName
@onready var tower_capacity_label: Label = %TowerCapacity
@onready var selection_label: Label = %Selection
@onready var upgrade_button: Button = %Upgrade
@onready var sell_button: Button = %Sell
@onready var spec_a_button: Button = %SpecA
@onready var spec_b_button: Button = %SpecB
@onready var build_buttons: Dictionary[String, Button] = {
	"sentinela": %BuildSentinela,
	"glaciar": %BuildGlaciar,
	"braseiro": %BuildBraseiro,
	"panico": %BuildPanico,
	"venenoide": %BuildVenenoide,
	"tesla": %BuildTesla,
	"impacto": %BuildImpacto,
	"solar": %BuildSolar,
	"tempestade": %BuildTempestade
}

var simulation := Simulation.new()
var speed := 1.0
var paused := false
var selected_tower_index := -1
var build_tower_id := ""
var obstacle_mode := false
var interaction_message := ""
var bot_cooldown := 0.0
var bot_planner := StrategicBotPlanner.new()
var result_recorded := false
var app_state: Node
var last_checkpoint_wave := -2
var save_coordinator := SaveCoordinator.new()
var autosave_feedback_time := 0.0

func _ready() -> void:
	app_state = get_node_or_null("/root/AppState")
	if app_state != null and not app_state.active_account.is_empty():
		simulation = Simulation.new(app_state.catalog, app_state.selected_map_id, str(app_state.active_account.get("selected_mode", "history")), app_state.towers_for_selected_mode(), app_state.custom_map_for_run(), app_state.run_context_for_selected_mode())
		var settings: Dictionary = app_state.settings()
		entity_layer.reduced_effects = bool(settings.get("reduced_effects", false))
		entity_layer.show_damage_numbers = bool(settings.get("damage_numbers", true))
		if app_state.resume_pending_run:
			var pending: Variant = app_state.pending_run()
			if pending is Dictionary:
				var restore_errors := simulation.restore(pending)
				interaction_message = "Partida retomada" if restore_errors.is_empty() else ", ".join(restore_errors)
			app_state.resume_pending_run = false
	entity_layer.simulation = simulation
	map_layer.set_grid(simulation.map_grid)
	pause_button.pressed.connect(_toggle_pause)
	speed_buttons[0].pressed.connect(func() -> void: _set_speed(1.0))
	speed_buttons[1].pressed.connect(func() -> void: _set_speed(2.0))
	speed_buttons[2].pressed.connect(func() -> void: _set_speed(4.0))
	%BuildSentinela.pressed.connect(func() -> void: _choose_build("sentinela"))
	%BuildGlaciar.pressed.connect(func() -> void: _choose_build("glaciar"))
	%BuildBraseiro.pressed.connect(func() -> void: _choose_build("braseiro"))
	%BuildPanico.pressed.connect(func() -> void: _choose_build("panico"))
	%BuildVenenoide.pressed.connect(func() -> void: _choose_build("venenoide"))
	%BuildTesla.pressed.connect(func() -> void: _choose_build("tesla"))
	%BuildImpacto.pressed.connect(func() -> void: _choose_build("impacto"))
	%BuildSolar.pressed.connect(func() -> void: _choose_build("solar"))
	%BuildTempestade.pressed.connect(func() -> void: _choose_build("tempestade"))
	upgrade_button.pressed.connect(_upgrade_selected)
	sell_button.pressed.connect(_sell_selected)
	%Cancel.pressed.connect(_cancel_action)
	%Obstacle.pressed.connect(_choose_obstacle)
	spec_a_button.pressed.connect(func() -> void: _upgrade_selected_with("a"))
	spec_b_button.pressed.connect(func() -> void: _upgrade_selected_with("b"))
	map_frame.gui_input.connect(_on_map_input)
	%Save.pressed.connect(_save_run)
	%Load.pressed.connect(_load_run)
	%Suggest.pressed.connect(_suggest_bot_action)
	%ToggleHud.pressed.connect(_toggle_hud)
	%TogglePanel.pressed.connect(_toggle_panel)
	%Back.pressed.connect(_return_to_menu)
	%OverlayResume.pressed.connect(_toggle_pause)
	%OverlayRestart.pressed.connect(_restart_run)
	%OverlayMenu.pressed.connect(_return_to_menu)
	_configure_shop_buttons()
	_refresh_hud()
	_refresh_selection()
	%Back.text = "VOLTAR AO MENU"

func _physics_process(delta: float) -> void:
	if autosave_feedback_time > 0.0:
		autosave_feedback_time -= delta
		if autosave_feedback_time <= 0.0:
			autosave_status.text = ""
	if not paused:
		simulation.update(delta * speed)
		save_coordinator.mark_dirty()
		_update_bot(delta)
		if simulation.wave_index != last_checkpoint_wave and not simulation.finished:
			last_checkpoint_wave = simulation.wave_index
			_request_checkpoint(true)
		else:
			var was_dirty := save_coordinator.dirty
			var autosave_error := save_coordinator.tick(delta, _checkpoint_run)
			if not autosave_error.is_empty():
				interaction_message = autosave_error
			elif was_dirty and not save_coordinator.dirty:
				_show_autosave_feedback()
	if simulation.finished and not result_recorded:
		result_recorded = true
		save_coordinator.clear()
		if app_state != null:
			var result_error: String = app_state.complete_run(simulation)
			if not result_error.is_empty():
				interaction_message = result_error
		%OverlayTitle.text = "VITORIA" if simulation.victory else "DERROTA"
		var reached_wave := RunRewards.wave_reached(simulation)
		var result_detail := "Vaga alcancada: " + str(reached_wave) + "   Creditos: " + str(simulation.credits)
		if simulation.mode_id == "infinite":
			var gem_reward := int(app_state.last_run_gem_reward) if app_state != null else RunRewards.infinite_gems(reached_wave)
			result_detail += "   Gemas: +" + str(gem_reward)
		%OverlayDetail.text = result_detail
		%OverlayResume.visible = false
		%GameOverlay.visible = true
	entity_layer.queue_redraw()
	_refresh_hud()
	_refresh_selection()

func _unhandled_input(event: InputEvent) -> void:
	if event.is_action_pressed("pause"):
		_toggle_pause()
	elif event.is_action_pressed("speed_cycle"):
		_set_speed(2.0 if speed == 1.0 else (4.0 if speed == 2.0 else 1.0))
	elif event.is_action_pressed("cancel"):
		if simulation.finished:
			_return_to_menu()
		elif paused:
			_toggle_pause()
		else:
			_cancel_action()
	elif event.is_action_pressed("place_obstacle"):
		_choose_obstacle()
	elif event.is_action_pressed("toggle_hud"):
		_toggle_hud()
	elif event.is_action_pressed("toggle_shop"):
		_toggle_panel()
	elif event.is_action_pressed("toggle_bot"):
		%AutoBot.button_pressed = not %AutoBot.button_pressed
		interaction_message = "Bot automatico ativo" if %AutoBot.button_pressed else "Bot automatico parado"
	elif event.is_action_pressed("suggest_bot"):
		_suggest_bot_action()
	elif event.is_action_pressed("save_run"):
		_save_run()
	elif event.is_action_pressed("load_run"):
		_load_run()

func _toggle_pause() -> void:
	if simulation.finished:
		return
	paused = not paused
	pause_button.text = "CONTINUAR" if paused else "PAUSA"
	%OverlayTitle.text = "JOGO EM PAUSA"
	%OverlayDetail.text = "A simulacao esta parada."
	%OverlayResume.visible = true
	%GameOverlay.visible = paused
	_request_checkpoint(true)

func _restart_run() -> void:
	var current_catalog: Variant = simulation.catalog
	var current_map := simulation.map_id
	var current_mode := simulation.mode_id
	var unlocked := simulation.available_tower_ids
	simulation = Simulation.new(current_catalog, current_map, current_mode, unlocked, simulation.custom_map_document)
	entity_layer.simulation = simulation
	map_layer.set_grid(simulation.map_grid)
	selected_tower_index = -1
	build_tower_id = ""
	obstacle_mode = false
	interaction_message = "Partida reiniciada"
	paused = false
	result_recorded = false
	last_checkpoint_wave = -2
	%GameOverlay.visible = false
	_request_checkpoint(true)
	_sync_view_state()

func _return_to_menu() -> void:
	if not simulation.finished:
		_request_checkpoint(true)
	get_tree().change_scene_to_file("res://scenes/boot/boot.tscn")

func _set_speed(value: float) -> void:
	speed = value
	for index in speed_buttons.size():
		speed_buttons[index].disabled = speed_buttons[index].text == str(int(value)) + "x"

func _toggle_hud() -> void:
	hud_values.visible = not hud_values.visible
	%ToggleHud.text = "MOSTRAR HUD" if not hud_values.visible else "HUD"

func _toggle_panel() -> void:
	side_panel.visible = not side_panel.visible
	%TogglePanel.text = "MOSTRAR PAINEL" if not side_panel.visible else "PAINEL"

func _choose_build(tower_id: String) -> void:
	build_tower_id = tower_id
	obstacle_mode = false
	selected_tower_index = -1
	interaction_message = "Escolhe uma celula grande de relva livre"
	_sync_view_state()

func _choose_obstacle() -> void:
	build_tower_id = ""
	obstacle_mode = true
	selected_tower_index = -1
	interaction_message = "Escolhe uma celula da estrada"
	_sync_view_state()

func _cancel_action() -> void:
	build_tower_id = ""
	obstacle_mode = false
	selected_tower_index = -1
	interaction_message = "Acao cancelada"
	_sync_view_state()

func _on_map_input(event: InputEvent) -> void:
	if event is InputEventMouseMotion:
		entity_layer.hovered_cell = Vector2i(floori(event.position.x / GameLayout.TILE_SIZE), floori(event.position.y / GameLayout.TILE_SIZE))
		entity_layer.queue_redraw()
		return
	if not event is InputEventMouseButton or event.button_index != MOUSE_BUTTON_LEFT or not event.pressed:
		return
	var cell := Vector2i(floori(event.position.x / GameLayout.TILE_SIZE), floori(event.position.y / GameLayout.TILE_SIZE))
	if obstacle_mode:
		var error := simulation.place_obstacle(cell)
		if error.is_empty():
			interaction_message = "Obstaculo colocado: inimigos abrandam nesta celula"
			obstacle_mode = false
			_request_checkpoint(true)
		else:
			interaction_message = error
	elif not build_tower_id.is_empty():
		var error := simulation.build_tower(build_tower_id, cell)
		if error.is_empty():
			selected_tower_index = simulation.towers.size() - 1
			interaction_message = "Torre construida"
			build_tower_id = ""
			_request_checkpoint(true)
		else:
			interaction_message = error
	else:
		selected_tower_index = simulation.tower_at(cell)
		interaction_message = "" if selected_tower_index >= 0 else "Nenhuma torre nesta celula"
	_sync_view_state()

func _upgrade_selected() -> void:
	if selected_tower_index < 0 or selected_tower_index >= simulation.towers.size():
		return
	var tower := simulation.towers[selected_tower_index]
	if tower.level >= 3 and tower.specialization.is_empty():
		interaction_message = "Escolhe POTENCIA ou CADENCIA"
		_refresh_selection()
		return
	_upgrade_selected_with("")

func _upgrade_selected_with(specialization: String) -> void:
	var error := simulation.upgrade_tower(selected_tower_index, specialization)
	interaction_message = "Upgrade concluido" if error.is_empty() else error
	if error.is_empty():
		_request_checkpoint(true)
	_refresh_selection()

func _sell_selected() -> void:
	var value := simulation.sell_tower(selected_tower_index)
	if value > 0:
		interaction_message = "Torre vendida por " + str(value)
		selected_tower_index = -1
		_request_checkpoint(true)
	else:
		interaction_message = "Venda indisponivel"
	_sync_view_state()

func _save_run() -> void:
	var error := _request_checkpoint(true)
	interaction_message = "Partida guardada" if error.is_empty() else error
	_refresh_selection()

func _load_run() -> void:
	var snapshot: Variant = app_state.pending_run() if app_state != null else RunPersistence.load_snapshot()
	if not snapshot is Dictionary:
		interaction_message = "Nao existe partida guardada"
		_refresh_selection()
		return
	var errors: PackedStringArray = simulation.restore(snapshot)
	interaction_message = "Partida carregada" if errors.is_empty() else ", ".join(errors)
	selected_tower_index = -1
	build_tower_id = ""
	obstacle_mode = false
	save_coordinator.clear()
	_sync_view_state()

func _request_checkpoint(immediate: bool = false) -> String:
	save_coordinator.mark_dirty()
	if not immediate:
		return ""
	var error := save_coordinator.flush(_checkpoint_run)
	if error.is_empty():
		_show_autosave_feedback()
	return error

func _show_autosave_feedback() -> void:
	autosave_status.text = "GUARDADO"
	autosave_feedback_time = 1.8

func _checkpoint_run() -> String:
	if app_state != null and not app_state.active_account.is_empty():
		return app_state.save_pending_run(simulation.snapshot())
	return RunPersistence.save_snapshot(simulation.snapshot())

func _suggest_bot_action() -> void:
	var decision: Dictionary = bot_planner.decide(simulation)
	interaction_message = _bot_decision_summary(decision)
	_refresh_selection()

func _update_bot(delta: float) -> void:
	if not %AutoBot.button_pressed or simulation.finished:
		return
	bot_cooldown -= delta
	if bot_cooldown > 0.0:
		return
	var decision: Dictionary = bot_planner.decide(simulation)
	save_coordinator.mark_dirty()
	var outcome := BotRuntime.execute(simulation, decision, func() -> String: return save_coordinator.flush(_checkpoint_run))
	bot_planner.record_outcome(simulation, decision, outcome)
	var result := str(outcome.get("error", ""))
	interaction_message = _bot_decision_summary(decision) if result.is_empty() else result
	if not bool(outcome.get("mutated", false)):
		save_coordinator.clear()
	bot_cooldown = 1.0

func _bot_decision_summary(decision: Dictionary) -> String:
	var summary := str(decision.get("reason", "Sem sugestao"))
	var breakdown: Dictionary = decision.get("breakdown", {})
	if not breakdown.is_empty():
		var components: Array[Dictionary] = []
		for key: String in breakdown:
			components.append({"name": key, "value": float(breakdown.get(key, 0.0))})
		components.sort_custom(func(left: Dictionary, right: Dictionary) -> bool: return absf(float(left.value)) > absf(float(right.value)))
		var labels := PackedStringArray()
		for index in mini(3, components.size()):
			labels.append(str(components[index].name).to_upper() + " " + str(snappedf(float(components[index].value), 0.01)))
		if not labels.is_empty():
			summary += "\n" + " | ".join(labels)
	var alternatives: Array = decision.get("alternatives", [])
	if not alternatives.is_empty():
		summary += "\nAlternativa: " + str(alternatives[0].get("label", alternatives[0].get("kind", "")))
	return summary

func _notification(what: int) -> void:
	if what in [NOTIFICATION_APPLICATION_FOCUS_OUT, NOTIFICATION_WM_CLOSE_REQUEST]:
		if simulation != null and not simulation.finished:
			_request_checkpoint(true)

func _sync_view_state() -> void:
	entity_layer.selected_tower_index = selected_tower_index
	entity_layer.build_tower_id = build_tower_id
	entity_layer.obstacle_mode = obstacle_mode
	entity_layer.queue_redraw()
	_refresh_selection()

func _refresh_selection() -> void:
	if selected_tower_index < 0 or selected_tower_index >= simulation.towers.size():
		upgrade_button.disabled = true
		sell_button.disabled = true
		spec_a_button.visible = false
		spec_b_button.visible = false
		selection_label.text = interaction_message if not interaction_message.is_empty() else "Seleciona uma torre ou escolhe uma construcao."
		return
	var tower := simulation.towers[selected_tower_index]
	var needs_specialization := tower.level >= 3 and tower.specialization.is_empty() and tower.level < tower.max_level
	var cost := simulation.upgrade_cost(selected_tower_index)
	upgrade_button.disabled = needs_specialization or cost < 0 or simulation.credits < cost
	sell_button.disabled = false
	spec_a_button.visible = needs_specialization
	spec_b_button.visible = needs_specialization
	spec_a_button.disabled = simulation.credits < simulation.upgrade_cost(selected_tower_index, "a")
	spec_b_button.disabled = simulation.credits < simulation.upgrade_cost(selected_tower_index, "b")
	var upgrade_text := "MAX" if tower.level >= tower.max_level else ("ESPECIALIZACAO" if needs_specialization else str(cost) + " creditos")
	selection_label.text = tower.tower_id.to_upper() + "  NIVEL " + str(tower.level) + "/" + str(tower.max_level) + "\nDano " + str(snappedf(tower.damage, 0.1)) + "  Alcance " + str(snappedf(tower.range_cells, 0.1)) + "\nProximo: " + upgrade_text
	selection_label.text += "\nInvestido " + str(tower.investment_paid) + "  Venda " + str(TowerEconomy.sale_value(tower))
	if not needs_specialization:
		var preview := simulation.upgrade_preview(selected_tower_index)
		if not preview.is_empty():
			selection_label.text += "\nDano " + str(snappedf(tower.damage, 0.1)) + " -> " + str(snappedf(float(preview.get("damage")), 0.1))
			selection_label.text += "   Alcance " + str(snappedf(tower.range_cells, 0.1)) + " -> " + str(snappedf(float(preview.get("range")), 0.1))
	else:
		spec_a_button.tooltip_text = _preview_tooltip(simulation.upgrade_preview(selected_tower_index, "a"))
		spec_b_button.tooltip_text = _preview_tooltip(simulation.upgrade_preview(selected_tower_index, "b"))
	if not interaction_message.is_empty():
		selection_label.text += "\n" + interaction_message

func _refresh_hud() -> void:
	var total := "INF" if simulation.mode_id == "infinite" else str(simulation.wave_definitions.size())
	wave_label.text = str(maxi(0, simulation.wave_index + 1)) + "/" + total
	health_label.text = str(int(ceil(simulation.base_health)))
	credits_label.text = str(simulation.credits)
	map_name_label.text = ("MAPA PERSONALIZADO" if simulation.map_id == "custom" else simulation.map_id.to_upper().replace("_", " "))
	tower_capacity_label.text = "TORRES  " + str(simulation.towers.size()) + " / " + str(simulation.tower_limit)
	var capacity_ratio := float(simulation.towers.size()) / float(maxi(1, simulation.tower_limit))
	tower_capacity_label.add_theme_color_override("font_color", Color("ef7468") if capacity_ratio >= 1.0 else (Color("e9c45c") if capacity_ratio >= 0.8 else Color("9dc58d")))
	var remaining := simulation.enemies.size() + simulation.pending_enemies.size()
	wave_progress.value = 0.0 if simulation.current_wave_total <= 0 else 100.0 * float(simulation.current_wave_total - remaining) / float(simulation.current_wave_total)
	next_wave_label.text = _next_wave_summary()
	if simulation.finished:
		status_label.text = "VITORIA" if simulation.victory else "DERROTA"
	elif simulation.wave_index < 0:
		status_label.text = _campaign_label() + "A PREPARAR PRIMEIRA VAGA"
	else:
		status_label.text = _campaign_label() + simulation.map_id.to_upper().replace("_", " ") + "  |  " + str(simulation.enemies.size() + simulation.pending_enemies.size()) + " INIMIGOS"
	for tower_id: String in build_buttons:
		var price := simulation.tower_price(tower_id)
		build_buttons[tower_id].visible = tower_id in simulation.available_tower_ids
		build_buttons[tower_id].disabled = simulation.credits < price or not simulation.has_tower_capacity()

func _campaign_label() -> String:
	if simulation.mode_id != "history" or not bool(simulation.difficulty_profile.get("enabled", false)):
		return ""
	return "CAP " + str(simulation.difficulty_profile.get("chapter", 1)) + "  EST " + str(simulation.difficulty_profile.get("stage", 1)) + "  |  "

func _configure_shop_buttons() -> void:
	for tower_id: String in build_buttons:
		var spec: Dictionary = simulation.catalog.tower(tower_id)
		build_buttons[tower_id].text = str(spec.get("name", tower_id.to_upper())) + "  " + str(simulation.tower_price(tower_id))
		var base: Dictionary = spec.get("base", {})
		build_buttons[tower_id].tooltip_text = str(spec.get("rarity", "")) + " / " + str(spec.get("role", "")) + "\n" + str(spec.get("description", "")) + "\nDano " + str(base.get("damage", 0)) + "  Alcance " + str(base.get("range", 0)) + "  Ciclo " + str(base.get("cycle", 0))

func _next_wave_summary() -> String:
	var next_index := 0 if simulation.wave_index < 0 else simulation.wave_index + 1
	if next_index < 0 or next_index >= simulation.wave_definitions.size():
		return "ULTIMA VAGA" if simulation.mode_id != "infinite" else "PROXIMA VAGA: AMEACA CRESCENTE"
	var counts := {}
	for instance: Dictionary in simulation.wave_definitions[next_index].get("enemies", []):
		var class_id := str(instance.get("class_id", "basico"))
		counts[class_id] = int(counts.get(class_id, 0)) + 1
	var parts := PackedStringArray()
	for class_id: String in counts:
		parts.append(str(counts[class_id]) + " " + str(simulation.catalog.enemy(class_id).get("name", class_id)))
	return "PROXIMA: " + (", ".join(parts) if not parts.is_empty() else "SEM DADOS")

func _preview_tooltip(preview: Dictionary) -> String:
	if preview.is_empty():
		return "Upgrade indisponivel"
	return str(preview.get("cost", 0)) + " creditos\nDano " + str(snappedf(float(preview.get("damage", 0.0)), 0.1)) + "  Alcance " + str(snappedf(float(preview.get("range", 0.0)), 0.1)) + "\nRajada " + str(preview.get("burst", 1)) + "  Ciclo " + str(snappedf(float(preview.get("cycle", 1.0)), 0.01))
