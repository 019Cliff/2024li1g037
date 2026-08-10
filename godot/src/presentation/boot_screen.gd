extends Control

const DomainIds = preload("res://src/domain/domain_ids.gd")
const ShopChestCardClass = preload("res://src/presentation/shop_chest_card.gd")
const ShopRewardRevealClass = preload("res://src/presentation/shop_reward_reveal.gd")

@onready var app_state: Node = get_node("/root/AppState")
@onready var page_title: Label = %PageTitle
@onready var page_body: RichTextLabel = %PageBody
@onready var account_summary: Label = %AccountSummary
@onready var message: Label = %Message
@onready var actions: Array[Button] = [%Action1, %Action2, %Action3, %Action4, %Action5]
@onready var name_input: LineEdit = %NameInput
@onready var account_select: OptionButton = %AccountSelect
@onready var options_panel: VBoxContainer = %OptionsPanel
@onready var fullscreen_toggle: CheckButton = %FullscreenToggle
@onready var reduced_effects_toggle: CheckButton = %ReducedEffectsToggle
@onready var damage_numbers_toggle: CheckButton = %DamageNumbersToggle
@onready var remember_account_toggle: CheckButton = %RememberAccountToggle
@onready var shop_panel: VBoxContainer = %ShopPanel
@onready var shop_header: Label = %ShopHeader
@onready var shop_hint: Label = %ShopHint
@onready var fusion_status: Label = %FusionStatus
@onready var fusion_button: Button = %FusionButton
@onready var reward_reveal: ShopRewardRevealClass = %RewardReveal
@onready var claim_reward_button: Button = %ClaimReward
@onready var chest_cards: Dictionary[String, ShopChestCardClass] = {
	"wood": %WoodChest,
	"crystal": %CrystalChest,
	"imperial": %ImperialChest
}

var section := "home"
var pending_transfer: Dictionary = {}
var delete_armed := false
var import_as_new_button: Button

func _ready() -> void:
	%Play.pressed.connect(_play)
	%Modes.pressed.connect(func() -> void: _show_section("modes"))
	%Shop.pressed.connect(func() -> void: _show_section("shop"))
	%Profile.pressed.connect(func() -> void: _show_section("profile" if not app_state.active_account.is_empty() else "onboarding"))
	%Ranking.pressed.connect(func() -> void: _show_section("ranking"))
	%Help.pressed.connect(func() -> void: _show_section("help"))
	%Options.pressed.connect(func() -> void: _show_section("options"))
	%Editor.pressed.connect(func() -> void: get_tree().change_scene_to_file("res://scenes/editor/map_editor.tscn"))
	%Exit.pressed.connect(func() -> void: get_tree().quit())
	for index in actions.size():
		actions[index].pressed.connect(func() -> void: _on_action(index))
	%WoodChest.pressed.connect(func() -> void: _purchase_chest("wood", %WoodChest))
	%CrystalChest.pressed.connect(func() -> void: _purchase_chest("crystal", %CrystalChest))
	%ImperialChest.pressed.connect(func() -> void: _purchase_chest("imperial", %ImperialChest))
	fusion_button.pressed.connect(_fuse_shop_tower)
	claim_reward_button.pressed.connect(_claim_shop_reward)
	account_select.item_selected.connect(_select_account)
	%ImportFile.file_selected.connect(_import_transfer_file)
	%ConfirmImport.confirmed.connect(_confirm_transfer_import)
	%ConfirmImport.custom_action.connect(_on_import_custom_action)
	import_as_new_button = %ConfirmImport.add_button("IMPORTAR COMO NOVA", true, "import_as_new")
	%ConfirmNewRun.confirmed.connect(_confirm_new_run)
	%ExportFile.file_selected.connect(_export_active_account)
	fullscreen_toggle.toggled.connect(func(value: bool) -> void: _set_option("fullscreen", value))
	reduced_effects_toggle.toggled.connect(func(value: bool) -> void: _set_option("reduced_effects", value))
	damage_numbers_toggle.toggled.connect(func(value: bool) -> void: _set_option("damage_numbers", value))
	remember_account_toggle.toggled.connect(_set_remember_account)
	_refresh_accounts()
	_apply_window_settings()
	_show_section("onboarding" if app_state.active_account.is_empty() else "home")

func _show_section(next_section: String) -> void:
	section = next_section
	message.text = ""
	delete_armed = false
	name_input.visible = section in ["profile", "onboarding"]
	account_select.visible = section in ["profile", "onboarding"] and account_select.item_count > 0
	options_panel.visible = section == "options"
	shop_panel.visible = section == "shop"
	page_body.visible = section != "shop"
	var compact_content := section in ["options", "shop", "profile", "onboarding"]
	page_body.size_flags_vertical = Control.SIZE_SHRINK_BEGIN if compact_content else Control.SIZE_EXPAND_FILL
	page_body.custom_minimum_size.y = 170.0 if section == "shop" else (150.0 if compact_content else 0.0)
	page_body.remove_theme_font_size_override("normal_font_size")
	if section == "shop":
		page_body.add_theme_font_size_override("normal_font_size", 18)
	for button in actions:
		button.visible = false
		button.disabled = false
		button.custom_minimum_size.y = 54.0
		button.remove_theme_font_size_override("font_size")
	match section:
		"onboarding": _render_onboarding()
		"modes": _render_modes()
		"shop": _render_shop()
		"profile": _render_profile()
		"ranking": _render_ranking()
		"help": _render_help()
		"almanac_towers": _render_almanac_towers()
		"almanac_enemies": _render_almanac_enemies()
		"credits": _render_credits()
		"options": _render_options()
		_: _render_home()
	_refresh_account_summary()

func _render_onboarding() -> void:
	page_title.text = "CONTA LOCAL"
	page_body.text = "O teu progresso fica guardado apenas neste computador.\nNao existe palavra-passe nem sincronizacao cloud.\n\nCria uma conta, escolhe uma conta existente ou importa um backup JSON."
	name_input.placeholder_text = "Nome da nova conta"
	name_input.text = ""
	actions[0].visible = true
	actions[0].text = "CRIAR CONTA LOCAL"
	actions[1].visible = true
	actions[1].text = "IMPORTAR BACKUP"

func _render_home() -> void:
	page_title.text = "DEFENDE. EVOLUI. RESISTE."
	var mode_id := str(app_state.active_account.get("selected_mode", "history"))
	var next_map := "PERSONALIZADO" if bool(app_state.active_account.get("use_custom_map", false)) and app_state.has_custom_map() else _display_id(app_state.selected_map_id)
	page_body.text = "Prepara o arsenal e protege a base.\n\nModo selecionado: " + _display_id(mode_id) + "\nProximo mapa: " + next_map
	actions[0].visible = true
	actions[0].text = "CONTINUAR" if app_state.has_pending_run() else "JOGAR AGORA"
	if app_state.has_pending_run():
		actions[1].visible = true
		actions[1].text = "NOVA PARTIDA"

func _render_modes() -> void:
	page_title.text = "MODOS DE JOGO"
	page_body.text = "Cada modo altera a economia, as vagas e a pressao sobre a base."
	for index in DomainIds.MODES.size():
		var mode_id := DomainIds.MODES[index]
		var minimum := int(app_state.catalog.mode(mode_id).get("minimum_level", 1))
		actions[index].visible = true
		actions[index].disabled = not app_state.mode_unlocked(mode_id)
		actions[index].text = _display_id(mode_id) + ("  [SELECIONADO]" if app_state.active_account.get("selected_mode") == mode_id else ("  NIVEL " + str(minimum) if not app_state.mode_unlocked(mode_id) else ""))

func _render_shop() -> void:
	page_title.text = "LOJA E FUSAO"
	var progress: Dictionary = app_state.active_account.get("meta_progress", {})
	var collection: Array = progress.get("unlocked_tower_ids", [])
	var pending: Variant = app_state.active_account.get("pending_reward")
	var has_tesla := "tesla" in collection
	var has_solar := "solar" in collection
	var has_tempestade := "tempestade" in collection
	var gems := int(progress.get("gems", 0))
	var fusion_ready := has_tesla and has_solar and gems >= 180 and not has_tempestade
	shop_header.text = str(gems) + " GEMAS   |   COLECAO " + str(collection.size()) + "/9"
	shop_hint.text = "Os duplicados devolvem parte do custo em gemas. Recolhe uma recompensa antes de abrir outro bau."
	for chest_id: String in chest_cards:
		var card: ShopChestCardClass = chest_cards[chest_id]
		card.reset_opening()
		card.set_shop_state(gems >= card.cost, pending is Dictionary)
	fusion_status.text = (
		"TEMPESTADE JA CRIADA"
		if has_tempestade
		else "FUSAO TEMPESTADE   "
			+ ("TESLA OK" if has_tesla else "TESLA EM FALTA")
			+ "  +  "
			+ ("SOLAR OK" if has_solar else "SOLAR EM FALTA")
			+ "  +  180 GEMAS"
	)
	fusion_button.disabled = pending is Dictionary or not fusion_ready
	fusion_button.text = "JA DESBLOQUEADA" if has_tempestade else "FUNDIR TEMPESTADE"
	claim_reward_button.visible = pending is Dictionary
	if pending is Dictionary:
		reward_reveal.present(pending, false)
	else:
		reward_reveal.dismiss()

func _render_profile() -> void:
	page_title.text = "PERFIL LOCAL"
	var profile: Dictionary = app_state.active_account.get("profile", {})
	page_body.text = "Jogos: " + str(int(profile.get("games", 0))) + "   Vitorias: " + str(int(profile.get("wins", 0))) + "   Derrotas: " + str(int(profile.get("losses", 0))) + "\nMelhor pontuacao: " + str(int(profile.get("best_score", 0)))
	name_input.text = str(app_state.active_account.get("name", ""))
	actions[0].visible = true
	actions[0].text = "GUARDAR NOME"
	actions[1].visible = true
	actions[1].text = "TROCAR / TERMINAR SESSAO"
	actions[2].visible = true
	actions[2].text = "IMPORTAR BACKUP"
	actions[3].visible = true
	actions[3].text = "EXPORTAR CONTA"
	actions[4].visible = true
	actions[4].text = "ELIMINAR CONTA"

func _render_ranking() -> void:
	page_title.text = "RANKING LOCAL"
	var lines := PackedStringArray()
	var scores: Array = app_state.active_account.get("leaderboard", [])
	for index in scores.size():
		var entry: Dictionary = scores[index]
		lines.append(str(index + 1) + ".  " + str(entry.get("name", "Jogador")) + "   " + str(entry.get("score", 0)) + " pts   " + _display_id(str(entry.get("mode_id", "history"))))
	page_body.text = "Ainda nao existem partidas registadas." if lines.is_empty() else "\n".join(lines)

func _render_help() -> void:
	page_title.text = "COMO JOGAR"
	page_body.text = "O mapa usa uma grelha ampliada: cada celula atual equivale a 3x3 celulas do mapa antigo.\nTorres, estrada, agua, inimigos, portal, base e obstaculos usam todos a mesma escala. Cada torre ocupa uma celula grande.\n\nP pausa   X velocidade   S guarda   L carrega   Esc cancela\nH HUD   K painel   A bot automatico   B sugestao   O obstaculo\n\nAsfalto acelera inimigos. Gelo para, resina abranda e medo faz recuar."
	actions[0].visible = true
	actions[0].text = "ALMANAQUE DE TORRES"
	actions[1].visible = true
	actions[1].text = "ALMANAQUE DE INIMIGOS"
	actions[2].visible = true
	actions[2].text = "CREDITOS"

func _render_almanac_towers() -> void:
	page_title.text = "ALMANAQUE DE TORRES"
	var lines := PackedStringArray()
	for tower_id: String in DomainIds.TOWERS:
		var spec: Dictionary = app_state.catalog.tower(tower_id)
		var base: Dictionary = spec.get("base", {})
		var state := "DESBLOQUEADA" if tower_id in app_state.unlocked_towers() else "BLOQUEADA"
		lines.append(_display_id(tower_id) + "  " + str(spec.get("rarity", "common")).to_upper() + "  " + state + "\n  Dano " + str(base.get("damage")) + "  Alcance " + str(base.get("range")) + "  " + str(spec.get("role", "")))
	page_body.text = "\n\n".join(lines)
	actions[0].visible = true
	actions[0].text = "VOLTAR A AJUDA"

func _render_almanac_enemies() -> void:
	page_title.text = "ALMANAQUE DE INIMIGOS"
	var lines := PackedStringArray()
	for enemy_id: String in DomainIds.ENEMIES:
		var spec: Dictionary = app_state.catalog.enemy(enemy_id)
		lines.append(_display_id(enemy_id) + "\n  Vida " + str(spec.get("health")) + "  Vel " + str(spec.get("speed")) + "  Armadura " + str(spec.get("armor")) + "  Ameaca " + str(spec.get("threat")))
	page_body.text = "\n\n".join(lines)
	actions[0].visible = true
	actions[0].text = "VOLTAR A AJUDA"

func _render_credits() -> void:
	page_title.text = "CREDITOS"
	page_body.text = "Immutable Towers\n\nTower Defense desenvolvido para o projeto de Laboratorios de Informatica I.\n\nDesign, gameplay, programacao e testes: equipa Immutable Towers.\nMotor Godot distribuido sob licenca MIT."
	actions[0].visible = true
	actions[0].text = "VOLTAR A AJUDA"

func _render_options() -> void:
	page_title.text = "OPCOES"
	page_body.text = "VIDEO E ACESSIBILIDADE\n\nResolucao virtual: 1920 x 1080\nRenderer: Compatibility"
	var settings: Dictionary = app_state.settings()
	fullscreen_toggle.set_pressed_no_signal(bool(settings.get("fullscreen", false)))
	reduced_effects_toggle.set_pressed_no_signal(bool(settings.get("reduced_effects", false)))
	damage_numbers_toggle.set_pressed_no_signal(bool(settings.get("damage_numbers", true)))
	remember_account_toggle.set_pressed_no_signal(app_state.remember_account())

func _on_action(index: int) -> void:
	match section:
		"onboarding":
			if index == 0:
				var create_error: String = app_state.create_account_with_generated_id(name_input.text)
				message.text = "Conta local criada" if create_error.is_empty() else create_error
				if create_error.is_empty():
					_refresh_accounts()
					_show_section("home")
			elif index == 1:
				%ImportFile.popup_centered_ratio(0.72)
		"home":
			if index == 0 and app_state.has_pending_run():
				_resume_run()
			else:
				_request_new_run()
		"modes":
			var error: String = app_state.set_selected_mode(DomainIds.MODES[index])
			message.text = "Modo selecionado" if error.is_empty() else error
			_render_modes()
		"shop": _shop_action(index)
		"profile": _profile_action(index)
		"help": _show_section("almanac_towers" if index == 0 else ("almanac_enemies" if index == 1 else "credits"))
		"almanac_towers", "almanac_enemies", "credits": _show_section("help")

func _shop_action(index: int) -> void:
	if index == 4:
		_claim_shop_reward()
	elif index < 3:
		var chest_id: String = ["wood", "crystal", "imperial"][index]
		_purchase_chest(chest_id, chest_cards[chest_id])
	elif index == 3:
		_fuse_shop_tower()

func _purchase_chest(chest_id: String, card: ShopChestCardClass) -> void:
	if card.opening:
		return
	var result: Dictionary = app_state.buy_chest(chest_id)
	var operation_error := str(result.get("error", ""))
	if not operation_error.is_empty():
		message.text = operation_error
		_render_shop()
		return
	for value: ShopChestCardClass in chest_cards.values():
		value.disabled = true
	fusion_button.disabled = true
	claim_reward_button.visible = false
	reward_reveal.dismiss()
	await card.play_opening(bool(app_state.settings().get("reduced_effects", false)))
	if section != "shop":
		return
	var reward: Dictionary = result.get("reward", {})
	var save_error := str(result.get("save_error", ""))
	message.text = "Recebeste " + _display_id(str(reward.get("tower_id", "")))
	if int(reward.get("gems", 0)) > 0:
		message.text += " e " + str(reward.get("gems", 0)) + " gemas"
	if not save_error.is_empty():
		message.text += "   |   AVISO: " + save_error
	_render_shop()
	reward_reveal.present(reward, not bool(app_state.settings().get("reduced_effects", false)))
	_refresh_account_summary()

func _claim_shop_reward() -> void:
	var claim_error: String = app_state.claim_pending_reward()
	message.text = "Recompensa adicionada a colecao" if claim_error.is_empty() else claim_error
	_render_shop()
	_refresh_account_summary()

func _fuse_shop_tower() -> void:
	var result: Dictionary = app_state.fuse_tempestade()
	var error := str(result.get("error", ""))
	var save_error := str(result.get("save_error", ""))
	if not error.is_empty():
		message.text = error
		_render_shop()
		return
	message.text = "Fusao concluida: TEMPESTADE desbloqueada"
	if not save_error.is_empty():
		message.text += "   |   AVISO: " + save_error
	_render_shop()
	reward_reveal.present({"kind": "new_tower", "tower_id": "tempestade", "gems": 0}, not bool(app_state.settings().get("reduced_effects", false)))
	claim_reward_button.visible = false
	_refresh_account_summary()

func _profile_action(index: int) -> void:
	if index == 1:
		var logout_error: String = app_state.logout()
		message.text = "Sessao local terminada" if logout_error.is_empty() else logout_error
		if logout_error.is_empty():
			_refresh_accounts()
			_show_section("onboarding")
		return
	if index == 2:
		%ImportFile.popup_centered_ratio(0.72)
		return
	if index == 3:
		%ExportFile.current_file = str(app_state.active_account.get("account_id", "account")) + "-export.json"
		%ExportFile.popup_centered_ratio(0.72)
		return
	if index == 4:
		if not delete_armed:
			delete_armed = true
			message.text = "Clica ELIMINAR CONTA novamente para confirmar"
			return
		var delete_error: String = app_state.delete_active_account()
		delete_armed = false
		message.text = "Conta eliminada" if delete_error.is_empty() else delete_error
		_refresh_accounts()
		_show_section("onboarding" if app_state.active_account.is_empty() else "profile")
		return
	var error: String = app_state.rename_active_account(name_input.text)
	message.text = "Perfil atualizado" if error.is_empty() else error
	_refresh_accounts()
	_render_profile()
	_refresh_account_summary()

func _export_active_account(path: String) -> void:
	var error: String = app_state.repository.export_account(str(app_state.active_account.get("account_id", "")), path)
	message.text = "Conta exportada" if error.is_empty() else error

func _import_transfer_file(path: String) -> void:
	var file := FileAccess.open(path, FileAccess.READ)
	if file == null:
		message.text = "Nao foi possivel abrir o ficheiro"
		return
	var parsed: Variant = JSON.parse_string(file.get_as_text())
	if not parsed is Dictionary:
		message.text = "O ficheiro nao contem um save exportado valido"
		return
	pending_transfer = parsed
	var result: Dictionary = app_state.repository.import_document(pending_transfer, "cancel")
	if result.get("status") == "confirmation_required":
		%ConfirmImport.ok_button_text = "SUBSTITUIR"
		var native_collision: bool = result.get("import_kind") == "native_account"
		import_as_new_button.visible = native_collision
		%ConfirmImport.dialog_text = (
			"Ja existe uma conta com este identificador.\n\nEscolhe IMPORTAR COMO NOVA para manter ambas, SUBSTITUIR para trocar os dados existentes ou CANCELAR."
			if native_collision
			else "O progresso local parece mais recente. Escolhe SUBSTITUIR apenas se pretendes restaurar os dados Haskell."
		)
		%ConfirmImport.popup_centered()
	else:
		_finish_import(result)

func _confirm_transfer_import() -> void:
	_finish_import(app_state.repository.import_document(pending_transfer, "replace"))

func _on_import_custom_action(action: StringName) -> void:
	if action != &"import_as_new":
		return
	%ConfirmImport.hide()
	_finish_import(app_state.repository.import_document(pending_transfer, "import_as_new"))

func _finish_import(result: Dictionary) -> void:
	match str(result.get("status", "error")):
		"imported":
			_refresh_accounts()
			var imported_id := str(result.get("account_id", ""))
			if not imported_id.is_empty():
				app_state.select_account(imported_id)
				_show_section("profile")
			message.text = str(result.get("imported", 0)) + " conta(s) importada(s)"
		"already_imported": message.text = "Este save ja foi importado"
		"invalid": message.text = ", ".join(result.get("errors", []))
		_: message.text = str(result.get("error", "Falha ao importar"))

func _refresh_accounts() -> void:
	account_select.clear()
	var accounts: Array = app_state.repository.load_index().get("accounts", [])
	for index in accounts.size():
		var summary: Dictionary = accounts[index]
		account_select.add_item(str(summary.get("name", "Jogador")))
		account_select.set_item_metadata(index, summary.get("account_id"))
		if summary.get("account_id") == app_state.active_account.get("account_id"):
			account_select.select(index)
	_set_navigation_enabled(not app_state.active_account.is_empty())

func _select_account(index: int) -> void:
	var error: String = app_state.select_account(str(account_select.get_item_metadata(index)))
	_apply_window_settings()
	_show_section("profile")
	message.text = "Conta selecionada" if error.is_empty() else error

func _set_navigation_enabled(has_account: bool) -> void:
	for button: Button in [%Play, %Modes, %Shop, %Ranking, %Options, %Editor]:
		button.disabled = not has_account

func _set_option(setting_id: String, value: bool) -> void:
	var error: String = app_state.set_setting(setting_id, value)
	message.text = "Opcao guardada" if error.is_empty() else error
	if setting_id == "fullscreen" and error.is_empty():
		DisplayServer.window_set_mode(DisplayServer.WINDOW_MODE_FULLSCREEN if value else DisplayServer.WINDOW_MODE_WINDOWED)

func _set_remember_account(value: bool) -> void:
	var error: String = app_state.set_remember_account(value)
	message.text = "Preferencia guardada" if error.is_empty() else error

func _apply_window_settings() -> void:
	var settings: Dictionary = app_state.settings()
	DisplayServer.window_set_mode(DisplayServer.WINDOW_MODE_FULLSCREEN if bool(settings.get("fullscreen", false)) else DisplayServer.WINDOW_MODE_WINDOWED)

func _animate_reward() -> void:
	if bool(app_state.settings().get("reduced_effects", false)):
		return
	page_title.modulate = Color("f1cf65")
	message.modulate = Color("fff1b8")
	var tween := create_tween().set_parallel(true)
	tween.tween_property(page_title, "modulate", Color.WHITE, 0.55)
	tween.tween_property(message, "modulate", Color.WHITE, 0.55)

func _refresh_account_summary() -> void:
	if app_state.active_account.is_empty():
		account_summary.text = "SEM CONTA ATIVA"
		return
	var progress: Dictionary = app_state.active_account.get("meta_progress", {})
	account_summary.text = str(app_state.active_account.get("name", "Jogador")) + "   NIVEL " + str(int(progress.get("level", 1))) + "   " + str(int(progress.get("gems", 0))) + " GEMAS"

func _play() -> void:
	if app_state.active_account.is_empty():
		_show_section("onboarding")
		return
	if app_state.has_pending_run():
		_resume_run()
	else:
		_request_new_run()

func _resume_run() -> void:
	app_state.resume_pending_run = true
	get_tree().change_scene_to_file("res://scenes/game/vertical_slice.tscn")

func _request_new_run() -> void:
	if app_state.has_pending_run():
		%ConfirmNewRun.popup_centered()
		return
	_start_new_run()

func _confirm_new_run() -> void:
	_start_new_run()

func _start_new_run() -> void:
	app_state.resume_pending_run = false
	app_state.clear_pending_run()
	get_tree().change_scene_to_file("res://scenes/game/vertical_slice.tscn")

func _display_id(value: String) -> String:
	var labels := {
		"history": "HISTORIA",
		"infinite": "INFINITO",
		"challenge": "DESAFIO",
		"boss": "BOSSES",
		"sandbox": "LIVRE"
	}
	return str(labels.get(value, value.to_upper().replace("_", " ")))
